'use strict';
import { default as hash } from 'hash-sum';
import * as cmn from './common.mjs';
import * as bl from './biolink-model.mjs';

// Set by configuration. They allow us to assign endpoints to a graph for path generation.
// Both the subject and object keys could be the start or end of a path, but will be consistent
// for a single query.
let SUBJECT_KEY = null;
let OBJECT_KEY = null;

export const CONSTANTS = {
  QUALIFIERS: {
    CONSTRAINTS: 'qualifier_constraints',
    SET: 'qualifier_set',
    ID: 'qualifier_type_id',
    VALUE: 'qualifier_value'
  },
  QGRAPH: {
    INFERRED: 'inferred',
    TEMPLATE: {
      CHEMICAL_GENE: 0,
      CHEMICAL_DISEASE: 1,
      GENE_CHEMICAL: 2
    },
    NODES: 'nodes',
    EDGES: 'edges',
  }
}

class QNode {
  constructor(binding, category, curies) {
    if (!category) throw new TypeError(`Expected category to be a string, got: ${category}`);
    this.categories = [bl.tagBiolink(category)];
    if (curies && !cmn.isArrayEmpty(curies)) {
      this.ids = curies;
    }

    this.binding = binding;
  }

  static fromTrapi(binding, trapiQNode) {
    const category = cmn.jsonGet(trapiQNode, 'categories')[0];
    const curies = cmn.jsonGet(trapiQNode, 'ids', []);
    return new QNode(binding, category, curies);
  }
}

class QEdge {
  constructor(subject, object, predicate, /* TODO: edgeMode, */ constraints) {
    this.subject = subject.binding;
    this.object = object.binding;
    this.predicates = [bl.tagBiolink(predicate)];
    // We only support creative queries right now. In the future we may want to support lookup queries.
    this.queryMode = CONSTANTS.QGRAPH.INFERRED;
    if (constraints && !cmn.isArrayEmpty(constraints)) {
      this.constraints = constraints;
    }
  }

  static fromTrapi(trapiQEdge) {
    const subject = cmn.jsonGet(trapiQEdge, 'subject');
    const object = cmn.jsonGet(trapiQEdge, 'object');
    const predicate = cmn.jsonGet(trapiQEdge, 'predicates')[0];
    const constraints = cmn.jsonGet(trapiQEdge, 'constraints', []).map(constraint => {
      if (CONSTANTS.QUALIFIERS.SET in constraint) {
        return QEdgeQualifierSet.fromTrapi(constraint[CONSTANTS.QUALIFIERS.SET]);
      } else {
        throw new TypeError(`Unsupported constraint type found while building QEdge: ${constraint}`);
      }
    });

    return new QEdge(subject, object, predicate, constraints);
  }

  // Knowledge type is used everywhere in Translator and can mean different things depending on context. In the context of
  // a query edge it can have two values:
  //   1. 'lookup' - The edge will be directly queried from a knowledge graph.
  //   2. 'inferred' - The reasoners are allowed to infer an edge based on combinations of other edges.
  // Right now this essentially chooses between two different "query modes": lookup and creative. In the future a query could
  // have a mix of types.
  get queryMode() { return this.knowledge_type; }
  set queryMode(qm) { this.knowledge_type = qm; }

  get constraints() { return this[CONSTANTS.QUALIFIERS.CONSTRAINTS]; }
  set constraints(constraints) { this[CONSTANTS.QUALIFIERS.CONSTRAINTS] = constraints; }

  // TODO: The TRAPI spec seems to allow for more than one qualifier set. What does that mean?
  get qualifiers() {
    return this.constraints.map(constraint => {
      if (CONSTANTS.QUALIFIERS.SET in constraint) {
        return constraint;
      }
    });
  }

  get binding() {
    return hash([this.subject, this.object, this.predicates[0]]);
  }
}

class QEdgeQualifierSet {
  constructor(qualifiers = []) {
    this[CONSTANTS.QUALIFIERS.SET] = qualifiers;
  }

  static fromTrapi(trapiQEdgeQualifiers) {
    // Only support qualifier constraints right now. Not even sure there are other constraint types.
    const qualifiers = trapiQEdgeQualifiers.map(qualifier => {
      return QEdgeQualifier.fromTrapi(qualifier);
    });

    return new QEdgeQualifierSet(qualifiers);
  }

  // Lets just say its a multiset
  add(qualifier) {
    this[CONSTANTS.QUALIFIERS.SET].push(qualifier);
  }
}

class QEdgeQualifier {
  constructor(type, value) {
    this[CONSTANTS.QUALIFIERS.ID] = type;
    this[CONSTANTS.QUALIFIERS.VALUE] = value;
  }

  static fromTrapi(trapiQEdgeQualifier) {
    const type = cmn.jsonGet(trapiQEdgeQualifier, CONSTANTS.QUALIFIERS.ID);
    const value = cmn.jsonGet(trapiQEdgeQualifier, CONSTANTS.QUALIFIERS.VALUE);
    return new QEdgeQualifier(type, value);
  }
}

function makeQEdgeAspectQualifier(aspect) {
  return new QEdgeQualifier('biolink:object_aspect_qualifier', aspect);
}

function makeQEdgeDirectionQualifier(direction) {
  return new QEdgeQualifier('biolink:object_direction_qualifier', direction);
}

function makeQEdgePredicateQualifier(predicate) {
  return new QEdgeQualifier('biolink:qualified_predicate', predicate);
}

class QGraph {
  constructor(qNodes = {}, qEdges = {}) {
    this.qNodes = qNodes;
    this.qEdges = qEdges;
  }

  static fromTrapi(trapiQGraph) {
    const qg = new QGraph();
    const nodes = cmn.jsonGet(trapiQGraph, 'nodes');
    for (const [binding, node] of Object.entries(nodes)) {
      qg.qNodes[binding] = QNode.fromTrapi(binding, node);
    }

    const edges = cmn.jsonGet(trapiQGraph, 'edges');
    for (const [binding, edge] of Object.entries(edges)) {
      qg.qEdges[binding] = QEdge.fromTrapi(edge);
    }

    return qg;
  }

  get qNodes() { return this[CONSTANTS.QGRAPH.NODES]; }
  set qNodes(nodes) { this[CONSTANTS.QGRAPH.NODES] = nodes; }
  get qEdges() { return this[CONSTANTS.QGRAPH.EDGES]; }
  set qEdges(edges) { this[CONSTANTS.QGRAPH.EDGES] = edges; }

  get template() {
    let [start, end] = [SUBJECT_KEY, OBJECT_KEY];
    const startIsObject = this.nodes[SUBJECT_KEY].ids
    if (startIsObject) {
      [start, end] = [OBJECT_KEY, SUBJECT_KEY];
    }

    const startCategory = this.nodes[start].categories[0];
    const endCategory = this.nodes[end].categories[0];
    if (startCategory === bl.tagBiolink('ChemicalEntity') &&
        endCategory === bl.tagBiolink('Gene')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
    }
    else if (startCategory === bl.tagBiolink('ChemicalEntity') &&
             endCategory === bl.tagBiolink('Disease')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
    }
    else if (startCategory === bl.tagBiolink('Gene') &&
             endCategory === bl.tagBiolink('ChemicalEntity')) {
      return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
    }

    throw new RangeError(`Unsupported query graph template: ${startCategory} -> ${endCategory}`);
  }
}

class Query {
  constructor(clientReq) {
    if (!cmn.isObj(clientReq)) {
      throw new TypeError(`Expected clientReq to be type object, got: ${clientReq}`);
    }

    this.template = queryTypeToTemplate(cmn.jsonGet(clientReq, 'type'));
    this.curie = cmn.jsonGet(clientReq, 'curie');
    this.direction = cmn.jsonGet(clientReq, 'direction');
  }
}

/*
 * Load the TRAPI configuration. Must be called before using the module.
 */
export function loadTrapi(trapiConfig) {
  SUBJECT_KEY = trapiConfig.query_subject_key
  OBJECT_KEY = trapiConfig.query_object_key
}

/*
 * Convert a request from the client to a TRAPI query for the ARS.
 *
 * @param {object} query - The query to convert.
 * @returns {object} - A TRAPI compliant message wth a query graph.
 */
export function clientReqToTrapiQuery(clientReq) {
  function makeQueryTemplateGraph(subjectNode, objectNode, qEdge) {
    const qNodes = {};
    qNodes[subjectNode.binding] = subjectNode;
    qNodes[objectNode.binding] = objectNode;

    const qEdges = {};
    qEdges[qEdge.binding] = qEdge;
    return new QGraph(qNodes, qEdges);
  }

  function makeGeneChemicalConstraints(direction) {
    const qualifierSet = new QEdgeQualifierSet();
    qualifierSet.add(makeQEdgePredicateQualifier(bl.tagBiolink('causes')));
    qualifierSet.add(makeQEdgeAspectQualifier('activity_or_abundance'));
    qualifierSet.add(makeQEdgeDirectionQualifier(direction));
    return [qualifierSet];
  }

  function diseaseToTrapiQgraph(diseaseCurie) {
    const subjectNode = new QNode(SUBJECT_KEY, 'ChemicalEntity');
    const objectNode = new QNode(OBJECT_KEY, 'Disease', [diseaseCurie]);
    return makeQueryTemplateGraph(
      subjectNode,
      objectNode,
      new QEdge(subjectNode, objectNode, bl.tagBiolink('treats')));
  }

  function geneToTrapiQgraph(geneCurie, direction) {
    const subjectNode = new QNode(SUBJECT_KEY, 'ChemicalEntity');
    const objectNode = new QNode(OBJECT_KEY, 'Gene', [geneCurie]);
    return makeQueryTemplateGraph(
      subjectNode,
      objectNode,
      new QEdge(subjectNode, objectNode, bl.tagBiolink('affects'), makeGeneChemicalConstraints(direction)));
  }

  function chemicalToTrapiQgraph(chemicalCurie, direction) {
    const subjectNode = new QNode(SUBJECT_KEY, 'ChemicalEntity', [chemicalCurie]);
    const objectNode = new QNode(OBJECT_KEY, 'Gene');
    return makeQueryTemplateGraph(
      subjectNode,
      objectNode,
      new QEdge(subjectNode, objectNode, bl.tagBiolink('affects'), makeGeneChemicalConstraints(direction)));
  }

  const query = new Query(clientReq);
  if (isChemicalDiseaseQuery(query.template)) {
    return makeTrapiMessage(diseaseToTrapiQgraph(query.curie));
  } else if (isGeneChemicalQuery(query.template)) {
    return makeTrapiMessage(chemicalToTrapiQgraph(query.curie, query.direction));
  } else if (isChemicalGeneQuery(query.template)) {
    return makeTrapiMessage(geneToTrapiQgraph(query.curie, query.direction));
  }

  throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
}

export function nodeIdsToTrapiMessage(nodeIds) {
  return makeTrapiMessage(false, makeKnowledgeGraphFromNodeIds(nodeIds));
}

export function getQueryGraph(trapiMessage) {
  return cmn.jsonGetFromKpath(trapiMessage, ['message', 'query_graph']);
}

/*
 * Determine the query template type based on the TRAPI message.
 *
 * @param {object} message - The message to determine the query template from.
 *
 * @returns {number} - The query template. See CONSTANTS.QGRAPH.TEMPLATE.
 */
export function messageToQueryTemplate(message) {
  const qg = QGraph.fromTrapi(getQueryGraph(message));
  return qg.template;
}

/*
 * Determine which keys correspond to the start and end points of all graphs.
 *
 * @param {object} message - The message to determine the start and end keys from.
 *
 * @returns {string[]} - The keys corresponding to the start and end points.
 */
export function messageToEndpoints(message) {
  const qg = getQueryGraph(message);
  const startIsObject = cmn.jsonGetFromKpath(qg, ['nodes', SUBJECT_KEY, 'ids'], false);
  if (startIsObject) {
    return [OBJECT_KEY, SUBJECT_KEY];
  }

  return [SUBJECT_KEY, OBJECT_KEY];
}

export function isChemicalGeneQuery(queryType) {
  return queryType === CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
}

export function isChemicalDiseaseQuery(queryType) {
  return queryType === CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
}

export function isGeneChemicalQuery(queryType) {
  return queryType === CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
}

export function isValidQuery(queryType) {
  return Object.values(CONSTANTS.QGRAPH.TEMPLATE).includes(queryType);
}

/*
 * Create a minimal TRAPI message from a collection of node CURIEs
 *
 * @param {Array} nodeIds - Array of CURIEs
 * @returns {Object} - TRAPI message
 */
function makeKnowledgeGraphFromNodeIds(nodeIds) {
  const nodes = {};
  nodeIds.forEach(id => {
    if (bl.isValidCurie(id)) {
      nodes[id] = {};
    }
  });

  return {
    edges: {},
    nodes: nodes
  };
}

/*
 * Create a TRAPI message from a query graph and knowledge graph.
 *
 * @param {object} queryGraph? - Optional. The query graph to include in the message.
 * @param {object} knowledgeGraph? - Optional. The knowledge graph to include in the message.
 *
 * @returns {object} - A TRAPI compliant message.
 */
function makeTrapiMessage(queryGraph, knowledgeGraph) {
  const message = {};
  if (queryGraph) {
    message['query_graph'] = queryGraph;
  }

  if (knowledgeGraph) {
    message['knowledge_graph'] = knowledgeGraph;
  }

  return { 'message': message };
}

/* Convert a query type sent from the client to a query template.
 *
 * @param {string} queryType - The query type from the client.
 *
 * @returns {number} - The query template. See CONSTANTS.QGRAPH.TEMPLATE.
 */
function queryTypeToTemplate(queryType) {
  switch (queryType) {
    case 'drug':
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
    case 'gene':
      return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
    case 'chemical':
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
    default:
      throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
  }
}
