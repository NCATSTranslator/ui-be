'use strict'

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

export function loadTrapi(trapiConfig) {
  SUBJECT_KEY = trapiConfig.query_subject_key
  OBJECT_KEY = trapiConfig.query_object_key
}

class QNode {
  constructor(category, ids) {
    if (!category) throw new TypeError(`Expected category to be a string, got: ${category}`);
    this.categories = [bl.tagBiolink(category)];
    if (ids && !cmn.isArrayEmpty(ids)) {
      this.ids = ids;
    }
  }
}

class QEdge {
  constructor(subjectBinding, objectBinding, predicate, /* TODO: edgeMode, */ constraints) {
    this.subject = subjectBinding;
    this.object = objectBinding;
    this.predicates = [bl.tagBiolink(predicate)];
    // We only support creative queries right now. In the future we may want to support lookup queries.
    this.queryMode = CONSTANTS.QGRAPH.INFERRED;
    if (constraints && !cmn.isArrayEmpty(constraints)) {
      this.constraints = constraints;
    }
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
}

class QEdgeQualifierSet {
  constructor(qualifiers = []) {
    this[CONSTANTS.QUALIFIERS.SET] = qualifiers;
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
}

function makeQEdgeAspectQualifier(aspect) {
  return new QEdgeQualifier('biolink:object_aspect_qualifier', aspect);
}

function makeQEdgeDirectionQualifier(direction) {
  return new QEdgeQualifier('biolink:object_direction_qualifier', direction);
}

class QGraph {
  constructor(qNodes, qEdges) {
    this[CONSTANTS.QGRAPH.NODES] = qNodes;
    this[CONSTANTS.QGRAPH.EDGES] = qEdges;
  }
}

/*
 * Convert a request from the client to a TRAPI query for the ARS.
 *
 * @param {object} query - The query to convert.
 * @returns {object} - A TRAPI compliant message wth a query graph.
 */
export function queryToTrapiQuery(query) {
  function buildTrapiQueryGraph(subject, object, predicate, constraints = []) {
    function nodeToQNode(node) {
      const nodeIds = [];
      if (node.id) {
        nodeIds.push(node.id);
      }

      return new QNode(node.category, nodeIds);
    }

    const qNodes = {};
    qNodes[SUBJECT_KEY] = nodeToQNode(subject);
    qNodes[OBJECT_KEY] = nodeToQNode(object);
    const qEdges = {'t_edge': new QEdge(SUBJECT_KEY, OBJECT_KEY, predicate, constraints)};
    return new QGraph(qNodes, qEdges);
  }

  function makeGenetoChemicalConstraints(direction) {
    const qualifierSet = new QEdgeQualifierSet();
    qualifierSet.add(makeQEdgeAspectQualifier('activity_or_abundance'));
    qualifierSet.add(makeQEdgeDirectionQualifier(direction));
    return [qualifierSet];
  }

  function diseaseToTrapiQgraph(disease) {
    return buildTrapiQueryGraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Disease', 'id': disease},
      'treats');
  }

  function geneToTrapiQgraph(gene, direction) {
    return buildTrapiQueryGraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Gene', 'id': gene},
      'affects',
      makeGenetoChemicalConstraints(direction));
  }

  function chemicalToTrapiQgraph(chemical, direction) {
    return buildTrapiQueryGraph(
      {'category': 'ChemicalEntity', 'id': chemical},
      {'category': 'Gene'},
      'affects',
      makeGenetoChemicalConstraints(direction));
  }

  if (!cmn.isObj(query)) {
    throw new TypeError(`Expected query to be type object, got: ${query}`);
  }

  const validKeys = ['type', 'curie', 'direction'];
  for (const key of validKeys) {
    if (!cmn.jsonHasKey(query, key)) {
      throw new ReferenceError(`Expected query to have key ${key}, got: ${query}`);
    }
  }

  let qg = null;
  const queryType = cmn.jsonGet(query, 'type');
  switch (queryType) {
    case 'drug':
      qg = diseaseToTrapiQgraph(cmn.jsonGet(query, 'curie'));
      break;
    case 'gene':
      qg = chemicalToTrapiQgraph(cmn.jsonGet(query, 'curie'), cmn.jsonGet(query, 'direction'));
      break;
    case 'chemical':
      qg = geneToTrapiQgraph(cmn.jsonGet(query, 'curie'), cmn.jsonGet(query, 'direction'));
      break;
    default:
      throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
  }

  return makeTrapiMessage(qg, null);
}

export function nodeIdsToTrapiMessage(nodeIds) {
  return makeTrapiMessage(null, makeKnowledgeGraphFromNodeIds(nodeIds));
}

export function getQueryGraph(trapiMessage) {
  return cmn.jsonGetFromKpath(trapiMessage, ['message', 'query_graph'], false);
}

/*
 * Determine the query template type based on an answer.
 *
 * @param {object} answer - The answer to determine the query type from.
 *
 * @returns {number} - The query type.
 */
export function messageToQueryTemplate(message) {
  const qg = getQueryGraph(message);
  if (!qg)
  {
    return false;
  }

  const [SUBJECT_KEY, OBJECT_KEY] = messageToEndpoints(message);

  const subCategory = cmn.jsonGetFromKpath(qg, ['nodes', SUBJECT_KEY, 'categories'], false)[0];
  const objCategory = cmn.jsonGetFromKpath(qg, ['nodes', OBJECT_KEY, 'categories'], false)[0];
  if (subCategory === bl.tagBiolink('ChemicalEntity') &&
      objCategory === bl.tagBiolink('Gene')) {
    return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
  }
  else if (subCategory === bl.tagBiolink('ChemicalEntity') &&
           objCategory === bl.tagBiolink('Disease')) {
    return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
  }
  else if (subCategory === bl.tagBiolink('Gene') &&
           objCategory === bl.tagBiolink('ChemicalEntity')) {
    return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
  }

  return false;
}

/*
 * Determine which keys correspond to the start and end points of all graphs
 *
 * @param {object} message - The message to determine the start and end keys from.
 *
 * @returns {string[]} - The keys corresponding to the start and end points
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

