'use strict';
import { default as hash } from 'hash-sum';
import * as cmn from './common.mjs';
import * as bl from './biolink-model.mjs';

// Set by configuration. They allow us to assign endpoints to a graph for path generation.
// Both the subject and object keys could be the start or end of a path, but will be consistent
// for a single query.
let SUBJECT_KEY = null;
let OBJECT_KEY = null;

export const CONSTANTS = Object.freeze({
  ROOT: 'message',
  PRIMARY_KEY: 'pk',
  QUALIFIERS: {
    CONSTRAINTS: 'qualifier_constraints',
    SET: 'qualifier_set',
    ID: 'qualifier_type_id',
    VALUE: 'qualifier_value'
  },
  QGRAPH: {
    KEY: 'query_graph',
    INFERRED: 'inferred',
    TEMPLATE: {
      CHEMICAL_GENE: 0,
      CHEMICAL_DISEASE: 1,
      GENE_CHEMICAL: 2,
      PATHFINDER: 3
    },
    NODES: 'nodes',
    EDGES: 'edges',
    EDGE: {
      SUBJECT: 'subject',
      OBJECT: 'object',
      PREDICATES: 'predicates',
      QUERY_MODE: 'knowledge_type'
    }
  },
  GRAPH: {
    KEY: 'knowledge_graph',
    NODES: 'nodes',
    EDGES: 'edges',
    EDGE: {
      SUBJECT: 'subject',
      OBJECT: 'object',
      PREDICATE: 'predicate',
      TYPE: {
        DIRECT: 'direct',
        INDIRECT: 'indirect'
      },
      QUALIFIER: {
        KEY: 'qualifiers',
        ID: 'qualifier_type_id',
        VALUE: 'qualifier_value'
      }
    },
    ATTRIBUTES: {
      KEY: 'attributes',
      ID: 'attribute_type_id',
      VALUE: 'value'
    },
    SOURCES: {
      KEY: 'sources',
      ID: 'resource_id',
      ROLE: 'resource_role',
      PRIMARY: 'primary_knowledge_source',
      URLS: 'source_record_urls'
    }
  },
  AGRAPH: {
    KEY: 'auxiliary_graphs'
  },
  RESULTS: {
    KEY: 'results',
  }
});

class QNode {
  constructor(binding, category, curies) {
    if (!category) throw new TypeError(`Expected category to be a string, got: ${category}`);
    this.categories = [bl.tagBiolink(category)];
    if (curies && !cmn.isArrayEmpty(curies)) {
      this.ids = curies;
    }

    this.binding = binding;
  }

  to_trapi() {
    const trapiQNode = {};
    if (this.ids !== undefined) {
      trapiQNode.ids = this.ids;
    }
    trapiQNode.categories = this.categories;
    return trapiQNode;
  }

  static from_trapi(binding, trapiQNode) {
    const category = cmn.jsonGet(trapiQNode, 'categories')[0];
    const curies = cmn.jsonGet(trapiQNode, 'ids', []);
    return new QNode(binding, category, curies);
  }
}

class QEdge {
  constructor(sub, obj, pred, /* TODO: edgeMode, */ constraints) {
    this.subject = sub;
    this.object = obj;
    this.predicates = [bl.tagBiolink(pred)];
    // We only support creative queries right now. In the future we may want to support lookup queries.
    this.queryMode = CONSTANTS.QGRAPH.INFERRED;
    if (constraints && !cmn.isArrayEmpty(constraints)) {
      this.constraints = constraints;
    }
  }

  to_trapi() {
    const trapi = {};
    trapi[CONSTANTS.QGRAPH.EDGE.SUBJECT] = this.subject.binding;
    trapi[CONSTANTS.QGRAPH.EDGE.OBJECT] = this.object.binding;
    trapi[CONSTANTS.QGRAPH.EDGE.PREDICATES] = this.predicates;
    trapi[CONSTANTS.QUALIFIERS.CONSTRAINTS] = this.constraints;
    trapi[CONSTANTS.QGRAPH.EDGE.QUERY_MODE] = this.queryMode;
    return trapi;
  }

  static from_trapi(trapiQEdge, qNodes) {
    const sub = getSub(trapiQEdge);
    const obj = getObj(trapiQEdge);
    const pred = cmn.jsonGet(trapiQEdge, 'predicates')[0];
    const constraints = cmn.jsonGet(trapiQEdge, 'constraints', []).map(constraint => {
      if (CONSTANTS.QUALIFIERS.SET in constraint) {
        return QEdgeQualifierSet.from_trapi(constraint[CONSTANTS.QUALIFIERS.SET]);
      } else {
        throw new TypeError(`Unsupported constraint type found while building QEdge: ${constraint}`);
      }
    });

    return new QEdge(qNodes[sub], qNodes[obj], pred, constraints);
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

  genBinding() {
    return hash([this.subject, this.object, this.predicates[0]]);
  }
}

class QEdgeQualifierSet {
  constructor(qualifiers = []) {
    this[CONSTANTS.QUALIFIERS.SET] = qualifiers;
  }

  static from_trapi(trapiQEdgeQualifiers) {
    // Only support qualifier constraints right now. Not even sure there are other constraint types.
    const qualifiers = trapiQEdgeQualifiers.map(qualifier => {
      return QEdgeQualifier.from_trapi(qualifier);
    });
    return new QEdgeQualifierSet(qualifiers);
  }

  // Lets just say its a multiset
  add(qualifier) {
    this[CONSTANTS.QUALIFIERS.SET].push(qualifier);
  }
}

class QEdgeQualifier {
  constructor(type, val) {
    this[CONSTANTS.QUALIFIERS.ID] = type;
    this[CONSTANTS.QUALIFIERS.VALUE] = val;
  }

  static from_trapi(trapiQEdgeQualifier) {
    const type = cmn.jsonGet(trapiQEdgeQualifier, CONSTANTS.QUALIFIERS.ID);
    const val = cmn.jsonGet(trapiQEdgeQualifier, CONSTANTS.QUALIFIERS.VALUE);
    return new QEdgeQualifier(type, val);
  }
}

class QPath {
  constructor(binding, subject, object, constraint) {
    this.binding = binding;
    this.subject = subject;
    this.object = object;
    this.constraint = constraint;
  }

  to_trapi() {
    const trapi =  {
      "subject": this.subject,
      "object": this.object,
    };
    if (this.constraint !== null) {
      trapi["constraints"] = [{ "intermediate_categories": [this.constraint] }]
    }
    return trapi;
  }

  static from_trapi(binding, trapi_qpath) {
    const subject = cmn.jsonGet(trapi_qpath, 'subject');
    const object = cmn.jsonGet(trapi_qpath, 'object');
    let constraint = null;
    const constraints = cmn.jsonGet(trapi_qpath, 'constraints', false);
    if (constraints) {
      constraint = cmn.jsonGet(constraints, 'intermediate_categories', null);
    }
    return new QPath(binding, subject, object, constraint);
  }

  static gen_binding() {
    return 'p0'; // TODO: Do something else maybe?
  }
}

function makeQEdgeAspectQualifier(aspect) {
  return new QEdgeQualifier(bl.tagBiolink('object_aspect_qualifier'), aspect);
}

function makeQEdgeDirectionQualifier(direction) {
  return new QEdgeQualifier(bl.tagBiolink('object_direction_qualifier'), direction);
}

function makeQEdgePredQualifier(pred) {
  return new QEdgeQualifier(bl.tagBiolink('qualified_predicate'), pred);
}

class QGraph {
  constructor(qNodes = {}, qConnections = {}, queryType = 'standard') {
    this.qNodes = qNodes;
    this.qConnections = qConnections;
    this._queryType = queryType;
  }

  to_trapi() {
    let connectionKey = null;
    if (this._queryType === 'standard') {
      connectionKey = 'edges';
    } else if (this._queryType === 'pathfinder') {
      connectionKey = 'paths';
    } else {
      throw Error() // TODO: Fix this
    }

    const nodes = {};
    Object.keys(this.qNodes).forEach((binding) => {
      nodes[binding] = this.qNodes[binding].to_trapi();
    });
    const connections = {};
    Object.keys(this.qConnections).forEach((binding) => {
      connections[binding] = this.qConnections[binding].to_trapi();
    });

    const qg = { 'nodes': nodes };
    qg[connectionKey] = connections;
    return qg;
  }

  static from_trapi(trapiQGraph) {
    const qg = new QGraph();
    const nodes = cmn.jsonGet(trapiQGraph, 'nodes');
    for (const [binding, node] of Object.entries(nodes)) {
      qg.qNodes[binding] = QNode.from_trapi(binding, node);
    }

    const edges = cmn.jsonGet(trapiQGraph, 'edges', false);
    if (edges) {
      for (const [binding, edge] of Object.entries(edges)) {
        qg.qConnections[binding] = QEdge.from_trapi(edge, qg.qNodes);
      }
    } else {
      const paths = cmn.jsonGet(trapiQGraph, 'paths');
      for (const [binding, path] of Object.entries(paths)) {
        qg.qConnections[binding] = QPath.from_trapi(binding, path);
      }
      qg._queryType = 'pathfinder';
    }

    return qg;
  }

  get template() {
    if (this._queryType === 'pathfinder') {
      return CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
    }
    let [start, end] = [SUBJECT_KEY, OBJECT_KEY];
    const startIsObj = this.qNodes[SUBJECT_KEY].ids
    if (startIsObj) {
      [start, end] = [OBJECT_KEY, SUBJECT_KEY];
    }

    const startCategory = this.qNodes[start].categories[0];
    const endCategory = this.qNodes[end].categories[0];
    if (startCategory === bl.tagBiolink('ChemicalEntity') &&
        endCategory === bl.tagBiolink('Gene')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
    } else if (startCategory === bl.tagBiolink('ChemicalEntity') &&
               endCategory === bl.tagBiolink('Disease')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
    } else if (startCategory === bl.tagBiolink('Gene') &&
               endCategory === bl.tagBiolink('ChemicalEntity')) {
      return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
    }
    throw new RangeError(`Unsupported query graph template: ${startCategory} -> ${endCategory}`);
  }
}

class Query {
  constructor(clientReq) {
    if (!cmn.isObject(clientReq)) {
      throw new TypeError(`Expected clientReq to be type object, got: ${clientReq}`);
    }

    this.template = queryTypeToTemplate(cmn.jsonGet(clientReq, 'type', null));
    this.curie = cmn.jsonGet(clientReq, 'curie', null);
    this.direction = cmn.jsonGet(clientReq, 'direction', null);

    // TODO: This is only for pathfinder, we need to fix this
    this.subject = cmn.jsonGet(clientReq, 'subject', null);
    this.object = cmn.jsonGet(clientReq, 'object', null);
    this.constraint = cmn.jsonGet(clientReq, 'constraint', null);
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
  function make_smart_query_graph(subNode, objNode, qEdge) {
    const qNodes = {};
    qNodes[subNode.binding] = subNode;
    qNodes[objNode.binding] = objNode;
    const qEdges = {};
    qEdges[qEdge.genBinding()] = qEdge;
    return (new QGraph(qNodes, qEdges)).to_trapi();
  }

  function make_pathfinder_query_graph(subject, object, constraint) {
    const subject_node = new QNode(SUBJECT_KEY, subject.category, [subject.id]);
    const object_node = new QNode(OBJECT_KEY, object.category, [object.id]);
    const qnodes = {};
    qnodes[subject_node.binding] = subject_node;
    qnodes[object_node.binding] = object_node;
    const qpath = new QPath(QPath.gen_binding(), subject_node.binding, object_node.binding, constraint);
    const qpaths = {};
    qpaths[qpath.binding] = qpath;
    const qgraph = (new QGraph(qnodes, qpaths, 'pathfinder')).to_trapi();
    return qgraph;
  }

  function makeGeneChemicalConstraints(direction) {
    const qualifierSet = new QEdgeQualifierSet();
    qualifierSet.add(makeQEdgePredQualifier(bl.tagBiolink('causes')));
    qualifierSet.add(makeQEdgeAspectQualifier('activity_or_abundance'));
    qualifierSet.add(makeQEdgeDirectionQualifier(direction));
    return [qualifierSet];
  }

  function diseaseToTrapiQgraph(diseaseCurie) {
    const subNode = new QNode(SUBJECT_KEY, 'ChemicalEntity');
    const objNode = new QNode(OBJECT_KEY, 'Disease', [diseaseCurie]);
    return make_smart_query_graph(
      subNode,
      objNode,
      new QEdge(subNode, objNode, bl.tagBiolink('treats')));
  }

  function geneToTrapiQgraph(geneCurie, direction) {
    const subNode = new QNode(SUBJECT_KEY, 'ChemicalEntity');
    const objNode = new QNode(OBJECT_KEY, 'Gene', [geneCurie]);
    return make_smart_query_graph(
      subNode,
      objNode,
      new QEdge(subNode, objNode, bl.tagBiolink('affects'), makeGeneChemicalConstraints(direction)));
  }

  function chemicalToTrapiQgraph(chemicalCurie, direction) {
    const subNode = new QNode(SUBJECT_KEY, 'ChemicalEntity', [chemicalCurie]);
    const objNode = new QNode(OBJECT_KEY, 'Gene');
    return make_smart_query_graph(
      subNode,
      objNode,
      new QEdge(subNode, objNode, bl.tagBiolink('affects'), makeGeneChemicalConstraints(direction)));
  }

  const query = new Query(clientReq);
  if (isChemicalDiseaseQuery(query.template)) {
    return makeTrapiMessage(diseaseToTrapiQgraph(query.curie));
  } else if (isGeneChemicalQuery(query.template)) {
    return makeTrapiMessage(chemicalToTrapiQgraph(query.curie, query.direction));
  } else if (isChemicalGeneQuery(query.template)) {
    return makeTrapiMessage(geneToTrapiQgraph(query.curie, query.direction));
  } else if (isPathfinderQuery(query.template)) {
    return makeTrapiMessage(make_pathfinder_query_graph(query.subject, query.object, query.constraint));
  }

  throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
}

export function nodeIdsToTrapiMessage(nodeIds) {
  return makeTrapiMessage(false, makeKgraphFromNodeIds(nodeIds));
}

export function getPk(trapiMessage) {
  return cmn.jsonGet(trapiMessage, CONSTANTS.PRIMARY_KEY, false);
}

export function getQueryGraph(trapiMessage) {
  const qg = cmn.jsonGetFromKpath(trapiMessage, [CONSTANTS.ROOT, CONSTANTS.QGRAPH.KEY], false);
  if (!qg) {
    throw new MissingQueryGraphError(trapiMessage);
  }

  return qg;
}

export function getResults(trapiMessage) {
  const results = cmn.jsonGetFromKpath(trapiMessage, [CONSTANTS.ROOT, CONSTANTS.RESULTS.KEY], false);
  return results;
}

export function getAuxGraphs(trapiMessage) {
  const auxGraphs = cmn.jsonGetFromKpath(trapiMessage, [CONSTANTS.ROOT, CONSTANTS.AGRAPH.KEY], false);
  return auxGraphs;
}

export function getAuxGraph(gid, auxGraphs) {
  return cmn.jsonGet(auxGraphs, gid, false);
}

export function getAuxGraphEdges(auxGraph) {
  return cmn.jsonGet(auxGraph, 'edges', []);
}

export function getEdgeBindings(analysis) {
  return cmn.jsonGet(analysis, 'edge_bindings', {})
}

export function getPathBindings(analysis) {
  return cmn.jsonGet(analysis, 'path_bindings', {});
}

export function getNodeBinding(result, key) {
  return cmn.jsonGetFromKpath(result, ['node_bindings', key], []);
}

export function getKgraph(trapiMessage) {
  return cmn.jsonGetFromKpath(trapiMessage, [CONSTANTS.ROOT, CONSTANTS.GRAPH.KEY]);
}

export function getKedge(edgeBinding, kgraph) {
  return getKgraphElem(edgeBinding, CONSTANTS.GRAPH.EDGES, kgraph);
}

export function getKnode(nodeBinding, kgraph) {
  return getKgraphElem(nodeBinding, CONSTANTS.GRAPH.NODES, kgraph);
}

export function hasKnode(nodeBinding, kgraph) {
  return getKnode(nodeBinding, kgraph) !== null;
}

export class AttributeIterator {
  constructor(attrs) {
    if (noAttrs(attrs)) {
      this._attrs = [];
    } else {
      this._attrs = attrs;
    }

    this._index = 0;
  }

  hasNext() {
    return this._index < this._attrs.length;
  }

  findOne(searchIds, sentinel = null) {
    while (this.hasNext()) {
      const attr = this._attrs[this._index++];
      if (searchIds.includes(getAttrId(attr))) {
        return attr;
      }
    }

    return sentinel
  }

  findOneVal(searchIds, sentinel = null) {
    const attr = this.findOne(searchIds, sentinel);
    if (attr !== sentinel) {
      return getAttrVal(attr);
    }

    return sentinel;
  }

  findAll(searchIds, sentinel = null) {
    const result = [];
    while (this.hasNext()) {
      const attr = this.findOne(searchIds, sentinel);
      if (attr === sentinel) break;
      result.push(attr);
    }

    return result;
  }

  findAllVal(searchIds, sentinel = null) {
    const result = [];
    while (this.hasNext()) {
      const attr = this.findOne(searchIds, sentinel);
      if (attr === sentinel) break;
      const attrVal = getAttrVal(attr);
      if (attrVal !== null) {
        result.push(...cmn.coerceArray(attrVal));
      }
    }

    return result;
  }
}

export function getAttrs(graphElem) {
  return cmn.jsonGet(graphElem, CONSTANTS.GRAPH.ATTRIBUTES.KEY, []);
}

export function getAttrId(attr) {
  return cmn.jsonGet(attr, CONSTANTS.GRAPH.ATTRIBUTES.ID);
}

export function getAttrVal(attr) {
  return cmn.jsonGet(attr, CONSTANTS.GRAPH.ATTRIBUTES.VALUE);
}

export function noAttrs(attrs) {
  return attrs === undefined || attrs === null || cmn.isArrayEmpty(attrs);
}

/* Gets the primary knowledge source from a TRAPI Graph Element.
 *
 * @param {string} sources - The key to extract from a Graph Element.
 *
 * @returns {function} - The extraction rule.
 */
export function getPrimarySrc(graphElem) {
  const srcs = cmn.jsonGet(graphElem, CONSTANTS.GRAPH.SOURCES.KEY, []);
  for (let src of srcs) {
    const id = cmn.jsonGet(src, CONSTANTS.GRAPH.SOURCES.ID, false);
    const role = cmn.jsonGet(src, CONSTANTS.GRAPH.SOURCES.ROLE, false);
    if (!role || !id) continue;
    if (role === CONSTANTS.GRAPH.SOURCES.PRIMARY) return id;
  }

  throw new Error(`No primary knowledge source found: ${JSON.stringify(graphElem)}`);
}

export function getSub(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.SUBJECT);
}

export function getObj(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.OBJECT);
}

export function getPred(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.PREDICATE);
}

export function getSupGraphs(kedge) {
  return _getGraphElemAttrVal(kedge, bl.tagBiolink('support_graphs'), []);
}

export function getQualifiers(kedge) {
  const qualifiers = cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.QUALIFIER.KEY, []);
  if (!cmn.isArray(qualifiers)) {
    throw new InvalidQualifiersError(kedge);
  }

  return qualifiers;
}

export function getQualifierId(qualifier) {
  return cmn.jsonGet(qualifier, CONSTANTS.GRAPH.EDGE.QUALIFIER.ID, false);
}

export function getQualifierVal(qualifier) {
  return cmn.jsonGet(qualifier, CONSTANTS.GRAPH.EDGE.QUALIFIER.VALUE);
}

/* Gets the knowledge level from a TRAPI Knowledge Edge.
 *
 * @param {object} kedge - An edge from the Knowledge Graph to extract the knowledge level from.
 *
 * @returns {string} - The knowledge level.
 */
export function getKlevel(kedge) {
  return _getGraphElemAttrVal(kedge, bl.tagBiolink('knowledge_level'));
}

/* Gets the agent type from a TRAPI Knowledge Edge.
 *
 * @param {object} kedge - An edge from the Knowledge Graph to extract the agent type from.
 *
 * @returns {string} - The agent type.
 */
export function getAgentType(kedge) {
  return _getGraphElemAttrVal(kedge, bl.tagBiolink('agent_type'));
}

/*
 * Determine the query template type based on the TRAPI message.
 *
 * @param {object} message - The message to determine the query template from.
 *
 * @returns {number} - The query template. See CONSTANTS.QGRAPH.TEMPLATE.
 */
export function messageToQueryTemplate(message) {
  const qg = QGraph.from_trapi(getQueryGraph(message));
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
  const qg = QGraph.from_trapi(getQueryGraph(message));
  if (qg.template === CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL) {
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

export function isPathfinderQuery(queryType) {
  return queryType === CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
}

export function isValidQuery(queryType) {
  return Object.values(CONSTANTS.QGRAPH.TEMPLATE).includes(queryType);
}

export function getEdgeType(kedge) {
  const supGraphs = getSupGraphs(kedge);
  if (cmn.isArrayEmpty(supGraphs)) return CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
  return CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT;
}

export class EdgeBindingNotFoundError extends Error {
  constructor(edgeBinding) {
    super(`Edge binding not found for ${JSON.stringify(edgeBinding)}`);
  }
}

export class AuxGraphNotFoundError extends Error {
  constructor(aux_graph_binding) {
    super(`Auxiliary Graph binding not found for ${JSON.stringify(aux_graph_binding)}`);
  }
}

function getKgraphElem(binding, type, kgraph) {
  return cmn.jsonGet(cmn.jsonGet(kgraph, type, {}), binding, null);
}

function _getGraphElemAttrVal(graphElem, id, defaultVal = null) {
  const attrs = getAttrs(graphElem);
  const attrIter = new AttributeIterator(attrs);
  const attrVal = attrIter.findOneVal([id]);
  if (attrVal === null) return defaultVal;
  return attrVal;
}

/*
 * Create a minimal TRAPI message from a collection of node CURIEs
 *
 * @param {Array} nodeIds - Array of CURIEs
 * @returns {Object} - TRAPI message
 */
function makeKgraphFromNodeIds(nodeIds) {
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
function makeTrapiMessage(queryGraph, kgraph) {
  const message = {};
  if (queryGraph) {
    message['query_graph'] = queryGraph;
  }

  if (kgraph) {
    message['knowledge_graph'] = kgraph;
  }

  const trapiMessage = {};
  trapiMessage[CONSTANTS.ROOT] = message;
  return trapiMessage;
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
    case 'pathfinder':
      return CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
    default:
      throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
  }
}

class InvalidQualifiersError extends Error {
  constructor(kedge) {
    super(`Invalid qualifiers in knowledge edge: ${JSON.stringify(kedge)}}`);
  }
}

class MissingQueryGraphError extends Error {
  constructor(message) {
    super(`No query graph in ${JSON.stringify(message)}`);
  }
}
