'use strict';
export {
  load_trapi,
  client_request_to_trapi_query,
  get_pk,
  get_results,
  get_auxiliary_graphs,
  get_auxiliary_graph,
  get_auxiliary_graph_edges,
  get_edge_bindings,
  get_path_bindings,
  get_node_bindings,
  get_kgraph,
  get_kedge,
  get_knode,
  has_knode,
  get_attrs,
  get_attr_id,
  get_attr_val,
  get_primary_source,
  get_subject,
  get_object,
  get_predicate,
  get_support_graphs,
  get_qualifiers,
  get_qualifier_id,
  get_qualifier_val,
  get_knowledge_level,
  get_agent_type,
  get_edge_type,
  message_to_query_type,
  message_to_endpoints,
  is_chemical_disease_query,
  is_gene_chemical_query,
  is_pathfinder_query,
  is_valid_query,
  AttributeIterator,
  EdgeBindingNotFoundError,
  AuxGraphNotFoundError,
  CONSTANTS
}

import { default as hash } from 'hash-sum';
import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';

// Set by configuration. They allow us to assign endpoints to a graph for path generation.
// Both the subject and object keys could be the start or end of a path, but will be consistent
// for a single query.
let SUBJECT_KEY = null;
let OBJECT_KEY = null;

/*
 * Load the TRAPI configuration. Must be called before using the module.
 */
function load_trapi(trapiConfig) {
  SUBJECT_KEY = trapiConfig.query_subject_key
  OBJECT_KEY = trapiConfig.query_object_key
}

/*
 * Convert a request from the client to a TRAPI query for the ARS.
 *
 * @param {object} query - The query to convert.
 * @returns {object} - A TRAPI compliant message wth a query graph.
 */
function client_request_to_trapi_query(client_request) {
  const query = new _Query(client_request);
  if (is_chemical_disease_query(query.type)) {
    return _make_trapi_message(__disease_to_qgraph(query.curie));
  } else if (is_gene_chemical_query(query.type)) {
    return _make_trapi_message(__chemical_to_qgraph(query.curie, query.direction));
  } else if (_is_chemical_gene_query(query.type)) {
    return _make_trapi_message(__gene_to_qgraph(query.curie, query.direction));
  } else if (is_pathfinder_query(query.type)) {
    const subject = new _QNode(SUBJECT_KEY, query.subject.category, [query.subject.id]);
    const object = new _QNode(OBJECT_KEY, query.object.category, [query.object.id]);
    return _make_trapi_message(__make_pathfinder_query_graph(subject, object, query.constraint));
  }
  throw new RangeError(`Expected query type to be one of [drug, gene, chemical, pathfinder], got: ${query.type}`);

  function __make_smart_query_graph(subject, object, qedge) {
    const qnodes = {};
    qnodes[subject.binding] = subject;
    qnodes[object.binding] = object;
    const qedges = {};
    qedges[qedge.gen_binding()] = qedge;
    return new _QGraph(qnodes, qedges);
  }

  function __make_pathfinder_query_graph(subject, object, constraint) {
    const qnodes = {};
    qnodes[subject.binding] = subject;
    qnodes[object.binding] = object;
    const qpath = new _QPath(_QPath.gen_binding(), subject.binding, object.binding, constraint);
    const qpaths = {};
    qpaths[qpath.binding] = qpath;
    return new _QGraph(qnodes, qpaths, 'pathfinder');
  }

  function __make_gene_chemical_constraints(direction) {
    const qualifier_set = new _QEdgeQualifierSet();
    qualifier_set.add(_make_qedge_predicate_qualifier(bl.tagBiolink('causes')));
    qualifier_set.add(_make_qedge_aspect_qualifier('activity_or_abundance'));
    qualifier_set.add(_make_qedge_direction_qualifier(direction));
    return [qualifier_set];
  }

  function __disease_to_qgraph(disease_curie) {
    const subject = new _QNode(SUBJECT_KEY, 'ChemicalEntity');
    const object = new _QNode(OBJECT_KEY, 'Disease', [disease_curie]);
    const predicate = bl.tagBiolink('treats');
    const qedge = new _QEdge(subject, object, predicate);
    return __make_smart_query_graph(subject, object, qedge);
  }

  function __gene_to_qgraph(geneCurie, direction) {
    const subject = new _QNode(SUBJECT_KEY, 'ChemicalEntity');
    const object = new _QNode(OBJECT_KEY, 'Gene', [geneCurie]);
    const predicate = bl.tagBiolink('affects');
    const constraints = __make_gene_chemical_constraints(direction);
    const qedge = new _QEdge(subject, object, predicate, constraints);
    return __make_smart_query_graph(subject, object, qedge);
  }

  function __chemical_to_qgraph(chemicalCurie, direction) {
    const subject = new _QNode(SUBJECT_KEY, 'ChemicalEntity', [chemicalCurie]);
    const object = new _QNode(OBJECT_KEY, 'Gene');
    const predicate = bl.tagBiolink('affects');
    const constraints = __make_gene_chemical_constraints(direction);
    const qedge = new _QEdge(subject, object, predicate, constraints);
    return __make_smart_query_graph(subject, object, qedge);
  }
}

function get_pk(trapi_message) {
  return cmn.jsonGet(trapi_message, CONSTANTS.PRIMARY_KEY, false);
}

function get_results(trapi_message) {
  return cmn.jsonGetFromKpath(
    trapi_message,
    [CONSTANTS.ROOT, CONSTANTS.RESULTS.KEY],
    false);
}

function get_auxiliary_graphs(trapi_message) {
  return cmn.jsonGetFromKpath(
    trapi_message,
    [CONSTANTS.ROOT, CONSTANTS.AGRAPH.KEY],
    false);
}

function get_auxiliary_graph(gid, graphs) {
  return cmn.jsonGet(graphs, gid, false);
}

function get_auxiliary_graph_edges(graph) {
  return cmn.jsonGet(graph, 'edges', []);
}

function get_edge_bindings(analysis) {
  return cmn.jsonGet(analysis, 'edge_bindings', {})
}

function get_node_bindings(result, key) {
  return cmn.jsonGetFromKpath(result, ['node_bindings', key], []);
}

function get_path_bindings(analysis) {
  return cmn.jsonGet(analysis, 'path_bindings', {});
}

function get_kgraph(trapi_message) {
  return cmn.jsonGetFromKpath(trapi_message, [CONSTANTS.ROOT, CONSTANTS.GRAPH.KEY]);
}

function get_kedge(edgeBinding, kgraph) {
  return _get_kgraph_element(edgeBinding, CONSTANTS.GRAPH.EDGES, kgraph);
}

function get_knode(nodeBinding, kgraph) {
  return _get_kgraph_element(nodeBinding, CONSTANTS.GRAPH.NODES, kgraph);
}

function has_knode(nodeBinding, kgraph) {
  return get_knode(nodeBinding, kgraph) !== null;
}

function get_attrs(graph_element) {
  return cmn.jsonGet(graph_element, CONSTANTS.GRAPH.ATTRIBUTES.KEY, []);
}

function get_attr_id(attr) {
  return cmn.jsonGet(attr, CONSTANTS.GRAPH.ATTRIBUTES.ID);
}

function get_attr_val(attr) {
  return cmn.jsonGet(attr, CONSTANTS.GRAPH.ATTRIBUTES.VALUE);
}


/* Gets the primary knowledge source from a TRAPI Graph Element.
 *
 * @param {string} sources - The key to extract from a Graph Element.
 *
 * @returns {function} - The extraction rule.
 */
function get_primary_source(graph_element) {
  const sources = cmn.jsonGet(graph_element, CONSTANTS.GRAPH.SOURCES.KEY, []);
  for (let s of sources) {
    const id = cmn.jsonGet(s, CONSTANTS.GRAPH.SOURCES.ID, false);
    const role = cmn.jsonGet(s, CONSTANTS.GRAPH.SOURCES.ROLE, false);
    const urls = cmn.jsonGet(s, CONSTANTS.GRAPH.SOURCES.URLS, null);
    if (role === undefined || id === undefined) continue;
    if (role === CONSTANTS.GRAPH.SOURCES.PRIMARY) {
      return {
        id: id,
        url: cmn.is_missing(urls) ? null : urls[0]
      };
    }
  }

  throw new Error(`No primary knowledge source found: ${JSON.stringify(graph_element)}`);
}

function get_subject(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.SUBJECT);
}

function get_object(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.OBJECT);
}

function get_predicate(kedge) {
  return cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.PREDICATE);
}

function get_support_graphs(kedge) {
  return _get_graph_element_attr_val(kedge, bl.tagBiolink('support_graphs'), []);
}

function get_qualifiers(kedge) {
  const qualifiers = cmn.jsonGet(kedge, CONSTANTS.GRAPH.EDGE.QUALIFIER.KEY, []);
  if (!cmn.is_array(qualifiers)) {
    throw new _InvalidQualifiersError(kedge);
  }
  return qualifiers;
}

function get_qualifier_id(qualifier) {
  return cmn.jsonGet(qualifier, CONSTANTS.GRAPH.EDGE.QUALIFIER.ID, false);
}

function get_qualifier_val(qualifier) {
  return cmn.jsonGet(qualifier, CONSTANTS.GRAPH.EDGE.QUALIFIER.VALUE);
}

/* Gets the knowledge level from a TRAPI Knowledge Edge.
 *
 * @param {object} kedge - An edge from the Knowledge Graph to extract the knowledge level from.
 *
 * @returns {string} - The knowledge level.
 */
function get_knowledge_level(kedge) {
  return _get_graph_element_attr_val(kedge, bl.tagBiolink('knowledge_level'));
}

/* Gets the agent type from a TRAPI Knowledge Edge.
 *
 * @param {object} kedge - An edge from the Knowledge Graph to extract the agent type from.
 *
 * @returns {string} - The agent type.
 */
function get_agent_type(kedge) {
  return _get_graph_element_attr_val(kedge, bl.tagBiolink('agent_type'));
}

function get_edge_type(kedge) {
  const support_graphs = get_support_graphs(kedge);
  if (cmn.is_array_empty(support_graphs)) return CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
  return CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT;
}


/*
 * Determine the query template type based on the TRAPI message.
 *
 * @param {object} message - The message to determine the query template from.
 *
 * @returns {number} - The query type. See CONSTANTS.QGRAPH.TEMPLATE.
 */
function message_to_query_type(message) {
  const qgraph = _QGraph.from_trapi(_get_query_graph(message));
  return qgraph.get_type();
}

/*
 * Determine which keys correspond to the start and end points of all graphs.
 *
 * @param {object} message - The message to determine the start and end keys from.
 *
 * @returns {string[]} - The keys corresponding to the start and end points.
 */
function message_to_endpoints(message) {
  if (SUBJECT_KEY === null || OBJECT_KEY === null) throw new cmn.DeveloperError('lib/trapi/core.mjs', 'message_to_endpoints', `SUBJECT_KEY (${SUBJECT_KEY}) and OBJECT_KEY (${OBJECT_KEY}) must be configured`);
  const qgraph = _QGraph.from_trapi(_get_query_graph(message));
  if (qgraph.get_type() === CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL) {
    return [OBJECT_KEY, SUBJECT_KEY];
  }
  return [SUBJECT_KEY, OBJECT_KEY];
}

function is_chemical_disease_query(query_type) {
  return query_type === CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
}

function is_gene_chemical_query(query_type) {
  return query_type === CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
}

function is_pathfinder_query(query_type) {
  return query_type === CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
}

function is_valid_query(query_type) {
  return Object.values(CONSTANTS.QGRAPH.TEMPLATE).includes(query_type);
}

class AttributeIterator {
  constructor(graph_element) {
    const attrs = cmn.is_missing(graph_element) ? [] : get_attrs(graph_element);
    if (_has_no_attrs(attrs)) {
      this._attrs = [];
    } else {
      this._attrs = attrs;
    }
    this._index = 0;
  }

  has_next() {
    return this._index < this._attrs.length;
  }

  next() {
    if (!this.has_next()) throw RangeError('next() called on invalid AttributeIterator');
    const current = this._attrs[this._index];
    this._index += 1;
    return current;
  }

  find_one(search_ids, sentinel = null) {
    search_ids = cmn.coerce_array(search_ids);
    while (this.has_next()) {
      const attr = this.next();
      if (search_ids.includes(get_attr_id(attr))) {
        return attr;
      }
    }
    return sentinel
  }

  find_all(search_ids, sentinel = null) {
    search_ids = cmn.coerce_array(search_ids);
    const result = [];
    while (this.has_next()) {
      const attr = this.find_one(search_ids, sentinel);
      if (attr === sentinel) break;
      result.push(attr);
    }
    return result;
  }
}

class EdgeBindingNotFoundError extends Error {
  constructor(edge_binding) {
    super(`Edge binding not found for ${JSON.stringify(edge_binding)}`);
  }
}

class AuxGraphNotFoundError extends Error {
  constructor(aux_graph_binding) {
    super(`Auxiliary Graph binding not found for ${JSON.stringify(aux_graph_binding)}`);
  }
}

const CONSTANTS = Object.freeze({
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
  },
  ATTRIBUTES: {
    ID: {
      PUBLICATIONS: 'publications',
      SUPPORTING_TEXT: 'supporting_text',
      SUPPORTING_TRIALS: 'supporting_trials'
    }
  }
});

function _get_kgraph_element(binding, type, kgraph) {
  return cmn.jsonGet(cmn.jsonGet(kgraph, type, {}), binding, null);
}

function _get_graph_element_attr_val(graph_element, id, default_value = null) {
  const attr_itr = new AttributeIterator(graph_element);
  const attr = attr_itr.find_one(id);
  if (attr === null) return default_value;
  return get_attr_val(attr);
}

function _has_no_attrs(attrs) {
  return cmn.is_missing(attrs) || cmn.is_array_empty(attrs);
}

function _is_chemical_gene_query(query_type) {
  return query_type === CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
}

/*
 * Create a TRAPI message from a query graph and knowledge graph.
 *
 * @param {object} queryGraph? - Optional. The query graph to include in the message.
 * @param {object} knowledgeGraph? - Optional. The knowledge graph to include in the message.
 *
 * @returns {object} - A TRAPI compliant message.
 */
function _make_trapi_message(qgraph, kgraph) {
  if (!(qgraph instanceof _QGraph)) {
    throw new cmn.DeveloperError('lib/trapi/core.mjs', '_make_trapi_message', `qgraph is not an instance of _QGraph\n  Got: ${qgraph}`);
  }
  const message = {
    query_graph: qgraph.to_trapi(),
  };
  if (!cmn.is_missing(kgraph)) {
    message.knowledge_graph = kgraph;
  }
  const trapi_message = { [CONSTANTS.ROOT]: message };
  return trapi_message;
}

/* Convert a query type sent from the client to a query template.
 *
 * @param {string} query_type - The query type from the client.
 *
 * @returns {number} - The query template. See CONSTANTS.QGRAPH.TEMPLATE.
 */
function _query_template_to_type(query_type) {
  switch (query_type) {
    case 'drug':
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
    case 'gene':
      return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
    case 'chemical':
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
    case 'pathfinder':
      return CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
    default:
      throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${query_type}`);
  }
}

function _get_query_graph(trapi_message) {
  const qgraph = cmn.jsonGetFromKpath(trapi_message, [CONSTANTS.ROOT, CONSTANTS.QGRAPH.KEY], null);
  if (cmn.is_missing(qgraph)) throw new _MissingQueryGraphError(trapi_message);
  return qgraph;
}

class _Query {
  constructor(client_request) {
    if (!cmn.is_object(client_request)) {
      throw new TypeError(`Expected client_request to be type object, got: ${client_request}`);
    }

    this.type = _query_template_to_type(cmn.jsonGet(client_request, 'type', null));
    this.curie = cmn.jsonGet(client_request, 'curie', null);
    this.direction = cmn.jsonGet(client_request, 'direction', null);

    // TODO: This is only for pathfinder, we need to fix this
    this.subject = cmn.jsonGet(client_request, 'subject', null);
    this.object = cmn.jsonGet(client_request, 'object', null);
    this.constraint = cmn.jsonGet(client_request, 'constraint', null);
  }
}

class _QNode {
  constructor(binding, category, curies) {
    if (!category) throw new TypeError(`Expected category to be a string, got: ${category}`);
    this.categories = [bl.tagBiolink(category)];
    if (curies && !cmn.is_array_empty(curies)) {
      this.ids = curies;
    }

    this.binding = binding;
  }

  to_trapi() {
    const trapi_qnode = {};
    if (this.ids !== undefined) {
      trapi_qnode.ids = this.ids;
    }
    trapi_qnode.categories = this.categories;
    return trapi_qnode;
  }

  static from_trapi(binding, trapi_qnode) {
    const category = cmn.jsonGet(trapi_qnode, 'categories')[0];
    const curies = cmn.jsonGet(trapi_qnode, 'ids', []);
    return new _QNode(binding, category, curies);
  }
}

class _QEdge {
  constructor(subject, object, predicate, /* TODO: edgeMode, */ constraints) {
    this.subject = subject;
    this.object = object;
    this.predicates = [bl.tagBiolink(predicate)];
    // We only support creative queries right now. In the future we may want to support lookup queries.
    this.query_mode = CONSTANTS.QGRAPH.INFERRED;
    if (constraints && !cmn.is_array_empty(constraints)) {
      this.set_constraints(constraints);
    }
  }

  to_trapi() {
    const trapi = {};
    trapi[CONSTANTS.QGRAPH.EDGE.SUBJECT] = this.subject.binding;
    trapi[CONSTANTS.QGRAPH.EDGE.OBJECT] = this.object.binding;
    trapi[CONSTANTS.QGRAPH.EDGE.PREDICATES] = this.predicates;
    trapi[CONSTANTS.QUALIFIERS.CONSTRAINTS] = this.constraints;
    trapi[CONSTANTS.QGRAPH.EDGE.QUERY_MODE] = this.query_mode;
    return trapi;
  }

  static from_trapi(trapi_qedge, qnodes) {
    const subject = get_subject(trapi_qedge);
    const object = get_object(trapi_qedge);
    const predicate = cmn.jsonGet(trapi_qedge, 'predicates')[0];
    const constraints = cmn.jsonGet(trapi_qedge, 'constraints', []).map(constraint => {
      if (CONSTANTS.QUALIFIERS.SET in constraint) {
        return _QEdgeQualifierSet.from_trapi(constraint[CONSTANTS.QUALIFIERS.SET]);
      } else {
        throw new TypeError(`Unsupported constraint type found while building QEdge: ${constraint}`);
      }
    });

    return new _QEdge(qnodes[subject], qnodes[object], predicate, constraints);
  }

  // Knowledge type is used everywhere in Translator and can mean different things depending on context. In the context of
  // a query edge it can have two values:
  //   1. 'lookup' - The edge will be directly queried from a knowledge graph.
  //   2. 'inferred' - The reasoners are allowed to infer an edge based on combinations of other edges.
  // Right now this essentially chooses between two different "query modes": lookup and creative. In the future a query could
  // have a mix of types.

  get_constraints() {
    return this[CONSTANTS.QUALIFIERS.CONSTRAINTS];
  }

  set_constraints(constraints) {
    this[CONSTANTS.QUALIFIERS.CONSTRAINTS] = constraints;
    return this[CONSTANTS.QUALIFIERS.CONSTRAINTS];
  }

  // TODO: The TRAPI spec seems to allow for more than one qualifier set. What does that mean?
  get_qualifiers() {
    return this.get_constraints().map(constraint => {
      if (CONSTANTS.QUALIFIERS.SET in constraint) {
        return constraint;
      }
    });
  }

  gen_binding() {
    return hash([this.subject, this.object, this.predicates[0]]);
  }
}

class _QEdgeQualifierSet {
  constructor(qualifiers = []) {
    this[CONSTANTS.QUALIFIERS.SET] = qualifiers;
  }

  static from_trapi(trapi_qedge_qualifiers) {
    // Only support qualifier constraints right now. Not even sure there are other constraint types.
    const qualifiers = trapi_qedge_qualifiers.map(qualifier => {
      return QEdgeQualifier.from_trapi(qualifier);
    });
    return new _QEdgeQualifierSet(qualifiers);
  }

  add(qualifier) {
    this[CONSTANTS.QUALIFIERS.SET].push(qualifier);
  }
}

class _QEdgeQualifier {
  constructor(type, val) {
    this[CONSTANTS.QUALIFIERS.ID] = type;
    this[CONSTANTS.QUALIFIERS.VALUE] = val;
  }

  static from_trapi(trapi_qedge_qualifier) {
    const type = cmn.jsonGet(trapi_qedge_qualifier, CONSTANTS.QUALIFIERS.ID);
    const val = cmn.jsonGet(trapi_qedge_qualifier, CONSTANTS.QUALIFIERS.VALUE);
    return new _QEdgeQualifier(type, val);
  }
}

function _make_qedge_aspect_qualifier(aspect) {
  return new _QEdgeQualifier(bl.tagBiolink('object_aspect_qualifier'), aspect);
}

function _make_qedge_direction_qualifier(direction) {
  return new _QEdgeQualifier(bl.tagBiolink('object_direction_qualifier'), direction);
}

function _make_qedge_predicate_qualifier(pred) {
  return new _QEdgeQualifier(bl.tagBiolink('qualified_predicate'), pred);
}

class _QPath {
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
    return new _QPath(binding, subject, object, constraint);
  }

  static gen_binding() {
    return 'p0'; // TODO: Do something else maybe?
  }
}

class _QGraph {
  constructor(qnodes = {}, qconnections = {}, query_type = 'standard') {
    this.qnodes = qnodes;
    this.qconnections = qconnections;
    this._query_type = query_type;
  }

  to_trapi() {
    let connection_key = null;
    if (this._query_type === 'standard') {
      connection_key = 'edges';
    } else if (this._query_type === 'pathfinder') {
      connection_key = 'paths';
    } else {
      throw Error() // TODO: Fix this
    }

    const nodes = {};
    Object.keys(this.qnodes).forEach((binding) => {
      nodes[binding] = this.qnodes[binding].to_trapi();
    });
    const connections = {};
    Object.keys(this.qconnections).forEach((binding) => {
      connections[binding] = this.qconnections[binding].to_trapi();
    });

    const trapi_qgraph = { 'nodes': nodes };
    trapi_qgraph[connection_key] = connections;
    return trapi_qgraph;
  }

  static from_trapi(trapi_qgraph) {
    const qgraph = new _QGraph();
    const nodes = cmn.jsonGet(trapi_qgraph, 'nodes');
    for (const [binding, node] of Object.entries(nodes)) {
      qgraph.qnodes[binding] = _QNode.from_trapi(binding, node);
    }
    const edges = cmn.jsonGet(trapi_qgraph, 'edges', null);
    const paths = cmn.jsonGet(trapi_qgraph, 'paths', null);
    if (edges !== null && paths === null) {
      for (const [binding, edge] of Object.entries(edges)) {
        qgraph.qconnections[binding] = _QEdge.from_trapi(edge, qgraph.qnodes);
      }
    } else if (edges === null && paths !== null) {
      for (const [binding, path] of Object.entries(paths)) {
        qgraph.qconnections[binding] = _QPath.from_trapi(binding, path);
      }
      qgraph._query_type = 'pathfinder';
    } else {
      throw Error(`Error from lib/trapi/core.mjs in from _QGraph.from_trapi:\n  There must be exactly one non-null entry for properties edges and paths\n  Got: ${JSON.stringify(trapi_qgraph)}`);
    }
    return qgraph;
  }

  get_type() {
    if (this._query_type === 'pathfinder') {
      return CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER;
    }
    let [start, end] = [SUBJECT_KEY, OBJECT_KEY];
    const start_is_object = this.qnodes[SUBJECT_KEY].ids ?? null;
    if (start_is_object !== null) {
      [start, end] = [OBJECT_KEY, SUBJECT_KEY];
    }
    const start_category = this.qnodes[start].categories[0];
    const end_category = this.qnodes[end].categories[0];
    if (start_category === bl.tagBiolink('ChemicalEntity')
        && end_category === bl.tagBiolink('Gene')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE;
    } else if (start_category === bl.tagBiolink('ChemicalEntity')
        && end_category === bl.tagBiolink('Disease')) {
      return CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE;
    } else if (start_category === bl.tagBiolink('Gene')
        && end_category === bl.tagBiolink('ChemicalEntity')) {
      return CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL;
    }
    throw new RangeError(`Unsupported query graph template: ${start_category} -> ${end_category}`);
  }
}

class _InvalidQualifiersError extends Error {
  constructor(kedge) {
    super(`Invalid qualifiers in knowledge edge: ${JSON.stringify(kedge)}}`);
  }
}

class _MissingQueryGraphError extends Error {
  constructor(message) {
    super(`No query graph in ${JSON.stringify(message)}`);
  }
}
