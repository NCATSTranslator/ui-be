export {
  make_user_canvas_from_req,
  make_canvas_update_from_req,
  make_canvas_element_update_from_req,
  make_graph_merge_from_req,
  make_graph_selection_from_req,
  make_graph_geometry_from_req,
  make_annotation_from_req,
  make_annotation_content_update_from_req,
  Graph,
  UserCanvas,
  CanvasGraph,
  CanvasAnnotation,
  CanvasRequestError
}

import * as cmn from "#lib/common.mjs";
import * as taglib from "#lib/taglib.mjs";
import { SummaryNode } from "#lib/summarization/SummaryNode.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";

function make_user_canvas_from_req(user_id, canvas_req) {
  if (!_is_valid_canvas_req(canvas_req)) throw new CanvasRequestError(`Canvas data is malformed: ${JSON.stringify(canvas_req)}`);
  return new UserCanvas({
    user_id: user_id,
    label: canvas_req.label,
    layout: canvas_req.layout,
    data: {
      tags: canvas_req.graph?.tag_descriptions ?? null,
      query_ref: canvas_req.graph?.source?.query_ref ?? null,
      result_ref: canvas_req.graph?.source?.result_ref ?? null
    }
  });
}

function make_canvas_update_from_req(canvas_req) {
  if (!cmn.is_object(canvas_req)) {
    throw new CanvasRequestError(`Canvas update is malformed: ${JSON.stringify(canvas_req)}`);
  }
  const update = {};
  if (canvas_req.label !== undefined) {
    if ("string" !== typeof canvas_req.label) {
      throw new CanvasRequestError(`Canvas label must be a string: ${JSON.stringify(canvas_req.label)}`);
    }
    update.label = canvas_req.label;
  }
  if (canvas_req.layout !== undefined) {
    if (!_VALID_LAYOUTS.includes(canvas_req.layout)) {
      throw new CanvasRequestError(`Canvas layout is invalid: ${JSON.stringify(canvas_req.layout)}`);
    }
    update.layout = canvas_req.layout;
  }
  if (Object.keys(update).length === 0) {
    throw new CanvasRequestError("Canvas update must include at least one of: label, layout");
  }
  return update;
}

function make_canvas_element_update_from_req(element_req) {
  if (!cmn.is_object(element_req)) {
    throw new CanvasRequestError(`Canvas element update is malformed: ${JSON.stringify(element_req)}`);
  }
  const update = {};
  if (element_req.label !== undefined) {
    if ("string" !== typeof element_req.label) {
      throw new CanvasRequestError(`Canvas element label must be a string: ${JSON.stringify(element_req.label)}`);
    }
    update.label = element_req.label;
  }
  if (element_req.hidden !== undefined) {
    if ("boolean" !== typeof element_req.hidden) {
      throw new CanvasRequestError(`Canvas element hidden must be a boolean: ${JSON.stringify(element_req.hidden)}`);
    }
    update.hidden = element_req.hidden;
  }
  if (Object.keys(update).length === 0) {
    throw new CanvasRequestError("Canvas element update must include at least one of: label, hidden");
  }
  return update;
}

function make_graph_merge_from_req(graph_req, secret) {
  if (!cmn.is_object(graph_req) || cmn.is_object_empty(graph_req)) {
    throw new CanvasRequestError(`Graph merge request is malformed: ${JSON.stringify(graph_req)}`);
  }
  const graph = Graph.from_req({ graph: graph_req }, secret);
  const tag_descriptions = graph_req.tag_descriptions ?? null;
  return { graph: graph, tag_descriptions: tag_descriptions };
}

function make_graph_selection_from_req(graph_req) {
  if (!cmn.is_object(graph_req)) {
    throw new CanvasRequestError(`Graph selection is malformed: ${JSON.stringify(graph_req)}`);
  }
  const node_ids = _validate_graph_id_array(graph_req.nodes, "nodes");
  const edge_ids = _validate_graph_id_array(graph_req.edges, "edges");
  if (node_ids.length === 0 && edge_ids.length === 0) {
    throw new CanvasRequestError("Graph selection must include at least one node or edge id");
  }
  return { node_ids: node_ids, edge_ids: edge_ids };
}

function make_graph_geometry_from_req(geometry_req) {
  if (!cmn.is_object(geometry_req)) {
    throw new CanvasRequestError(`Graph geometry is malformed: ${JSON.stringify(geometry_req)}`);
  }
  const node_moves = __make_node_moves(geometry_req.nodes);
  const annotation_geometries = __make_annotation_geometries(geometry_req.annotations);
  if (node_moves.length === 0 && annotation_geometries.length === 0) {
    throw new CanvasRequestError("Graph geometry must include at least one node or annotation");
  }
  return { node_moves: node_moves, annotation_geometries: annotation_geometries };

  function __make_node_moves(raw_nodes) {
    if (cmn.is_missing(raw_nodes)) return [];
    if (!cmn.is_array(raw_nodes)) {
      throw new CanvasRequestError(`Graph geometry nodes must be an array: ${JSON.stringify(raw_nodes)}`);
    }
    return raw_nodes.map((raw) => {
      if (!cmn.is_object(raw) || !Number.isInteger(raw.data_id)) {
        throw new CanvasRequestError(`Graph geometry node requires an integer data_id: ${JSON.stringify(raw)}`);
      }
      _validate_coord_pair(raw);
      return { data_id: raw.data_id, x: raw.x, y: raw.y };
    });
  }

  function __make_annotation_geometries(raw_annotations) {
    if (cmn.is_missing(raw_annotations)) return [];
    if (!cmn.is_array(raw_annotations)) {
      throw new CanvasRequestError(`Graph geometry annotations must be an array: ${JSON.stringify(raw_annotations)}`);
    }
    return raw_annotations.map((raw) => {
      if (!cmn.is_object(raw) || !Number.isInteger(raw.id)) {
        throw new CanvasRequestError(`Graph geometry annotation requires an integer id: ${JSON.stringify(raw)}`);
      }
      _validate_coord_pair(raw);
      const size = _validate_annotation_size(raw);
      return { id: raw.id, x: raw.x, y: raw.y, ...size };
    });
  }
}

function make_annotation_from_req(canvas_id, annotation_req) {
  const annotation = __validate_annotation_req(annotation_req);
  return new CanvasAnnotation({ canvas_id: canvas_id, ...annotation });

  function __validate_annotation_req(annotation_req) {
    if (!cmn.is_object(annotation_req)) {
      throw new CanvasRequestError(`Canvas annotation is malformed: ${JSON.stringify(annotation_req)}`);
    }
    if ("string" !== typeof annotation_req.content) {
      throw new CanvasRequestError(`Canvas annotation content must be a string: ${JSON.stringify(annotation_req.content)}`);
    }
    _validate_coord_pair(annotation_req);
    _validate_extent(annotation_req.width, "width");
    _validate_extent(annotation_req.height, "height");
    return {
      content: annotation_req.content,
      x: annotation_req.x,
      y: annotation_req.y,
      width: annotation_req.width,
      height: annotation_req.height
    };
  }
}

function make_annotation_content_update_from_req(annotation_req) {
  if (!cmn.is_object(annotation_req)) {
    throw new CanvasRequestError(`Canvas annotation update is malformed: ${JSON.stringify(annotation_req)}`);
  }
  if ("string" !== typeof annotation_req.content) {
    throw new CanvasRequestError(`Canvas annotation content must be a string: ${JSON.stringify(annotation_req.content)}`);
  }
  return { content: annotation_req.content };
}

function _validate_annotation_size(raw) {
  const has_width = raw.width !== undefined;
  const has_height = raw.height !== undefined;
  if (has_width !== has_height) {
    throw new CanvasRequestError(`Graph geometry annotation requires width and height together: ${JSON.stringify(raw)}`);
  }
  if (!has_width) return { width: null, height: null };
  _validate_extent(raw.width, "width");
  _validate_extent(raw.height, "height");
  return { width: raw.width, height: raw.height };
}

function _validate_coord_pair(raw) {
  _validate_coord(raw.x, "x");
  _validate_coord(raw.y, "y");
}

function _validate_coord(value, field) {
  if (!Number.isFinite(value)) {
    throw new CanvasRequestError(`Canvas geometry ${field} must be a number: ${JSON.stringify(value)}`);
  }
}

function _validate_extent(value, field) {
  if (!Number.isFinite(value)) {
    throw new CanvasRequestError(`Canvas geometry ${field} must be a number: ${JSON.stringify(value)}`);
  }
  if (value < 0) {
    throw new CanvasRequestError(`Canvas geometry ${field} must not be negative: ${JSON.stringify(value)}`);
  }
}

function _validate_graph_id_array(ids, field) {
  if (cmn.is_missing(ids)) return [];
  if (!cmn.is_array(ids) || !ids.every((id) => Number.isInteger(id))) {
    throw new CanvasRequestError(`Graph ${field} must be an array of integer ids: ${JSON.stringify(ids)}`);
  }
  return ids;
}

function _entity_data_to_canvas_tags(entity_data) {
  const tags = {};
  for (const tag of taglib.get_tags(entity_data)) {
    tags[tag.id] = null;
  }
  return tags;
}

function _make_graph_nodes(canvas_req, secret) {
  const raw_nodes = canvas_req.graph?.nodes;
  if (cmn.is_missing(raw_nodes)) return [];
  if (!cmn.is_object(raw_nodes)) {
    throw new CanvasRequestError(`Graph nodes must be a map of node id to node: ${JSON.stringify(raw_nodes)}`);
  }
  return Object.entries(raw_nodes).map(([id, raw]) => {
    if (!cmn.is_object(raw)) {
      throw new CanvasRequestError(`Graph node ${id} is malformed`);
    }
    return GraphNode.from_object({ ...raw, id: id }, secret);
  });
}

function _make_graph_edges(canvas_req, secret) {
  const raw_edges = canvas_req.graph?.edges;
  if (cmn.is_missing(raw_edges)) return [];
  if (!cmn.is_object(raw_edges)) {
    throw new CanvasRequestError(`Graph edges must be a map of edge id to edge: ${JSON.stringify(raw_edges)}`);
  }
  return Object.entries(raw_edges).map(([id, raw]) => {
    if (!cmn.is_object(raw)) {
      throw new CanvasRequestError(`Graph edge ${id} is malformed`);
    }
    return GraphEdge.from_object({ ...raw, id: id }, secret);
  });
}

class UserCanvas {
  constructor({
    id = null,
    user_id,
    label,
    layout,
    data,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.user_id = user_id;
    this.label = label;
    this.layout = layout;
    this.data = data;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }

  populate_from_raw(canvas) {
    this.id = canvas.id;
    this.label = canvas.label;
    this.layout = canvas.layout;
    this.data = canvas.data;
    this.time_created = canvas.time_created;
    this.time_updated = canvas.time_updated;
    this.time_deleted = canvas.time_deleted;
  }
}

class CanvasNode {
  constructor({
    id = null,
    canvas_id = null,
    data_id = null,
    ref,
    label,
    type,
    x = null,
    y = null,
    hidden = false,
    tags = {},
    time_created = new Date(),
    time_updated = new Date()
  } = {}) {
    this.id = id;
    this.canvas_id = canvas_id;
    this.data_id = data_id;
    this.ref = ref;
    this.label = label;
    this.type = type;
    this.x = x;
    this.y = y;
    this.hidden = hidden;
    this.tags = tags;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = null;
  }
}

class CanvasNodeData {
  constructor({
    id = null,
    ref,
    data,
    time_created = new Date(),
    time_updated = new Date()
  } = {}) {
    this.id = id;
    this.ref = ref;
    this.data = data;
    this.time_created = time_created;
    this.time_updated = time_updated;
  }
}

class CanvasEdge {
  constructor({
    id = null,
    canvas_id = null,
    data_id = null,
    subject_id = null,
    object_id = null,
    ref,
    label,
    hidden = false,
    tags = {},
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.canvas_id = canvas_id;
    this.data_id = data_id;
    this.subject_id = subject_id;
    this.object_id = object_id;
    this.ref = ref;
    this.label = label;
    this.hidden = hidden;
    this.tags = tags;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }
}

class CanvasEdgeData {
  constructor({
    id = null,
    ref,
    data,
    time_created = new Date(),
    time_updated = new Date()
  } = {}) {
    this.id = id;
    this.ref = ref;
    this.data = data;
    this.time_created = time_created;
    this.time_updated = time_updated;
  }
}

class CanvasAnnotation {
  constructor({
    id = null,
    canvas_id = null,
    content,
    x = null,
    y = null,
    width = null,
    height = null,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.canvas_id = canvas_id;
    this.content = content;
    this.x = x;
    this.y = y;
    this.width = width;
    this.height = height;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }
}

class GraphNode {
  constructor({
    data,
    x,
    y,
    hidden = false,
    label = null
  } = {}) {
    this.data = data;
    this.x = x;
    this.y = y;
    this.hidden = hidden;
    this.label = label;
  }

  static from_object(raw, secret) {
    if (!cmn.is_object(raw)) {
      throw new CanvasRequestError(`Graph node is malformed: ${JSON.stringify(raw)}`);
    }
    if (!Number.isFinite(raw.x) || !Number.isFinite(raw.y)) {
      throw new CanvasRequestError(`Graph node requires numeric x and y coordinates: ${JSON.stringify(raw)}`);
    }
    let data;
    try {
      data = SummaryNode.from_object(raw);
    } catch (err) {
      throw new CanvasRequestError(`Graph node is not a valid node: ${err.message}`);
    }
    if (!cmn.verify_entity_data(data.to_raw_obj(), raw.signature, secret)) {
      throw new CanvasRequestError(`Graph node ${data.id} has an invalid or missing signature`);
    }
    return new GraphNode({
      data: data,
      x: raw.x,
      y: raw.y,
      hidden: raw.hidden ?? false,
      label: raw.label ?? null
    });
  }

  ref() {
    return this.data.id;
  }

  to_canvas_node_data() {
    return new CanvasNodeData({
      ref: this.ref(),
      data: this.data.to_raw_obj()
    });
  }

  to_canvas_node(canvas_id, data_id) {
    return new CanvasNode({
      canvas_id: canvas_id,
      data_id: data_id,
      ref: this.ref(),
      label: this.label ?? this.data.name(),
      type: this.data.get_specific_type(),
      x: this.x,
      y: this.y,
      hidden: this.hidden,
      tags: _entity_data_to_canvas_tags(this.data)
    });
  }
}

class GraphEdge {
  constructor({
    data,
    hidden = false,
    label = null
  } = {}) {
    this.data = data;
    this.hidden = hidden;
    this.label = label;
  }

  static from_object(raw, secret) {
    if (!cmn.is_object(raw)) {
      throw new CanvasRequestError(`Graph edge is malformed: ${JSON.stringify(raw)}`);
    }
    let data;
    try {
      data = SummaryEdge.from_object(raw);
    } catch (err) {
      throw new CanvasRequestError(`Graph edge is not a valid edge: ${err.message}`);
    }
    if (!cmn.verify_entity_data(data.to_raw_obj(), raw.signature, secret)) {
      throw new CanvasRequestError(`Graph edge ${data.id} has an invalid or missing signature`);
    }
    return new GraphEdge({
      data: data,
      hidden: raw.hidden ?? false,
      label: raw.label ?? null
    });
  }

  ref() {
    return this.data.id;
  }

  subject_ref() {
    return this.data.subject;
  }

  object_ref() {
    return this.data.object;
  }

  to_canvas_edge_data() {
    return new CanvasEdgeData({
      ref: this.ref(),
      data: this.data.to_raw_obj()
    });
  }

  to_canvas_edge(canvas_id, data_id, subject_id, object_id) {
    return new CanvasEdge({
      canvas_id: canvas_id,
      data_id: data_id,
      subject_id: subject_id,
      object_id: object_id,
      ref: this.ref(),
      label: this.label ?? this.data.predicate,
      hidden: this.hidden,
      tags: _entity_data_to_canvas_tags(this.data)
    });
  }
}

class Graph {
  constructor({ nodes = [], edges = [] } = {}) {
    this._nodes = nodes;
    this._edges = edges;
  }

  static from_req(canvas_req, secret) {
    const nodes = _make_graph_nodes(canvas_req, secret);
    const edges = _make_graph_edges(canvas_req, secret);
    return new Graph({ nodes: nodes, edges: edges });
  }

  nodes() {
    return this._nodes;
  }

  edges() {
    return this._edges;
  }

  assert_edges_reference_nodes(known_node_refs = []) {
    const node_refs = new Set(this._nodes.map((node) => node.ref()));
    for (const ref of known_node_refs) {
      node_refs.add(ref);
    }
    for (const edge of this._edges) {
      if (!node_refs.has(edge.subject_ref()) || !node_refs.has(edge.object_ref())) {
        throw new CanvasRequestError(
          `Graph edge ${edge.ref()} references a node not present in the graph `
          + `(subject=${edge.subject_ref()}, object=${edge.object_ref()})`);
      }
    }
  }
}

class CanvasGraph {
  constructor({ nodes = [], edges = [], annotations = [], tags = null } = {}) {
    this.nodes = nodes;
    this.edges = edges;
    this.annotations = annotations;
    this.tags = tags;
  }
}

class CanvasRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "CanvasRequestError";
  }
}

function _is_valid_canvas_req(canvas_req) {
  return !cmn.is_missing(canvas_req)
         && _is_valid_label(canvas_req.label)
         && _is_valid_layout(canvas_req.layout);

  function _is_valid_label(label) {
    return "string" === typeof label;
  }
  function _is_valid_layout(layout) {
    return _VALID_LAYOUTS.includes(layout);
  }
}

const _VALID_LAYOUTS = Object.freeze(["horizontal", "vertical", "concentric", "custom"]);
