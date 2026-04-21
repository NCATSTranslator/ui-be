export {
  Canvas,
  CanvasNode,
  CanvasNodeData,
  CanvasEdge,
  CanvasEdgeData,
  CanvasCreationError,
  CanvasNodeCreationError,
  CanvasEdgeCreationError,
  CanvasRequestError
}

import * as cmn from "#lib/common.mjs";

class Canvas {
  constructor({
    id = null,
    label,
    layout,
    tags = {},
    graph = {},
    time_created = new Date(),
    time_updated = new Date()
  } = {}) {
    this.id = id;
    this.label = label;
    this.layout = layout;
    this.tags = tags;
    this.graph = graph;
    this.time_created = time_created;
    this.time_updated = time_updated;
  }
}

class CanvasNode {
  constructor({
    id = null,
    canvas_id,
    data_id,
    ref,
    label,
    type,
    x,
    y,
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
    if (cmn.is_any_missing(ref, data)) {
    }
    this.id = id;
    this.ref = ref;
    this.data = data;
    this.time_created = time_created;
    this.time_updated = time_updated;
    t
  }
}

class CanvasEdge {
  constructor({
    id = null,
    canvas_id,
    data_id,
    subject_id,
    object_id,
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
    this.time_created = time_created,
    this.time_updated = time_updated,
    this.time_deleted = time_deleted
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

class CanvasNodeCreationError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "CanvasNodeCreationError";
  }
}

class CanvasEdgeCreationError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "CanvasEdgeCreationError";
  }
}

class CanvasRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "CanvasRequestError";
  }
}

class CanvasCreationError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "CanvasCreationError";
  }
}
