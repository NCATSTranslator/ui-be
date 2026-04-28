export {
  make_user_canvas_from_req,
  UserCanvas,
  CanvasNode,
  CanvasNodeData,
  CanvasEdge,
  CanvasEdgeData,
  CanvasNodeCreationError,
  CanvasEdgeCreationError,
  CanvasRequestError
}

import * as cmn from "#lib/common.mjs";

function make_user_canvas_from_req(user_id, canvas_req) {
  if (!_is_valid_canvas_req(canvas_req)) throw new CanvasRequestError(`Canvas data is malformed: ${canvas_req}`);
  return new UserCanvas({
    user_id: user_id,
    label: canvas_req.label,
    layout: canvas_req.layout,
    data: canvas_req.data
  });
}

class UserCanvas {
  constructor({
    id = null,
    user_id,
    label,
    layout,
    data = {},
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
