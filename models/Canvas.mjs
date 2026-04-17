export {
  Canvas,
  CanvasCreationError,
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
    if (cmn.is_missing(label)) throw new CanvasRequestError("Label is a required field");
    if (cmn.is_missing(layout)) throw new CanvasRequestError("Layout is a required field");
    this.id = id;
    this.label = label;
    this.layout = layout;
    this.tags = tags;
    this.graph = graph;
    this.time_created = time_created;
    this.time_updated = time_updated;
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
