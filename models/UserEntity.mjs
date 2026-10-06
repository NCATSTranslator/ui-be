export {
  make_user_node_from_req,
  make_user_edge_from_req,
  make_user_node_update_from_req,
  make_user_edge_update_from_req,
  UserNode,
  UserEdge,
  UserEntityRequestError,
  USER_NODE_DEFAULT_TYPE
}

import * as cmn from "#lib/common.mjs";
import { client_fields } from "#model/common.mjs";

const USER_NODE_DEFAULT_TYPE = "Other";

const _USER_EDGE_ENDPOINT_FIELDS = Object.freeze([
  "subject_ref", "subject_user_node_id", "object_ref", "object_user_node_id"
]);

function make_user_node_from_req(user_id, node_req) {
  _validate_req_object(node_req, UserNode.create_fields());
  _validate_label(node_req.label);
  _validate_data(node_req.data);
  if (node_req.type !== undefined) _validate_type(node_req.type);
  return new UserNode({
    user_id: user_id,
    label: node_req.label,
    type: node_req.type ?? USER_NODE_DEFAULT_TYPE,
    data: node_req.data
  });
}

function make_user_edge_from_req(user_id, edge_req) {
  _validate_req_object(edge_req, UserEdge.create_fields());
  _validate_label(edge_req.label);
  _validate_data(edge_req.data);
  const subject = _make_user_edge_endpoint(edge_req, "subject");
  const object = _make_user_edge_endpoint(edge_req, "object");
  return new UserEdge({
    user_id: user_id,
    label: edge_req.label,
    data: edge_req.data,
    subject_ref: subject.ref,
    subject_user_node_id: subject.user_node_id,
    object_ref: object.ref,
    object_user_node_id: object.user_node_id
  });
}

function make_user_node_update_from_req(node_req) {
  _validate_req_object(node_req, UserNode.update_fields());
  const update = _make_update(node_req);
  if (node_req.type !== undefined) {
    _validate_type(node_req.type);
    update.type = node_req.type;
  }
  return update;
}

function make_user_edge_update_from_req(edge_req) {
  _validate_req_object(edge_req, UserEdge.update_fields());
  return _make_update(edge_req);
}

function _make_update(entity_req) {
  const update = {};
  if (entity_req.label !== undefined) {
    _validate_label(entity_req.label);
    update.label = entity_req.label;
  }
  if (entity_req.data !== undefined) {
    _validate_data(entity_req.data);
    update.data = entity_req.data;
  }
  return update;
}

function _make_user_edge_endpoint(edge_req, end) {
  const ref = edge_req[`${end}_ref`];
  const user_node_id = edge_req[`${end}_user_node_id`];
  const has_ref = !cmn.is_missing(ref);
  const has_user_node = !cmn.is_missing(user_node_id);
  if (has_ref === has_user_node) {
    throw new UserEntityRequestError(`User edge requires exactly one of ${end}_ref or ${end}_user_node_id`);
  }
  if (has_ref && !(cmn.is_string(ref) && ref.length > 0)) {
    throw new UserEntityRequestError(`User edge ${end}_ref must be a non-empty string: ${JSON.stringify(ref)}`);
  }
  if (has_user_node && !Number.isInteger(user_node_id)) {
    throw new UserEntityRequestError(
      `User edge ${end}_user_node_id must be an integer: ${JSON.stringify(user_node_id)}`);
  }
  return {
    ref: has_ref ? ref : null,
    user_node_id: has_user_node ? user_node_id : null
  };
}

function _validate_req_object(entity_req, allowed_fields) {
  if (!cmn.is_object(entity_req)) {
    throw new UserEntityRequestError(`Request body is malformed: ${JSON.stringify(entity_req)}`);
  }
  let has_fields = false;
  for (const field in entity_req) {
    if (!Object.hasOwn(entity_req, field)) continue;
    if (!allowed_fields.includes(field)) {
      throw new UserEntityRequestError(
        `Request has unknown field: ${field}. Allowed fields: ${allowed_fields.join(", ")}`);
    }
    has_fields = true;
  }
  if (!has_fields) {
    throw new UserEntityRequestError(`Request must include at least one of: ${allowed_fields.join(", ")}`);
  }
}

function _validate_label(label) {
  if (!cmn.is_string(label)) {
    throw new UserEntityRequestError(`Label must be a string: ${JSON.stringify(label)}`);
  }
}

function _validate_type(type) {
  if (!cmn.is_string(type) || type.length === 0) {
    throw new UserEntityRequestError(`User node type must be a non-empty string: ${JSON.stringify(type)}`);
  }
}

function _validate_data(data) {
  if (!cmn.is_object(data)) {
    throw new UserEntityRequestError(`Data must be a JSON object: ${JSON.stringify(data)}`);
  }
}

class UserNode {
  constructor({
    id = null,
    user_id,
    label,
    type = USER_NODE_DEFAULT_TYPE,
    data,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.user_id = user_id;
    this.label = label;
    this.type = type;
    this.data = data;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }

  static create_fields() {
    return client_fields(this);
  }

  static update_fields() {
    return client_fields(this);
  }
}

class UserEdge {
  constructor({
    id = null,
    user_id,
    label,
    data,
    subject_ref = null,
    subject_user_node_id = null,
    object_ref = null,
    object_user_node_id = null,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.user_id = user_id;
    this.label = label;
    this.data = data;
    this.subject_ref = subject_ref;
    this.subject_user_node_id = subject_user_node_id;
    this.object_ref = object_ref;
    this.object_user_node_id = object_user_node_id;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }

  static create_fields() {
    return client_fields(this);
  }

  static update_fields() {
    return client_fields(this).filter((field) => !_USER_EDGE_ENDPOINT_FIELDS.includes(field));
  }
}

class UserEntityRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "UserEntityRequestError";
  }
}
