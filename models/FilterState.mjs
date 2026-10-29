export {
  make_filter_state_from_req,
  make_filter_state_update_from_req,
  make_filter_state_ref_from_req,
  FilterState,
  FilterStateRequestError,
  FILTER_STATE_MAX_BYTES
}

import * as cmn from "#lib/common.mjs";
import { client_fields } from "#model/common.mjs";

const FILTER_STATE_MAX_BYTES = 64 * 1024;

function make_filter_state_from_req(user_id, filter_state_req) {
  _validate_req_object(filter_state_req, FilterState.create_fields());
  const hidden = filter_state_req.hidden ?? false;
  _validate_hidden(hidden);
  _validate_label(filter_state_req.label, hidden);
  _validate_version(filter_state_req.version);
  _validate_state(filter_state_req.state);
  return new FilterState({
    user_id: user_id,
    label: filter_state_req.label ?? null,
    hidden: hidden,
    version: filter_state_req.version,
    state: filter_state_req.state
  });
}

function make_filter_state_ref_from_req(user_id, filter_state_ref_req) {
  if (!cmn.is_object(filter_state_ref_req)) {
    throw new FilterStateRequestError(`Request body is malformed: ${JSON.stringify(filter_state_ref_req)}`);
  }
  const fields = Object.keys(filter_state_ref_req);
  if (fields.length === 1 && fields[0] === "filter_state_id") {
    _validate_filter_state_id(filter_state_ref_req.filter_state_id);
    return { filter_state_id: filter_state_ref_req.filter_state_id, filter_state: null };
  }
  if (fields.length > 0 && fields.every((field) => _REF_STATE_FIELDS.includes(field))) {
    const filter_state = make_filter_state_from_req(user_id, { hidden: true, ...filter_state_ref_req });
    return { filter_state_id: null, filter_state: filter_state };
  }
  throw new FilterStateRequestError(
    `Request must have either filter_state_id or version and state. Got fields: ${fields.join(", ")}`);
}

function make_filter_state_update_from_req(filter_state_req) {
  _validate_req_object(filter_state_req, FilterState.update_fields());
  const update = {};
  if (filter_state_req.label !== undefined) {
    _validate_label(filter_state_req.label, false);
    update.label = filter_state_req.label;
  }
  if (filter_state_req.hidden !== undefined) {
    _validate_hidden(filter_state_req.hidden);
    update.hidden = filter_state_req.hidden;
  }
  return update;
}

class FilterState {
  constructor({
    id = null,
    user_id,
    label = null,
    hidden = false,
    version,
    state,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.user_id = user_id;
    this.label = label;
    this.hidden = hidden;
    this.version = version;
    this.state = state;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }

  static create_fields() {
    return client_fields(this);
  }

  static update_fields() {
    return client_fields(this).filter((field) => !_IMMUTABLE_FIELDS.includes(field));
  }
}

class FilterStateRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "FilterStateRequestError";
  }
}

const _IMMUTABLE_FIELDS = Object.freeze(["version", "state"]);
const _REF_STATE_FIELDS = Object.freeze(["version", "state"]);

function _validate_req_object(filter_state_req, allowed_fields) {
  if (!cmn.is_object(filter_state_req)) {
    throw new FilterStateRequestError(`Request body is malformed: ${JSON.stringify(filter_state_req)}`);
  }
  let has_fields = false;
  for (const field in filter_state_req) {
    if (!Object.hasOwn(filter_state_req, field)) continue;
    if (!allowed_fields.includes(field)) {
      throw new FilterStateRequestError(
        `Request has unknown field: ${field}. Allowed fields: ${allowed_fields.join(", ")}`);
    }
    has_fields = true;
  }
  if (!has_fields) {
    throw new FilterStateRequestError(`Request must include at least one of: ${allowed_fields.join(", ")}`);
  }
}

function _validate_label(label, hidden) {
  if (cmn.is_missing(label) && hidden) return;
  if (!cmn.is_string(label) || label.length === 0) {
    throw new FilterStateRequestError(`Label must be a non-empty string: ${JSON.stringify(label)}`);
  }
}

function _validate_hidden(hidden) {
  if (!cmn.is_boolean(hidden)) {
    throw new FilterStateRequestError(`Hidden must be a boolean: ${JSON.stringify(hidden)}`);
  }
}

function _validate_version(version) {
  if (!Number.isInteger(version) || version < 0) {
    throw new FilterStateRequestError(`Version must be a non-negative integer: ${JSON.stringify(version)}`);
  }
}

function _validate_state(state) {
  if (state === undefined) {
    throw new FilterStateRequestError("State is required");
  }
}

function _validate_filter_state_id(filter_state_id) {
  if (!Number.isSafeInteger(filter_state_id) || filter_state_id < 1) {
    throw new FilterStateRequestError(`Filter state id must be a positive integer: ${JSON.stringify(filter_state_id)}`);
  }
}
