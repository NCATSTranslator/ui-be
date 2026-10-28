export {
  make_query_view_from_req,
  QueryView,
  QueryViewRequestError
}

import * as cmn from "#lib/common.mjs";
import { make_filter_state_from_req, FilterStateRequestError } from "#model/FilterState.mjs";

function make_query_view_from_req(user_id, query_view_req) {
  if (!cmn.is_object(query_view_req)) {
    throw new QueryViewRequestError(`Request body is malformed: ${JSON.stringify(query_view_req)}`);
  }
  const fields = Object.keys(query_view_req);
  if (fields.length === 1 && fields[0] === "filter_state_id") {
    _validate_filter_state_id(query_view_req.filter_state_id);
    return { filter_state_id: query_view_req.filter_state_id, filter_state: null };
  }
  if (fields.length > 0 && fields.every((field) => _FILTER_STATE_FIELDS.includes(field))) {
    try {
      const filter_state = make_filter_state_from_req(user_id, { hidden: true, ...query_view_req });
      return { filter_state_id: null, filter_state: filter_state };
    } catch (err) {
      if (err instanceof FilterStateRequestError) throw new QueryViewRequestError(err.message);
      throw err;
    }
  }
  throw new QueryViewRequestError(
    `Request must have either filter_state_id or version and state. Got fields: ${fields.join(", ")}`);
}

class QueryView {
  constructor({
    id = null,
    user_id,
    pk,
    filter_state,
    time_created = new Date(),
    time_updated = new Date(),
    time_deleted = null
  } = {}) {
    this.id = id;
    this.user_id = user_id;
    this.pk = pk;
    this.filter_state = filter_state;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.time_deleted = time_deleted;
  }
}

class QueryViewRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "QueryViewRequestError";
  }
}

const _FILTER_STATE_FIELDS = Object.freeze(["version", "state"]);

function _validate_filter_state_id(filter_state_id) {
  if (!Number.isSafeInteger(filter_state_id) || filter_state_id < 1) {
    throw new QueryViewRequestError(`Filter state id must be a positive integer: ${JSON.stringify(filter_state_id)}`);
  }
}
