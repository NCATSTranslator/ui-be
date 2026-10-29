export {
  QueryView,
  QueryViewRequestError
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
