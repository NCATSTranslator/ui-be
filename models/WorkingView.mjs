export {
  WorkingView,
  WorkingViewRequestError
}

class WorkingView {
  constructor({
    user_id,
    pk,
    version,
    state,
    time_updated = new Date()
  } = {}) {
    this.user_id = user_id;
    this.pk = pk;
    this.version = version;
    this.state = state;
    this.time_updated = time_updated;
  }
}

class WorkingViewRequestError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "WorkingViewRequestError";
  }
}
