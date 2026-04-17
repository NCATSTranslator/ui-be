export { canvasStorePostgres }

class canvasStorePostgres {
  constructor(db_pool) {
    this._db_pool = db_pool;
  }

  getCanvasByUser(user_id, include_deleted) {
    // TODO: [canvas] implement
    return [];
  }

  createUserCanvas(user_id, init_graph) {
    // TODO: [canvas] implement
    return true;
  }

}
