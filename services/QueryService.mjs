import { Query } from '../models/Query.mjs';

export { QueryService };

class QueryService {
  constructor(queryStore) {
    this._queryStore = queryStore;
  }

  async getQueryById(qid) {
    return this._queryStore.retrieveUserById(qid);
  }

  async getQueryByPk(pk) {
    return this._queryStore.retrieveUserByPk(pk);
  }

  async createQuery(model) {
    return this._queryStore.createQuery(model);
  }
}


