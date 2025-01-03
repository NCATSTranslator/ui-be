export { QueryService };
import { Query, QueryMetadata } from '../models/Query.mjs';
import * as cmn from '../lib/common.mjs';

class QueryService {
  constructor(queryStore, clientAdapter, feAdapter) {
    this._queryStore = queryStore;
    this._clientAdapter = clientAdapter;
    this.feAdapter = feAdapter;
  }

  async getQueryById(qid) {
    return this._queryStore.retrieveQueryById(qid);
  }

  async getQueryByPk(pk) {
    return this._queryStore.retrieveQueryByPk(pk);
  }

  async processQueryUpdate(update) {
    update = this._clientAdapter.processQueryUpdate(update);
    switch(update.status) {
      case cmn.QUERY_STATUS.RUNNING:
      case cmn.QUERY_STATUS.COMPLETE:
        return this._handleUpdate(update);
      case cmn.QUERY_STATUS.ERROR:
        return this._handleUpdateError(update);
      default:
        return _CALLBACK_RESPONSE.SUCCESS // Ignore the message by default
    }
  }

  async createQuery(pk, queryReq) {
    const query = new Query({pk: pk, metadata: new QueryMetadata(queryReq)});
    const storeQueryModel = await this._queryStore.createQuery(query);
    if (storeQueryModel === null) throw new Error(`Query store failed to create new query with PK: ${pk}`);
    return storeQueryModel;
  }

  async _handleUpdate(update) {
    const pk = update.pk;
    let storeQueryModel = await this.getQueryByPk(pk);
    if (storeQueryModel === null) return _CALLBACK_RESPONSE.NOT_FOUND;
    if (storeQueryModel.status !== cmn.QUERY_STATUS.RUNNING) return _CALLBACK_RESPONSE.SUCCESS; // Ignore updates to finished queries
    if (update.status === cmn.QUERY_STATUS.COMPLETE ||
        update.aras.length > storeQueryModel.metadata.aras.length) {
      // TODO: Optimize to only update the metadata if needed
      storeQueryModel.setAras(update.aras);
      storeQueryModel.setStatus(update.status);
      storeQueryModel = await this._queryStore.updateQuery(storeQueryModel);
      if (storeQueryModel === null) throw new Error(`Failed to update database for PK: ${pk}`);
    }

    return _CALLBACK_RESPONSE.SUCCESS;
  }

  async _handleUpdateError(update) {
    const pk = update.pk
    let storeQueryModel = await this.getQueryByPk(pk);
    if (storeQueryModel === null) return _CALLBACK_RESPONSE.NOT_FOUND;
    storeQueryModel.setStatus(cmn.QUERY_STATUS.ERROR);
    storeQueryModel = await this._queryStore.updateQuery(storeQueryModel);
    if (storeQueryModel === null) throw new Error(`Failed to update database for PK: ${pk}`);
    return _CALLBACK_RESPONSE.SUCCESS;
  }
}

const _CALLBACK_RESPONSE = Object.freeze({
  SUCCESS: 200,
  BAD_REQUEST: 400,
  VERIFICATION_FAILED: 401,
  NOT_FOUND: 404,
  GONE: 410,
  INTERNAL_FAILURE: 500
});
