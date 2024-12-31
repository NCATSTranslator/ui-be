export { QueryService };
import { Query } from '../models/Query.mjs';
import * as cmn from '../lib/common.mjs';

class QueryService {
  constructor(queryStore, queryClient, outputAdapter) {
    this._queryStore = queryStore;
    this._queryClient = queryClient;
    this.outputAdapter = outputAdapter;
  }

  async getQueryById(qid) {
    return this._queryStore.retrieveQueryById(qid);
  }

  async getQueryByPk(pk) {
    return this._queryStore.retrieveQueryByPk(pk);
  }

  async getQueryStatus(pk) {
    const storeQueryModel = this.getQueryByPk(pk);
    if (storeQueryModel === null) throw new Error(`Failed to get query with ID: ${pk} from store`);
    return storeQueryModel;
  }

  async processQueryUpdate(update) {
    update = this._queryClient.processQueryUpdate(update);
    console.log(update);
    switch(update.status) {
      case cmn.CONSTANTS.QUERY_STATUS.RUNNING:
      case cmn.CONSTANTS.QUERY_STATUS.FINISHED:
        return await this._handleUpdate(update);
      case cmn.CONSTANTS.QUERY_STATUS.ERROR:
        return await this._handleUpdateError();
      default:
        return _CONSTANTS.CALLBACK_RESPONSE.SUCCESS // Ignore the message by default
    }
  }

  async createQuery(pk, queryReq) {
    const metadata = {
      query: queryReq,
      aras: []
    };
    const storeQueryModel = await this._queryStore.createQuery(new Query({pk: pk, metadata: metadata}));
    if (storeQueryModel === null) throw new Error(`Failed to create new query with PK: ${pk}`);
    return storeQueryModel;
  }

  async _handleUpdate(update) {
    const pk = update.pk;
    let storeQueryModel = await this.getQueryByPk(pk);
    if (storeQueryModel === null) return _CONSTANTS.CALLBACK_RESPONSE.GONE;
    if (storeQueryModel.status !== cmn.CONSTANTS.QUERY_STATUS.RUNNING) return _CONSTANTS.CALLBACK_RESPONSE.SUCCESS; // Ignore updates to finished queries
    if (update.status === cmn.CONSTANTS.QUERY_STATUS.FINISHED ||
        update.aras.length > storeQueryModel.metadata.aras.length) {
      // TODO: Optimize to only update the metadata if needed
      storeQueryModel.metadata.aras = update.aras; // TODO: Absraction is broken
      storeQueryModel.update({ status: update.status });
      storeQueryModel = await this._queryStore.updateQuery(storeQueryModel);
      if (storeQueryModel === null) throw new Error(`Failed to update database for PK: ${pk}`);
    }

    return _CONSTANTS.CALLBACK_RESPONSE.SUCCESS;
  }

  async _handleUpdateError() {
    // TODO
    console.log('GDP WHA??');
    return;
  }
}

const _CONSTANTS = Object.freeze({
  CALLBACK_RESPONSE: {
    SUCCESS: 200,
    BAD_REQUEST: 400,
    VERIFICATION_FAILED: 401,
    NOT_FOUND: 404,
    GONE: 410,
    INTERNAL_FAILURE: 500
  }
});


