export { QueryService, QUERY_SERVICE_MSG };
import { Query, QueryMetadata } from '../models/Query.mjs';
import { SAVE_TYPE, mark_user_query_unseen } from '../models/UserSavedData.mjs';
import { logger } from '../lib/logger.mjs';
import * as cmn from '../lib/common.mjs';

const QUERY_SERVICE_MSG = Object.freeze({
  UPDATE_SUCCESS:  0,
  QUERY_NOT_FOUND: 1,
  QUERY_COMPLETE:  2,
  UPDATE_IGNORED:  3
});

class QueryService {
  constructor(queryStore, userStore, clientAdapter) {
    this._queryStore = queryStore;
    this._userStore = userStore;
    this._clientAdapter = clientAdapter;
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
      case cmn.QUERY_STATUS.COMPLETE: return this._handleUpdate(update);
      case cmn.QUERY_STATUS.ERROR:    return this._handleUpdateError(update);
      default: return QUERY_SERVICE_MSG.UPDATE_IGNORED
    }
  }

  async createQuery(pk, queryReq) {
    const query = new Query({pk: pk, metadata: new QueryMetadata(queryReq)});
    const storeQueryModel = await this._queryStore.createQuery(query);
    if (storeQueryModel === null) throw new Error(`Query store failed to create new query with PK: ${pk}`);
    return storeQueryModel;
  }

  async addQueryUserRelationship(queryModel, userQueryModel) {
    return this._queryStore.addQueryUserRelationship(queryModel, userQueryModel);
  }

  async _handleUpdate(update) {
    const pk = update.pk;
    let storeQueryModel = await this.getQueryByPk(pk);
    if (storeQueryModel === null) return QUERY_SERVICE_MSG.QUERY_NOT_FOUND;
    if (storeQueryModel.status !== cmn.QUERY_STATUS.RUNNING) return QUERY_SERVICE_MSG.QUERY_COMPLETE
    if (update.status === cmn.QUERY_STATUS.COMPLETE ||
        update.aras.length > storeQueryModel.metadata.aras.length) {
      // TODO: Optimize to only update the metadata if needed
      if (!cmn.isArrayEmpty(update.aras)) {
        storeQueryModel.setAras(update.aras);
      }
      storeQueryModel.setStatus(update.status);

      storeQueryModel = await this._queryStore.updateQuery(storeQueryModel);
      if (storeQueryModel === null) throw new Error(`Failed to update database for PK: ${pk}`);
      let usersQueries = await this._userStore.retrieveSavedDataBy({
        ars_pkey: pk,
        save_type: SAVE_TYPE.QUERY
      });
      if (userQueries === null) {
        logger.warn(`Query updated that is not associated with any user\n  PK: ${pk}`);
        return QUERY_SERVICE_MSG.UPDATE_SUCCESS;
      }
      userQueries.map(mark_user_query_unseen);
      userQueries = await this._userStore.updateUserSavedDataBatch(userQueries);
      if (userQueries === null) throw new Error(`Failed to update database for PK: ${pk}`);
    }

    return QUERY_SERVICE_MSG.UPDATE_SUCCESS;
  }

  async _handleUpdateError(update) {
    const pk = update.pk
    let storeQueryModel = await this.getQueryByPk(pk);
    if (storeQueryModel === null) return QUERY_SERVICE_MSG.QUERY_NOT_FOUND;
    storeQueryModel.setStatus(cmn.QUERY_STATUS.ERROR);
    storeQueryModel = await this._queryStore.updateQuery(storeQueryModel);
    if (storeQueryModel === null) throw new Error(`Failed to update database for PK: ${pk}`);
    return QUERY_SERVICE_MSG.UPDATE_SUCCESS;
  }
}

