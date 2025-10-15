'use strict';
export { QueryAPIController };
import { QUERY_SERVICE_MSG } from '../services/QueryService.mjs';
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as trapi from '../lib/trapi.mjs';
import { logger } from "../lib/logger.mjs";

class QueryAPIController {
  constructor(config, translatorService, translatorServicexFEAdapter,
      queryService, queryServicexFEAdapter, userService, filters) {
    this.config = config;
    this.apiKey = config.secrets.hmac.key;
    this.use_hmac = config.ars_endpoint.use_hmac_validation;
    this.translatorService = translatorService;
    this.translatorServicexFEAdapter = translatorServicexFEAdapter;
    this.queryService = queryService;
    this.queryServicexFEAdapter = queryServicexFEAdapter;
    this.userService = userService;
    this.filters = filters;
    this.using_pubsub = this.config.ars_endpoint.use_pubsub;
    if (this.using_pubsub) {
      this.submitQuery = this._submitQueryViaPubSub;
      this.getQueryStatus = this._getQueryStatusViaPubSub;
      this.getUserQueries = this._getUserQueriesViaPubSub;
    } else {
      this.submitQuery = this._submitQueryViaPolling;
      this.getQueryStatus = this._getQueryStatusViaPolling;
      this.getUserQueries = this._getUserQueriesViaPolling;
    }
    this.submitQuery.bind(this);
    this.getQueryStatus.bind(this);
    this.getUserQueries.bind(this);
  }

  async getQueryResult(req, res, next) {
    if (!this._isValidQueryResultRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      let results = await this.translatorService.getResults(uuid, this.filters);
      results = await this.translatorServicexFEAdapter.queryResultsToFE(
          results, this.config.max_hops, this.config.ara_to_infores_map);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(results);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async updateQuery(req, res, next) {
    const reqVerification = this._isValidQueryUpdateRequest(req);
    if (!reqVerification.valid) {
      return wutil.sendError(res, reqVerification.errorCode, reqVerification.errorMsg);
    }
    try {
      const update = req.body;
      const queryServiceMsg = await this.queryService.processQueryUpdate(update);
      if (this.use_hmac) {
        res.set(_CUSTOM_HEADERS.X_EVENT_SIG, cmn.generateHMACSignature(JSON.stringify(res.body), this.apiKey));
      }
      return res.status(this._queryServiceMsgToHTTPCode(queryServiceMsg)).send();
    } catch (err) {
      // TODO: Send errors at more granular level
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async update_user_query(req, res, next) {
    const user_query = req.body;
    if (!user_query) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to contain user query. Got: ${JSON.stringify(user_query)}`);
    }
    const uid = req.sessionData.user.id;
    const is_deleted = user_query.data.deleted;
    try {
      let user_saved_data = await this.userService.getUserSavesBy(uid,
        {id: parseInt(user_query.sid, 10)},
        is_deleted);
      if (!user_saved_data) {
        return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Failed to update user query. Does not exist');
      }
      user_saved_data = user_saved_data[0];
      user_saved_data.data.bookmark_ids = user_query.data.bookmark_ids;
      user_saved_data.data.title = user_query.data.title;
      user_saved_data.data.seen = user_query.data.seen;
      user_saved_data = await this.userService.updateUserSave(user_saved_data, is_deleted);
      if (!user_saved_data) {
        throw Error('PANIC: Database failed to update user query but did not throw');
      }
      user_query.data.time_updated = user_saved_data.time_updated;
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_query);
    }
    catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, `Database failed to update given usery query: ${JSON.stringify(user_query)}`);
    }
  }

  async deleteUserQueries(req, res, next) {
    const query_ids = await req.body;
    if (!cmn.isArray(query_ids)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const uid = req.sessionData.user.id;
    try {
      const _ = await this.userService.deleteUserSaveBatch(uid, query_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to update queries from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to update queries from the database');
    }
  }

  async restoreUserQueries(req, res, next) {
    const query_ids = await req.body;
    if (!cmn.isArray(query_ids)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const uid = req.sessionData.user.id;
    let queries = null;
    try {
      const _ = await this.userService.restoreUserSaveBatch(uid, query_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to update queries from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to update queries from the database');
    }
  }

  async _submitQueryViaPubSub(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const queryRequest = req.body;
      const pid = queryRequest.pid;
      let project = null;
      const uid = req.sessionData.user.id;
      if (pid) {
        const projects = await this.userService.getUserSavesBy(uid, {id: pid});
        if (!projects || cmn.isArrayEmpty(projects)) {
          throw new Error(`Submitted query includes unknown PID: ${pid}`);
        }
        project = projects[0];
        req.log.info({project: project});
      }
      const trapiQuery = this.translatorService.inputToQuery(queryRequest);
      req.log.info({query: trapiQuery});
      const submitResp = await this.translatorService.submitQuery(trapiQuery);
      req.log.info({arsqueryresp: submitResp});
      const pk = trapi.getPk(submitResp);
      if (!pk) throw new Error(`ARS query submission response has no PK: ${submitResp}`);
      const queryModel = await this.queryService.createQuery(pk, req.body);
      if (!queryModel) throw new Error(`Failed to create query with PK: ${pk}`);
      //TODO: verify subscribe response
      const subscribeResp = await this.translatorService.subscribeQuery(pk);
      const userQueryModel = await this.userService.createUserQuery(uid, pk, queryModel.metadata.query);
      if (!userQueryModel) throw new Error(`User service failed to create entry for query ${queryModel.id} and user ${uid}`);
      const isUserAssignedQuery = this.queryService.addQueryUserRelationship(queryModel, userQueryModel);
      if (!isUserAssignedQuery) throw new Error(`Query service failed to associate query ${queryModel.id} with user save ${userQueryModel.id}`);
      if (pid) {
        project.data.pks.push(pk);
        const updatedProject = await this.userService.updateUserSave(project);
        if (!updatedProject) {
          throw new Error(`Error updating project: ${pid} with PK: ${pk}`);
        }
      }
      return res.status(200).json(this.queryServicexFEAdapter.querySubmitToFE(queryModel));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async _getQueryStatusViaPubSub(req, res, next) {
    if (!this._isValidQueryResultRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      const queryModel = await this.queryService.getQueryByPk(uuid);
      let status = null;
      if (queryModel) {
        status = this.queryServicexFEAdapter.queryStatusToFE(queryModel);
      } else {
        const statusResp = await this.translatorService.getQueryStatus(uuid, this.filters);
        status = this.translatorServicexFEAdapter.queryStatusToFE(statusResp);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(status);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async _getUserQueriesViaPubSub(req, res, next) {
    const uid = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    try {
      const user_queries = await this.userService.get_user_queries_map(uid, include_deleted, this.using_pubsub);
      return res.status(cmn.HTTP_CODE.SUCCESS).json([...user_queries.values()]);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      wutil.logInternalServerError(req, `Failed to get queries status for the current user: ${uid}`);
      return wutil.sendInternalServerError(res, 'Failed to get queries status for the current user');
    }
  }

  async _submitQueryViaPolling(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const queryRequest = req.body;
      const pid = parseInt(queryRequest.pid, 10);
      const uid = req.sessionData.user.id;
      let project = null;
      if (pid) {
        const projects = await this.userService.getUserSavesBy(uid, {id: pid});
        if (!projects) {
          throw new Error(`Submitted query includes unknown PID: ${pid}`);
        }
        req.log.info({projects: projects});
        project = projects[0];
      }
      const trapiQuery = this.translatorService.inputToQuery(req.body);
      const submitResp = await this.translatorService.submitQuery(trapiQuery);
      req.log.info({ltype: 'query-submission', query_params: req.body, ars_response: submitResp}, 'Query submission and response');
      const pk = trapi.getPk(submitResp);
      const userQueryModel = this.userService.createUserQuery(uid, pk, req.body);
      if (!userQueryModel) throw new Error(`User service failed to create entry for query ${pk} and user ${uid}`);
      if (pid) {
        project.data.pks.push(pk);
        const updatedProject = await this.userService.updateUserSave(project);
        if (!updatedProject) {
          throw new Error(`Error updating project: ${pid} with PK: ${pk}`);
        }
      }
      return res.status(200).json(this.translatorServicexFEAdapter.querySubmitToFE(submitResp));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async _getQueryStatusViaPolling(req, res, next) {
    if (!this._isValidQueryResultRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed Request');
    }
    try {
      let uuid = req.params.qid;
      let statusResp = await this.translatorService.getQueryStatus(uuid, this.filters);
      logger.debug({ltype: 'query-status from service', statusResp: statusResp});
      let retval = this.translatorServicexFEAdapter.queryStatusToFE(statusResp);
      logger.debug({ltype: 'query-status after adapter', value: retval});
      return res.status(200).json(retval);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async _getUserQueriesViaPolling(req, res, next) {
    const uid = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    try {
      const user_queries = await this.userService.get_user_queries_map(uid, include_deleted, this.using_pubsub);
      const pks = [...user_queries.keys()];
      const resps = await Promise.all(pks.map(pk => this.translatorService.getQueryStatus(pk, this.filters)));
      for (const resp of resps) {
        const status_model = this.translatorServicexFEAdapter.queryStatusToFE(resp);
        const user_query = user_queries.get(status_model.data.qid);
        user_query.status = status_model.status;
        user_query.data.aras = status_model.data.aras;
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json([...user_queries.values()]);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      wutil.logInternalServerError(req, `Failed to get queries status for the current user: ${uid}`);
      return wutil.sendInternalServerError(res, 'Failed to get queries status for the current user');
    }
  }

  _isValidQuerySubmissionRequest(req) {
    return cmn.isObject(req.body) && req.sessionData.user.id;
  }

  _isValidQueryResultRequest(req) {
    return req.params.hasOwnProperty('qid') && req.params.qid;
  }

  _isValidQueryUpdateRequest(req) {
    const reqVerification = {
      valid: true,
      errorCode: null,
      errorMsg: ''
    };
    if (!this.use_hmac) return reqVerification;
    const signature = req.headers[_CUSTOM_HEADERS.X_EVENT_SIG];
    if (!signature) {
      reqVerification.valid = false;
      reqVerification.errorCode = cmn.HTTP_CODE.BAD_REQUEST;
      reqVerification.errorMsg = 'Signature not provided';
    } else if (!cmn.verifyHMACSignature(signature, req.rawBody, this.apiKey)) {
      reqVerification.valid = false;
      reqVerification.errorCode = cmn.HTTP_CODE.UNAUTHORIZED;
      reqVerification.errorMsg = 'Invalid signature provided';
    }
    return reqVerification;
  }

  _logQuerySubmissionRequest(req) {
    req.log.info({reqBody: req.body});
  }

  _queryServiceMsgToHTTPCode(msg) {
    switch (msg) {
      case QUERY_SERVICE_MSG.UPDATE_IGNORED:
      case QUERY_SERVICE_MSG.UPDATE_SUCCESS:  return cmn.HTTP_CODE.SUCCESS;
      case QUERY_SERVICE_MSG.QUERY_NOT_FOUND: return cmn.HTTP_CODE.BAD_REQUEST;
      case QUERY_SERVICE_MSG.QUERY_COMPLETE:  return cmn.HTTP_CODE.GONE;
      default: throw new Error(`Unknown query service message: ${msg}`);
    }
  }
}

const _CUSTOM_HEADERS = Object.freeze({
  X_EVENT_SIG: 'x-event-signature'
});
