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
    this.translatorService = translatorService;
    this.translatorServicexFEAdapter = translatorServicexFEAdapter;
    this.queryService = queryService;
    this.queryServicexFEAdapter = queryServicexFEAdapter;
    this.userService = userService;
    this.filters = filters;
    this.updateModel = config.ars_endpoint.use_pubsub ? UPDATE_MODEL.PUBSUB : UPDATE_MODEL.POLLING;
    switch (this.updateModel) {
      case UPDATE_MODEL.PUBSUB:
        this.getQueryStatus = this._getQueryStatusViaPubSub;
        break;
      case UPDATE_MODEL.POLLING:
        this.getQueryStatus = this._getQueryStatusViaPolling;
        break;
      default:
        throw Error(`Unknown update model for QueryAPIController: ${this.updateModel}`);
    }
  }

  async getUserQueries(req, res, next) {
    const user_id = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    try {
      const query_status_data = await this.userService.get_queries_status(user_id, include_deleted);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(query_status_data);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to get queries status for the current user: ${user_id}`);
      return wutil.sendInternalServerError(res, 'Failed to get queries status for the current user');
    }
  }

  async updateUserQueries(req, res, next) {
    throw new Error('updateUserQueries is not implemented');
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
      res.set(_CUSTOM_HEADERS.X_EVENT_SIG, cmn.generateHMACSignature(JSON.stringify(res.body), this.apiKey));
      return res.status(this._queryServiceMsgToHTTPCode(queryServiceMsg)).send();
    } catch (err) {
      // TODO: Send errors at more granular level
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async deleteUserQueries(req, res, next) {
    const query_ids = await req.body;
    if (!cmn.isArray(query_ids)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const user_id = req.sessionData.user.id;
    let queries = null;
    try {
      queries = await this.userService.deleteUserSaveBatch(user_id, query_ids);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(queries);
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
    const user_id = req.sessionData.user.id;
    let queries = null;
    try {
      queries = await this.userService.restoreUserSaveBatch(user_id, query_ids);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(queries);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to update queries from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to update queries from the database');
    }
  }

  async submitQuery(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const queryRequest = req.body;
      const pid = queryRequest.pid;
      let project = null;
      if (pid) {
        project = (await this.userService.getUserSavesBy(uid, {id: pid}))[0];
        if (!project) {
          throw new Error(`Submitted query includes unknown PID: ${pid}`);
        }
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
      if (this.updateModel === UPDATE_MODEL.PUBSUB) {
        const subscribeResp = await this.translatorService.subscribeQuery(pk);
        if (!subscribeResp) {
          throw new Error(`Failed to subscribe to query: ${pk}`);
        }
      }
      const uid = req.sessionData.user.id;
      const userQueryModel = await this.userService.createUserQuery(uid, queryModel.metadata.query);
      if (!userQueryModel) throw new Error(`User service failed to create entry for query ${queryModel.id} and user ${uid}`);
      const isUserAssignedQuery = this.queryService.addQueryUserRelationship(queryModel, userQueryModel);
      if (!isUserAssignedQUery) throw new Error(`Query service failed to associate query ${queryModel.id} with user save ${userQueryModel.id}`);
      if (pid) {
        project.data.pks.push(pk);
        const updatedProject = await this.userService.updateUserSavePartial(project);
        if (!updatedProject) {
          throw new Error(`Error updating project: ${pid} with PK: ${pk}`);
        }
      }
      return res.status(200).json(this.queryServicexFEAdapter.querySubmitToFE(queryModel));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
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

const UPDATE_MODEL = Object.freeze({
  PUBSUB: 'pubsub',
  POLLING: 'polling'
});
