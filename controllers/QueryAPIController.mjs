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
    if (this.config.ars_endpoint.use_pubsub) {
      this.submitQuery = this._submitQueryViaPubSub;
      this.getQueryStatus = this._getQueryStatusViaPubSub;
    } else {
      this.submitQuery = this._submitQueryViaPolling;
      this.getQueryStatus = this._getQueryStatusViaPolling;
    }
    this.submitQuery.bind(this);
    this.getQueryStatus.bind(this);
  }

  async getUserQueriesStatus(req, res, next) {
    return res.status(200).json(this._stub_queries_status());
  }

  _stub_queries_status() {
    return [
      {
        status: 'success',
        data: {
          qid: 'qryex1',
          aras: ['ara-1'],
          title: 'Example Query 1',
          bookmark_count: 10,
          note_count: 6,
          time_created: '1900-01-01 00:00:00.000000Z',
          time_updated: '1900-01-01 00:00:00.000000Z',
          deleted: false
        }
      },
      {
        status: 'running',
        data: {
          qid: 'qryex2',
          aras: ['ara-1', 'ara-2', 'ara-3'],
          title: 'Example Query 2',
          bookmark_count: 5,
          note_count: 5,
          time_created: '1901-01-01 00:00:00.000000Z',
          time_updated: '1901-01-01 00:00:00.000000Z',
          deleted: false
        }
      },
      {
        status: 'running',
        data: {
          qid: 'qryex3',
          aras: ['ara-1', 'ara-2', 'ara-3'],
          title: 'Example Query 3',
          bookmark_count: 3,
          note_count: 1,
          time_created: '1902-01-01 00:00:00.000000Z',
          time_updated: '1902-01-01 00:00:00.000000Z',
          deleted: true
        }
      },
      {
        status: 'error',
        data: {
          qid: 'qryex4',
          aras: [],
          title: 'Example Query 4',
          bookmark_count: 0,
          note_count: 0,
          time_created: '1903-01-01 00:00:00.000000Z',
          time_updated: '1903-01-01 00:00:00.000000Z',
          deleted: false
        }
      }
    ];
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
    return wutil.sendError(res, cmn.HTTP_CODE.NOT_IMPLEMENTED, 'Not implemented');
  }

  async restoreUserQueries(req, res, next) {
    return wutil.sendError(res, cmn.HTTP_CODE.NOT_IMPLEMENTED, 'Not implemented');
  }

  async _submitQueryViaPubSub(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Malformed request');
    }
    try {
      const trapiQuery = this.translatorService.inputToQuery(req.body);
      req.log.info({query: trapiQuery});
      const submitResp = await this.translatorService.submitQuery(trapiQuery);
      req.log.info({arsqueryresp: submitResp});
      const pk = trapi.getPk(submitResp);
      if (!pk) throw new Error(`ARS query submission response has no PK: ${submitResp}`);
      const queryModel = await this.queryService.createQuery(pk, req.body);
      if (!queryModel) throw new Error(`Failed to create query with PK: ${pk}`);
      //TODO: verify subscribe response
      const subscribeResp = await this.translatorService.subscribeQuery(pk);
      const uid = req.sessionData.user.id;
      const userQueryModel = await this.userService.createUserQuery(uid, queryModel);
      if (!userQueryModel) throw new Error(`User service failed to create entry for query ${queryModel.id} and user ${uid}`);
      const isTransactionComplete = this.queryService.addQueryUserRelationship(queryModel, userQueryModel);
      if (!isTransactionComplete) throw new Error(`Query service failed to associate query ${queryModel.id} with user save ${userQueryModel.id}`);
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

  async _submitQueryViaPolling(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed request');
    }
    try {
      let query = this.translatorService.inputToQuery(req.body);
      let submitResp = await this.translatorService.submitQuery(query);
      req.log.info({ltype: 'query-submission', query_params: req.body, ars_response: submitResp}, 'Query submission and response');
      return res.status(200).json(this.translatorServicexFEAdapter.querySubmitToFE(submitResp));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
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
