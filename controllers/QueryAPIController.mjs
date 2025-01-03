'use strict';
export { QueryAPIController };
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as trapi from '../lib/trapi.mjs';

class QueryAPIController {
  constructor(config, translatorService, queryService, filters) {
    this.config = config;
    this.apiKey = config.secrets.hmac.key;
    this.translatorService = translatorService;
    this.queryService = queryService;
    this.filters = filters;
  }

  async submitQuery(req, res, next) {
    this._logQuerySubmissionRequest(req);
    if (!this._isValidQuerySubmissionRequest(req.body)) {
      return wutil.sendError(res, 400, 'Malformed request');
    }
    try {
      const trapiQuery = this.translatorService.inputToQuery(req.body);
      req.log.info({query: trapiQuery});
      const arsResp = await this.translatorService.submitQuery(trapiQuery);
      req.log.info({arsqueryresp: arsResp});
      const pk = trapi.getPk(arsResp);
      if (!pk) throw new Error(`ARS query submission response has no PK: ${arsResp}`);
      const storeQueryModel = await this.queryService.createQuery(pk, req.body);
      return res.status(200).json(this.queryService.feAdapter.querySubmitToFE(storeQueryModel));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async getQueryStatus(req, res, next) {
    if (!this._isValidQueryResultRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      const storeQueryModel = await this.queryService.getQueryByPk(uuid);
      const status = this.queryService.feAdapter.queryStatusToFE(storeQueryModel);
      return res.status(200).json(status);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async getQueryResult(req, res, next) {
    if (!this._isValidQueryResultRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed Request');
    }
    try {
      const uuid = req.params.qid;
      let results = await this.translatorService.getResults(uuid, this.filters);
      results = await this.translatorService.feAdapter.queryResultsToFE(
          results, this.config.max_hops, this.config.ara_to_infores_map);
      return res.status(200).json(results);
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
      const resCode = await this.queryService.processQueryUpdate(update);
      res.set(_HEADERS.X_EVENT_SIG, cmn.generateHMACSignature(JSON.stringify(res.body), this.apiKey));
      return res.status(resCode).send();
    } catch (err) {
      // TODO: Send errors at more granular level
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  _isValidQuerySubmissionRequest(body) {
    return cmn.isObject(body);
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
    const signature = req.headers[_HEADERS.X_EVENT_SIG];
    if (!signature) {
      reqVerification.valid = false;
      reqVerification.errorCode = 400;
      reqVerification.errorMsg = 'Signature not provided';
    } else if (!cmn.verifyHMACSignature(signature, JSON.stringify(req.body), this.apiKey)) {
      reqVerification.valid = false;
      reqVerification.errorCode = 401;
      reqVerification.errorMsg = 'Invalid signature provided';
    }
    return reqVerification;
  }

  _logQuerySubmissionRequest(req) {
    req.log.info({reqBody: req.body});
  }
}

const _HEADERS = Object.freeze({
  X_EVENT_SIG: 'x-event-signature'
});
