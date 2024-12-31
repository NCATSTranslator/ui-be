'use strict';
export { QueryAPIController };
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as trapi from '../lib/trapi.mjs';

class QueryAPIController {
  constructor(config, translatorService, queryService, filters) {
    this.config = config;
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
      if (!pk) {
        throw new Error(`ARS submission response has no PK: ${arsResp}`);
      }
      const storeQueryModel = await this.queryService.createQuery(pk, req.body);
      return res.status(200).json(this.queryService.outputAdapter.querySubmitToFE(storeQueryModel));
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
      const storeQueryModel = await this.queryService.getQueryStatus(uuid);
      return res.status(200).json(this.queryService.outputAdapter.queryStatusToFE(storeQueryModel));
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
      let uuid = req.params.qid;
      let svcRes = await this.translatorService.getResults(uuid, this.filters);
      let retval = await this.translatorService.outputAdapter.queryResultsToFE(
        svcRes, this.config.max_hops, this.config.ara_to_infores_map);
      return res.status(200).json(retval);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async updateQuery(req, res, next) {
    if (!this._isValidQueryUpdateRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed Request');
    }
    try {
      const resCode = await this.queryService.processQueryUpdate(req.body);
      return res.status(resCode).send();
    } catch (err) {
      // TODO: Send errors at more granular level
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
    // TODO: Fill in stub
    return true;
  }

  _logQuerySubmissionRequest(req) {
    req.log.info({reqBody: req.body});
  }
}
