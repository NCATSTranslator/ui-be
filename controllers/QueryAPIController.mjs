'use strict';

import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as trapi from '../lib/trapi.mjs';
import { Query } from '../models/Query.mjs';

export { QueryAPIController };

class QueryAPIController {
  constructor(config, translatorService, queryService, filters) {
    this.config = config;
    this.translatorService = translatorService;
    this.queryService = queryService;
    this.filters = filters;
  }

  isValidQuerySubmissionRequest(body) {
    return cmn.isObject(body);
  }

  isValidQueryResultRequest(req) {
    return req.params.hasOwnProperty('qid') && req.params.qid;
  }

  logQuerySubmissionRequest(req) {
    req.log.info({reqBody: req.body});
  }

  async submitQuery(req, res, next) {
    this.logQuerySubmissionRequest(req);
    if (!this.isValidQuerySubmissionRequest(req.body)) {
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
      let queryModel = new Query({
        pk: pk,
        metadata: { query: req.body }
      });
      queryModel = await this.queryService.createQuery(queryModel);
      return res.status(200).json(this.translatorService.outputAdapter.querySubmitToFE(arsResp));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async getQueryStatus(req, res, next) {
    if (!this.isValidQueryResultRequest(req)) {
      return wutil.sendError(res, 400, 'Malformed Request');
    }
    try {
      let uuid = req.params.qid;
      let statusRes = await this.translatorService.getQueryStatus(uuid, this.filters);
      return res.status(200).json(this.translatorService.outputAdapter.queryStatusToFE(statusRes));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }

  async getQueryResult(req, res, next) {
    if (!this.isValidQueryResultRequest(req)) {
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
}
