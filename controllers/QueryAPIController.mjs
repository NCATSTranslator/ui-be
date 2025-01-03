'use strict';

import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';
import { logger } from "../lib/logger.mjs";

export { QueryAPIController };

class QueryAPIController {
  constructor(config, translatorService, filters) {
    this.config = config;
    this.translatorService = translatorService;
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
      let query = this.translatorService.inputToQuery(req.body);
      let resp = await this.translatorService.submitQuery(query);
      req.log.info({ltype: 'query-submission', query_params: req.body, ars_response: resp}, 'Query submission and response');
      return res.status(200).json(this.translatorService.outputAdapter.querySubmitToFE(resp));
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
      logger.debug({ltype: 'query-status from service', statusRes: statusRes});
      let retval = this.translatorService.outputAdapter.queryStatusToFE(statusRes)
      logger.debug({ltype: 'query-status after adapter', value: retval});
      return res.status(200).json(retval);
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
