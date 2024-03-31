'use strict';

import { default as express } from 'express';
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';

export { QueryAPIController };

class QueryAPIController {
  constructor(config, translatorService, filters) {
    this.config = config;
    this.translatorService = translatorService;
    this.filters = filters;
  }

  isValidQuerySubmissionRequest(body) {
    return cmn.isObj(body);
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
      req.log.info({query: query});
      let resp = await this.translatorService.submitQuery(query);
      req.log.info({arsqueryresp: resp});
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

/*
function queryAPIController(config, translatorService) {
  let router = express.Router();

  router.post('/', function(req, res, next) {
    logQuerySubmissionRequest,
    validateQuerySubmissionRequest,
    handleQuerySubmissionRequest(config, translatorService);
  });

  // All methods other than POST^^ to / are forbidden
  router.all('/', function(req, res, next) {
    return res.status(403).send('Forbidden');
  });

  router.all('/:qid', function(req, res, next) {
    return res.status(403).send('Forbidden');
  });

  router.get('/:qid/status',
    validateQueryResultRequest,
    handleStatusRequest(config, translatorService, config.filters));

  router.get('/:qid/result',
    validateQueryResultRequest,
    handleResultRequest(config, translatorService, config.filters));

  return router;
}

function logQuerySubmissionRequest(req, res, next) {
  req.log.info({reqBody: req.body});
  next();
}

function validateQuerySubmissionRequest(req, res, next) {
  let query = req.body;
  if (cmn.isObj(query)) {
    next();
  }
  else {
    return wutil.sendError(res, 400, "Malformed request");
  }
}

function handleQuerySubmissionRequest(config, service) {
  return async function(req, res, next) {
    try {
      let query = service.inputToQuery(req.body);
      req.log.info({query: query});
      let resp = await service.submitQuery(query);
      req.log.info({arsqueryresp: resp});
      return res.status(200).json(service.outputAdapter.querySubmitToFE(resp));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }
}

function validateQueryResultRequest(req, res, next) {
  if (req.params.qid) {
    next();
  } else {
    return wutil.sendError(res, 400, "No query id specified in request");
  }
}

function handleStatusRequest(config, service, filters) {
  return async function(req, res, next) {
    try {
      let uuid = req.params.qid;
      let statusRes = await service.getQueryStatus(uuid, filters);
      return res.status(200).json(service.outputAdapter.queryStatusToFE(statusRes));
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }
}

function handleResultRequest(config, service, filters) {
  return async function(req, res, next) {
    try {
      let uuid = req.params.qid;
      let svcRes = await service.getResults(uuid, filters);
      let retval = await service.outputAdapter.queryResultsToFE(svcRes,
        config.max_hops,
        config.ara_to_infores_map);
      return res.status(200).json(retval);
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res, err);
    }
  }
}
*/
