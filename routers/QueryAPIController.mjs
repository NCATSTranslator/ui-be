'use strict';

import { default as express } from 'express';
import * as cmn from '../lib/common.mjs';
import * as wutil from '../lib/webutils.mjs';

export { createQueryController };

function createQueryController(config, services, isDemo) {
  let router = express.Router();
  const translatorService = services.translatorService;
  const demoQueries = config.frontend;

  router.post('/',
    function(req, res, next) {
      if (isDemo) {
        return res.status(403).send('Forbidden');
      } else {
        next();
      }
    },
    logQuerySubmissionRequest,
    validateQuerySubmissionRequest,
    handleQuerySubmissionRequest(config, translatorService));

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
