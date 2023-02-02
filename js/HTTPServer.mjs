'use strict'

import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pinoHttp } from 'pino-http';
import * as cmn from './common.mjs';

export function startServer(config, service)
{
  console.log(config);
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  app.use(pinoHttp());
  app.use(express.json());
  app.use(express.static('../build'));
  const filters = {whitelistRx: /^ara-/}; // TODO: move to config

  app.post('/creative_query',
           logQuerySubmissionRequest,
           validateQuerySubmissionRequest,
           handleQuerySubmissionRequest(config, service));

  app.post('/creative_status',
           validateQueryResultRequest,
           handleStatusRequest(config, service, filters));

  app.post('/creative_result',
           validateQueryResultRequest,
           handleResultRequest(config, service, filters));

  app.get('*', (req, res, next) =>
    {
      res.sendFile(path.join(__root, '../build/index.html'));
    });

  app.listen(8386);
}

function logQuerySubmissionRequest(req, res, next)
{
  req.log.info({reqBody: req.body});
  next();
}

function validateQuerySubmissionRequest(req, res, next)
{
  let query = req.body;
  if (cmn.isObj(query))
  {
    next();
  }
  else
  {
    sendError(res, 400, "No disease specificed in request");
  }
}

function handleQuerySubmissionRequest(config, service)
{
  return async function(req, res, next)
  {
    try
    {
      let query = service.inputToQuery(req.body);
      req.log.info({query: query});
      let resp = await service.submitQuery(query);
      req.log.info({arsqueryresp: resp});
      res.status(200).json(service.outputAdapter.querySubmitToFE(resp));
    }
    catch (err)
    {
      logInternalServerError(req, err);
      sendInternalServerError(res);
    }
  }
}

function validateQueryResultRequest(req, res, next)
{
  let requestObj = req.body;
  if (cmn.isObj(requestObj)
    && requestObj.hasOwnProperty('qid')
    && requestObj.qid.length > 0)
  {
    next();
  }
  else
  {
    sendError(res, 400, "No query id specificed in request");
  }
}

function handleStatusRequest(config, service, filters)
{
  return async function(req, res, next)
  {
    try
    {
      let uuid = req.body.qid;
      let statusRes = await service.getQueryStatus(uuid, filters);
      res.status(200).json(service.outputAdapter.queryStatusToFE(statusRes));
    }
    catch (err)
    {
      logInternalServerError(req, err);
      sendInternalServerError(res);
    }
  }
}

function handleResultRequest(config, service, filters)
{
  return async function(req, res, next)
  {
    try
    {
      let uuid = req.body.qid;
      let svcRes = await service.getResults(uuid, filters);
      let retval = service.outputAdapter.queryResultsToFE(svcRes, config.max_hops);
      res.status(200).json(retval);
    }
    catch (err)
    {
      logInternalServerError(req, err);
      sendInternalServerError(res);
    }
  }
}

function sendError(res, errorCode, message)
{
  const response = {
    'status': 'error',
    'data': message
  }

  res.status(errorCode).json(response);
}

function sendInternalServerError(res)
{
  sendError(res, 500, 'Internal Server Error');
}

function logInternalServerError(req, err)
{
  req.log.error(`Internal Server Error: ${err}`);
}
