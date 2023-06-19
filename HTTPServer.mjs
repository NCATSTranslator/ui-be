'use strict'

import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pinoHttp } from 'pino-http';
import { default as cookieParser } from 'cookie-parser';

import * as cmn from './common.mjs';
import * as sso from './SocialSignOn.mjs';

export function startServer(config, service)
{
  console.log(config);
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  app.use(pinoHttp());
  app.use(express.json());
  app.use(cookieParser());

  app.use(express.static('./build'));
  const filters = {whitelistRx: /^ara-/}; // TODO: move to config

  app.all(['/api/*', '/admin/*', '/login'], loggahh);

  app.post(['/creative_query', '/api/creative_query'],
           logQuerySubmissionRequest,
           validateQuerySubmissionRequest,
           handleQuerySubmissionRequest(config, service));

  app.post(['/creative_status', '/api/creative_status'],
           validateQueryResultRequest,
           handleStatusRequest(config, service, filters));

  app.post(['/creative_result', '/api/creative_result'],
           validateQueryResultRequest,
           handleResultRequest(config, service, filters));

  app.get(['/config', '/admin/config'],
          handleConfigRequest(config));

  app.get('/oauth2/redir/:provider',
          sso.SSORedirectHandler(config));

  app.get('/login', function (req, res, next) {
    res.sendFile(path.join(__root, 'build', 'login.html'));
  });

  app.get('*', (req, res, next) =>
    {
      res.sendFile(path.join(__root, 'build/index.html'));
    });

  app.listen(8386);
}

function loggahh(req, res, next) {
  console.log("-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==-");
  console.log(`These be the headers for ${req.ip}: ${JSON.stringify(req.headers)}`);
  next();
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
      let retval = await service.outputAdapter.queryResultsToFE(svcRes,
        config.max_hops,
        config.ara_to_infores_map);
      res.status(200).json(retval);
    }
    catch (err)
    {
      logInternalServerError(req, err);
      sendInternalServerError(res);
    }
  }
}

function handleConfigRequest(config)
{
  return async function(req, res)
  {
    res.status(200).json(config.frontend);
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
