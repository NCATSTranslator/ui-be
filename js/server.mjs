'use strict'

import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pino } from 'pino-http';
import * as cmn from './common.mjs';

export function start(ars, trapi, config)
{
  console.log(config);
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  app.use(pino());
  app.use(express.json());
  app.use(express.static('../build'));
  app.post('/creative_query',  makeEndpoint(isValidQuery,
                                            trapi.diseaseToCreativeQuery,
                                            ars.postQuery,
                                            (diseaseCurie, qid) => { return qid; }));
  app.post('/creative_status', makeEndpoint(isValidQidObj,
                                            (queryReq) => { return queryReq.qid; },
                                            ars.pullQueryStatus,
                                            (qid, answers) => { return answers; }));
  app.post('/creative_result', makeEndpoint(isValidQidObj,
                                            (queryReq) => { return queryReq.qid; },
                                            ars.pullQueryAnswers,
                                            trapi.creativeAnswersToSummary));
  app.get('*', (req, res) =>
    {
      res.sendFile(path.join(__root, '../build/index.html'));
    });

  app.use(handleErrors);
  app.listen(8386);
}


function isValidQuery(query)
{
  return cmn.isObj && cmn.jsonHasKey(query, 'disease');
}

function isValidQidObj(qidObj)
{
  return cmn.isObj && cmn.jsonHasKey(qidObj, 'qid');
}

function makeEndpoint(isQueryReqValid, processQueryReq, pullProc, processQueryData)
{
  return async function(req, res, next)
  {
    try
    {
      const queryReq = req.body;
      if (!isQueryReqValid(queryReq))
      {
        throw new cmn.ClientError('Query is malformed');
      }

      const processedQueryReq = processQueryReq(queryReq);
      const queryState = await pullProc(processedQueryReq);
      if (!queryState)
      {
        throw new cmn.ServerError('ARS was unable to process the query');
      }

      res.json(makeResponse(queryState.status, processQueryData(processedQueryReq, queryState.data)));
    }
    catch (err)
    {
      next(err);
    }
  }
}

function handleErrors(err, req, res, next)
{
  res.setHeader('Content-Type', 'application/json');
  if (err.httpCode !== undefined)
  {
    res.status(err.httpCode);
  }
  else
  {
    res.status(500);
  }
  res.send(err.message);
}

function makeResponse(resStatus, resData)
{
  return {
    'status': resStatus,
    'data': resData
  };
}
