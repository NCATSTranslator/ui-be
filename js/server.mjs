import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pino } from 'pino-http';
import { SERVER_CONFIG } from './config.mjs';
import * as cmn from './common.mjs';
import * as ars from './ars.mjs';
import * as trapi from './trapi.mjs';

const __root = path.dirname(url.fileURLToPath(import.meta.url));
const app = express();

console.log(SERVER_CONFIG);
app.use(pino());
app.use(express.json());
app.use(express.static('../build'));

app.post('/creative_query', async (req, res, next) =>
  {
    const query = req.body;
    try
    {
      if (!isValidQuery(query))
      {
        throw new ClientError('Query is malformed');
      }

      const arsResp = await ars.postQuery(trapi.diseaseToCreativeQuery(query));
      if (!arsResp.qStatus)
      {
        throw new ServerError('ARS was unable to process query');
      }

      res.json(makeResponse('success', arsResp.qid));
    }
    catch (err)
    {
      next(err);
    }
  });

app.post('/creative_status', makeResultEndpoint(ars.pullQueryStatus, (qid, answers) => { return answers; }));
app.post('/creative_result', makeResultEndpoint(ars.pullQueryAnswers, trapi.creativeAnswersToSummary));

app.get('*', (req, res) =>
  {
    res.sendFile(path.join(__root, '../build/index.html'));
  });

app.use(handleErrors);
app.listen(8386);

class ApplicationError extends Error {
  constructor(message, httpCode) {
    super(message);
    this.name = this.constructor.name;
    this.httpCode = httpCode;
  }
}

class ClientError extends ApplicationError {
  constructor(message) {
    super(message, 400);
  }
}

class ServerError extends ApplicationError {
  constructor(message) {
    super(message, 500);
  }
}

function isValidQuery(query)
{
  return cmn.isObj && cmn.jsonHasKey(query, 'disease');
}

function isValidQidObj(qidObj)
{
  return cmn.isObj && cmn.jsonHasKey(qidObj, 'qid');
}

function makeResponse(resStatus, resData)
{
  return {
    'status': resStatus,
    'data': resData
  };
}

function makeResultEndpoint(pullProc, processQueryData)
{
  return async function(req, res, next)
  {
    const qidObj = req.body;
    try
    {
      if (!isValidQidObj(qidObj))
      {
        throw new ClientError('Query is malformed');
      }

      const qid = qidObj.qid;
      const queryState = await pullProc(qid);
      if (!queryState)
      {
        throw new ServerError('ARS was unable to process query');
      }

      res.json(makeResponse(queryState.status, processQueryData(qid, queryState.data)));
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
