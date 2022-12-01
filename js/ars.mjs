'use strict'

import { URL } from 'node:url';
import { SERVER_CONFIG } from './config.mjs';
import * as cmn from './common.mjs';
import { makeMetadataObject } from './trapi.mjs';

export async function postQuery(query)
{
  return parseSubmitQueryResp(await arsPost(arsPostUrl, JSON.stringify(query)));
}

export async function pullQueryStatus(qid)
{
  return parseQueryMetadata(await arsGet(arsPullUrl(qid, true)));
}

export async function pullQueryAnswers(qid)
{
  return await parseQueryAnswers(await arsGet(arsPullUrl(qid, true)));
}

const arsBase = `https://${SERVER_CONFIG.ars_endpoint.host}`;
const arsPostUrl = new URL(SERVER_CONFIG.ars_endpoint.post_uri, arsBase);
function arsPullUrl(qid, doTrace)
{
  const pullUrl = new URL(`${SERVER_CONFIG.ars_endpoint.pull_uri}/${qid}`, arsBase);
  if (!!doTrace)
  {
    pullUrl.searchParams.append('trace', 'y');
  }

  return pullUrl;
}

async function arsSendRecv(url, options)
{
  const resp = await fetch(url, options);
  if (resp.ok)
  {
    return await resp.json();
  }
  else
  {
    throw new Error(`The ARS failed to respond to our request. Got code ${resp.status}`);
  }
}

async function arsGet(url)
{
  const options = {
    'method': 'GET',
  };

  return await arsSendRecv(url, options);
}

async function arsPost(url, data)
{
  const options = {
    'method': 'POST',
    'body': data
  };

  return await arsSendRecv(url, options);
}

function getAnswer(json)
{
  return cmn.jsonGet(json, 'results');
}

function getMessage(json)
{
  return cmn.jsonGet(json, 'message');
}

function getFields(json)
{
  return cmn.jsonGet(json, 'fields');
}

function getStatus(json)
{
  return cmn.jsonGet(json, 'status');
}

function getCode(json)
{
  return cmn.jsonGet(json, 'code');
}

function getData(json)
{
  return cmn.jsonGet(json, 'data');
}

function getChildren(json)
{
  return cmn.jsonGet(json, 'children', []);
}

function getQid(json)
{
  return cmn.jsonGet(json, 'pk');
}

function getAgent(json)
{
  return cmn.jsonGetFromKpath(json, ['actor', 'agent']);
}

function isQueryDone(qStatus)
{
  return qStatus === 'done';
}

function isAra(agent)
{
  return agent.startsWith('ara');
}

function isRespOk(resp)
{
  const rStatus = getStatus(resp);
  return rStatus === 'Done' || rStatus === 'Running';
}

function rStatusToStatus(rStatus)
{
  switch(rStatus)
  {
    case 'Done':
      return 'success';
    case 'Running':
      return 'running'
  }

  throw new Error(`Query status not recognized. Got ${rStatus}`);
}

function makeQueryState(status, data)
{
  return {
    'status': status,
    'data': data
  };
}

function makeAnswer(message, agent)
{
  return {
    'message': message,
    'agent': agent
  };
}

function parseQueryStatus(resp)
{
  const jsonStatus = {
    'code': getCode(resp),
    'rStatus': getStatus(resp)
  };

  if (jsonStatus.code === 200 && jsonStatus.rStatus === 'Done')
  {
    return 'done';
  }
  else if (jsonStatus.code === 202 && jsonStatus.rStatus === 'Running')
  {
    return 'running';
  }
  else
  {
    return false;
  }
}

function parseQueryMetadata(resp)
{
  if (!isRespOk(resp))
  {
    return false;
  }

  const actors = getChildren(resp);
  const finishedActors = [];
  actors.forEach((actor) =>
    {
      let agent = getAgent(actor);
      let actorStatus = parseQueryStatus(actor);
      if (isAra(agent) && isQueryDone(actorStatus))
      {
        finishedActors.push(agent);
      }
    });

  const araStatus = rStatusToStatus(getStatus(resp));
  const qid = getMessage(resp);

  return makeQueryState(araStatus, makeMetadataObject(qid, finishedActors));
}

function parseSubmitQueryResp(resp)
{
  const fields = getFields(resp);
  const qStatus = parseQueryStatus(fields);
  const qid = getQid(resp);

  return {
    'qStatus': qStatus,
    'qid': qid
  };
}

async function parseQueryAnswers(resp)
{
  async function pullQueryActorAnswer(qid)
  {
    return parseQueryActorAnswer(await arsGet(arsPullUrl(qid, false)));
  }

  if (!isRespOk(resp))
  {
    return false;
  }

  const actors = getChildren(resp);
  let actorsData = [];
  for (const actor of actors)
  {
    let agent = getAgent(actor);
    let actorStatus = parseQueryStatus(actor);
    if (isAra(agent) && isQueryDone(actorStatus))
    {
      let actorMessage = await pullQueryActorAnswer(getMessage(actor));
      if (actorMessage)
      {
        actorsData.push(makeAnswer(actorMessage, agent));
      }
    }
  }

  const araStatus = rStatusToStatus(getStatus(resp));
  return makeQueryState(araStatus, actorsData);
}

function parseQueryActorAnswer(resp)
{
  try
  {
    const fields = getFields(resp);
    const qStatus = parseQueryStatus(fields);
    if (isQueryDone(qStatus))
    {
      const message = getMessage(getData(fields));
      const answer = getAnswer(message);
      if (!!answer && !(cmn.isArrayEmpty(answer)))
      {
        return message;
      }

      return false;
    }
  }
  catch(err)
  {
    return false; // Some ARAs report done when not done
  }
}
