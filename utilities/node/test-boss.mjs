' use strict';

import meow from 'meow';
import path from 'path';
import * as cmn from '../../common.mjs';
import { ARSClient } from '../../ARSClient.mjs';
import { TranslatorServicexFEAdapter } from '../../TranslatorServicexFEAdapter.mjs';
import { TranslatorService } from '../../TranslatorService.mjs';

// Determine the program name
const programName = path.basename(process.argv[1]);
const permittedEnvs = ['dev', 'test', 'ci', 'production'];
const LOG_LEVEL_INFO = 'info';
const LOG_LEVEL_WARN = 'warning';
const LOG_LEVEL_ERROR = 'error';

const cli = meow(`
    Usage
      $ node ${programName} --input-file <input-file> --submission-delay <delay> --poll-delay <delay>

    Options
      --input-file, -i  Path to the input file
      --config, -c  Path to the config file
      --submission-delay, -s  Delay in seconds between submitting the initial query
      --poll-delay, -p  Delay between polling the ARS for a given PK
      --env, -e Environment ${permittedEnvs}
      --max-iter, -n  Max num. times to loop on each query (do not specify for infinite)
      --max-minutes, -t  Max num. of minutes to spend on each query
    Examples
      $ node ${programName} --input-file ./data.txt --submission-delay 30 --poll-delay 45
`, {
    importMeta: import.meta,
    flags: {
      inputFile: {
          type: 'string',
          shortFlag: 'i',
          isRequired: true
      },/*
      config: {
        type: 'string',
        shortFlag: 'c',
        isRequired: (flags, input) => {
            if (!flags.config) {
                throw new Error('The --config option is required.');
            }
            return true;
        }
      },*/
      submissionDelay: {
          type: 'number',
          shortFlag: 's',
          isRequired: true,
          default: 40
      },
      pollDelay: {
        type: 'number',
        shortFlag: 'p',
        isRequired: true,
        default: 10
      },
      env: {
        type: 'string',
        choices: permittedEnvs,
        shortFlag: 'e',
        isRequired: true
      },
      maxIter: {
        type: 'number',
        shortFlag: 'n',
        isRequired: false,
        default: -1,
      },
      maxMinutes: {
        type: 'number',
        shortFlag: 'm',
        default: 5,
        isRequired: true
      },
  }
});

console.log(cli.flags);

let input;
let client;

class MyARSClient {
  constructor(service, baseClient) {
    this.service = service;
    this.baseClient = baseClient;
    this.filters = {whitelistRx: /^ara-/};
  }

  async postQuery(qdata) {
    let query = this.service.inputToQuery(qdata);
    let resp;
    try {
      resp = await this.service.submitQuery(query);
      //throw new Error({this: 'went south'});
    } catch (err) {
      err.qdata = qdata;
      err.querygraph = query;
      throw err;
    }
    return resp.pk;
  }

  async getResults(pk) {
    let res;
    try {
      res = await this.baseClient.collectAllResults(pk, this.filters, true);
      //throw new Error({we: "got problems"});
    } catch (err) {
      err.pk = pk;
      throw err;
    }
    return res;
  }
}
await (async function (opts) {
  const configRoot = '../../configurations';
  const outputAdapter = new TranslatorServicexFEAdapter(null);
  const config = await cmn.readJson(`${configRoot}/${opts.env}.json`);
  let baseClient = new ARSClient(`https://${config.ars_endpoint.host}`,
                              config.ars_endpoint.pull_uri,
                              config.ars_endpoint.post_uri, [200, 206]);
  let service = new TranslatorService(baseClient, outputAdapter);
  console.log(service);

  input = await cmn.readJson(opts.inputFile);
  console.log(input);
  client = new MyARSClient(service, baseClient);
})(cli.flags);

function summarizeResponse(queryData, responseData,       logger, env) {

  function summarize_query_data(qdata) {
    return {
      qname: qdata.name,
      qcurie: qdata.curie,
      qtype: qdata.type,
      qdirection: qdata.direction ? qdata.direction : 'none'
    }
    //return `${qdata.name}-${qdata.curie}-${qdata.type}` + (qdata.direction ? `-${qdata.direction}` : '');
  }
  function summarize_completed(res) {
    let retval = res.map((e) => {
      if (e?.data?.results && Array.isArray(e.data.results)) {
        return { agent: e.agent, nres: e.data.results.length,
          uuid: e.uuid, fetch_ms: e.meta.fetchMs, parse_ms: e.meta.parseMs};
        } else {
          console.log('hello??');
          logger(LOG_LEVEL_ERROR, env, `Error summarizing completed response from ${e.agent} for ${e.uuid}`,
            {agent: e.agent, data: e.data, fetch_ms: e.meta.fetchMs, parse_ms: e.meta.parseMs});
          return { agent: e.agent, nres: 0,
            uuid: e.uuid, fetch_ms: e.meta.fetchMs, parse_ms: e.meta.parseMs};
        }
    });
    return retval;
  }

  function summarize_non_completed(res) {
    let retval = res.map((e) => {
      return { agent: e.agent, status: e.code, uuid: e.uuid};
    });
    return retval;
  }

  function summarize_perf(res) {
    let retval = {
      fetch_ms: res.meta.fetchMs,
      parse_ms: res.meta.parseMs,
      children_processing_ms: res.meta.childrenProcessingMs
    };
    return retval;
  }

  if (!responseData || !responseData.hasOwnProperty('pk')) {
    console.log('HOLY SHIT NO PK');
    console.log(responseData);
    logger(LOG_LEVEL_ERROR, env, `NO PK found: ${JSON.stringify(responseData)}`, responseData);
    console.log("WOW");
  }

  const completed_summary = summarize_completed(responseData.completed);
  const errored_summary = summarize_non_completed(responseData.errored);
  const running_summary = summarize_non_completed(responseData.running);
  const qdata_summary = summarize_query_data(queryData);
  const perf_summary = summarize_perf(responseData);
  const retval = {
    pk: responseData.pk,
    queuing: responseData.queuing,
    ...qdata_summary,
    n_completed: completed_summary.length,
    n_running: running_summary.length,
    n_errored: errored_summary.length,
    completed: completed_summary,
    running: running_summary,
    errored: errored_summary,
    ...perf_summary
  };
  return retval;
}

function checkTerminated(res) {
  return !res.queuing && res.running.length === 0;
}

async function biggy(client, qdata, logger, pk, params) {

  if (!pk) {
    //console.log('null pk detected -- submitting query');
    params.startTime = new Date();
    try {
      pk = await client.postQuery(qdata);
    } catch (err) {
      console.error(`Oh crap - error submitting query: ${JSON.stringify(err)}`);
      logger(LOG_LEVEL_ERROR, params.env, JSON.stringify(err));
      setTimeout(biggy, params.submissionDelay * 1000, client, qdata, logger, null, params);
      return;
    }
  }

  let {pollDelay, startTime, maxMinutes, maxIter, env, submissionDelay, iter} = params;

  let resp;
  try {
    //console.log(pk);
    resp = await client.getResults(pk);
  } catch (err) {
    console.error(`Oh crap - error getting results: ${JSON.stringify(err)}`);
    /* Arguably the right thing to do here is still respect whether or not the time out
     * has occurred when resubmitting, but I'm not willing to deal with the complications
     * that this would cause in this function's code. */
    logger(LOG_LEVEL_ERROR, params.env, JSON.stringify(err));
    setTimeout(biggy, submissionDelay * 1000, client, qdata, logger, pk, params);
    return;
  }

  let respSummary;
  try {
    respSummary = summarizeResponse(qdata, resp, logger, env);
  } catch (err) {
    console.error(`YO: ${err}`);
    // Rare but possible case of a completely unexpected exception when parsing responses
    // Restart the query - not many good options here
    logger(LOG_LEVEL_ERROR, env, `Error creating summary for ${pk}: ${JSON.stringify(err)}.`, err);
    params.startTime = new Date();
    setTimeout(biggy, submissionDelay * 1000, client, qdata, logger, null, params);
    return;
  }
  let terminated = checkTerminated(resp);
  let timeExceeded = ((new Date() - startTime) / 1000) / 60 > maxMinutes;
  let displayIter = iter + 1;
  let msg;
  let level;
  let infoStr = `${pk} ${qdata.name} - ${qdata.type} - dir:${qdata.direction} [${qdata.curie}]`;

  if (!terminated && !timeExceeded) {
    if (respSummary.queuing) {
      msg = 'Query was queued';
      level = LOG_LEVEL_WARN;
    } else {
      msg = 'Query progressing'
      level = LOG_LEVEL_INFO;
    }
    msg = `${msg}: ${infoStr}`;
    logger(level, env, msg, respSummary, startTime, maxMinutes, displayIter, maxIter);
    setTimeout(biggy, pollDelay * 1000, client, qdata, logger, pk, params);
  } else {
    if (timeExceeded) {
      msg = `Query did not complete within ${maxMinutes} min.`;
      level = LOG_LEVEL_WARN;
    } else {
      msg = 'Query terminated successfully';
      level = LOG_LEVEL_INFO;
    }
    msg = `${msg}: ${infoStr}`;
    logger(level, env, msg, respSummary, startTime, maxMinutes, displayIter, maxIter);
    params.iter += 1;
    if (maxIter === -1 || params.iter < maxIter) {
      params.startTime = new Date();
      pk = null;
      console.log("legit restart");
      setTimeout(biggy, submissionDelay * 1000, client, qdata, logger, pk, params);
    } else {
      // DO NOTHING; DIE
    }
  }
}


function jsonLogger(logLevel, env, msg, respSummary={}, startTime=null, maxMinutes=null,
  iter=null, maxIter=null) {
  respSummary.status = logLevel;
  respSummary.env = env;
  respSummary.msg = msg;

  if (logLevel === 'info') {
    respSummary.start_time = startTime;
    respSummary.log_time = new Date();
    respSummary.elapsed_sec = Math.round((respSummary.log_time - respSummary.start_time) / 1000);
    respSummary.max_minutes = maxMinutes;
    respSummary.iter = iter;
    respSummary.max_iter = maxIter;
  }
  console.log(JSON.stringify(respSummary));
}


function textLogger(respSummary, env, startTime, maxMinutes, iter, maxIter, msg) {
  console.log("-=-=-=-");
  let elapsedSec = (new Date() - startTime) / 1000;
  let min = Math.floor(elapsedSec / 60);
  let sec = (elapsedSec % 60).toFixed(2);
  console.log(`
  Start time: ${startTime}
  Time spent: ${min}m${sec}s/${maxMinutes} min.
  Iteration: ${iter}/${maxIter}
  Message: ${msg}
  Env: ${env}
  Details: ${JSON.stringify(respSummary, null, 2)}
  `);
}

// Begin program

let params = {
  pollDelay: cli.flags.pollDelay,
  maxMinutes: cli.flags.maxMinutes,
  maxIter: cli.flags.maxIter,
  env: cli.flags.env,
  submissionDelay: cli.flags.submissionDelay
}

for (let i = 0; i < input.length; i++) {
  // Make sure each function gets an independent copy of the params so that
  // updating startTime and iter doesn't update it for all running copies
  biggy(client, input[i], jsonLogger, null, {...params,
    startTime: new Date(), iter: 0});
  console.log(`starting big sleep ${i}`);
  await cmn.sleep(cli.flags.submissionDelay * 1000);
  console.log(`ending big sleep ${i}`);
}
/*
input.forEach(async (qdata) => {
  params.startTime = new Date();
  params.iter = 0;
  biggy(client, qdata, jsonLogger, null, params);
  await cmn.sleep(cli.flags.submissionDelay * 1000);
});*/
