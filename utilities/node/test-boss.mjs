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
const cli = meow(`
    Usage
      $ node ${programName} --input-file <input-file> --submission-delay <delay> --poll-delay <delay>

    Options
      --input-file, -i  Path to the input file
      --config, -c  Path to the config file
      --submission-delay, -s  Delay in seconds between submitting the same query again after a complete run
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
          isRequired: true
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
//throw new Error('bye');
/*
console.log(`Input File: ${cli.flags.inputFile}`);
console.log(`Submission Delay: ${cli.flags.submissionDelay} seconds`);
console.log(`Poll Delay: ${cli.flags.pollDelay} seconds`);
*/

let service;
let input;
let client;
const filters = {whitelistRx: /^ara-/};

await (async function (opts) {
  const configRoot = '../../configurations';
  const outputAdapter = new TranslatorServicexFEAdapter(null);
  const config = await cmn.readJson(`${configRoot}/${opts.env}.json`);
  client = new ARSClient(`https://${config.ars_endpoint.host}`,
                              config.ars_endpoint.pull_uri,
                              config.ars_endpoint.post_uri, [200, 206]);
  service = new TranslatorService(client, outputAdapter);
  console.log(service);

  input = await cmn.readJson(opts.inputFile);
  console.log(input);
})(cli.flags);

function summarizeResponse(queryData, responseData) {
  const pk = responseData.pk;
  const queuing = responseData.queuing; // TODO


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
    const len = res.length;
    let retval = res.map((e) => {
      //return `${e.agent}:${e.data.results.length}`
      return { agent: e.agent, nres: e.data.results.length,
        uuid: e.uuid, fetch_ms: e.meta.fetchMs, parse_ms: e.meta.parseMs};
    });
    //return `${len}=${retval.join(';')}`
    return retval;
  }

  function summarize_non_completed(res) {
    const len = res.length;
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

  const completed_summary = summarize_completed(responseData.completed);
  const errored_summary = summarize_non_completed(responseData.errored);
  const running_summary = summarize_non_completed(responseData.running);
  const qdata_summary = summarize_query_data(queryData);
  const perf_summary = summarize_perf(responseData);
  const retval = {
    pk: responseData.pk,
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

async function initiateQuery(translatorService, qdata) {
  let query = translatorService.inputToQuery(qdata);
  let resp = await translatorService.submitQuery(query);
  console.log(resp);
  /* We'll assume that query submission mostly works and NOT track perf for it
   * to avoid complicating this program even more */
  let pk = resp.pk;
  return pk;
}

async function biggy0(service, client, filters, qdata, logger, pk,
  submissionDelay, startTime, maxMinutes, iter, maxIter) {

  //  if (iter !== -1 && iter >= maxIter) return;

  let resp = await client.collectAllResults(pk, filters, true);
  let respSummary = summarizeResponse(qdata, resp);
  let terminated = checkTerminated(resp);
  let timeExceeded = ((new Date() - startTime) / 1000) / 60 > maxMinutes;
  let keepLooping = maxIter === -1 || iter < maxIter;
  let displayIter = iter + 1;
  let msg;

  if (terminated) {
    if (keepLooping) {
      // RESTART
      msg = 'Query terminated successfully. Restarting.';
      logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
      let pk = await initiateQuery(service, qdata);
      setTimeout(biggy, submissionDelay * 1000, service, client, filters, qdata, logger, pk,
        submissionDelay, new Date(), maxMinutes, iter + 1, maxIter);
    } else {
      msg = 'Query terminated successfully. Exiting.';
      logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
      // DONE!
    }
  } else {
    if (keepLooping && timeExceeded) {
      // RESTART
      msg = `Query did not complete within ${maxMinutes} min. Restarting.`;
      logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
      let pk = await initiateQuery(service, qdata);
      setTimeout(biggy, submissionDelay * 1000, service, client, filters, qdata, logger, pk,
        submissionDelay, new Date(), maxMinutes, iter + 1, maxIter);
    } else if (keepLooping && !timeExceeded) {
      msg = 'Query progressing...';
      logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
      setTimeout(biggy, submissionDelay * 1000, service, client, filters, qdata, logger, pk,
        submissionDelay, startTime, maxMinutes, iter, maxIter);
    } else {
      msg = `Query did not complete within ${maxMinutes} min. Exiting.`;
      logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
      // DONE!
    }
  }
}


async function biggy(service, client, filters, qdata, logger, pk,
  submissionDelay, startTime, maxMinutes, iter, maxIter) {

  let resp = await client.collectAllResults(pk, filters, true);
  let respSummary = summarizeResponse(qdata, resp);
  let terminated = checkTerminated(resp);
  let timeExceeded = ((new Date() - startTime) / 1000) / 60 > maxMinutes;
  let displayIter = iter + 1;
  let msg;

  if (!terminated && !timeExceeded) {
    msg = 'Query progressing...';
    logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
    setTimeout(biggy, submissionDelay * 1000, service, client, filters, qdata, logger, pk,
      submissionDelay, startTime, maxMinutes, iter, maxIter);
  } else {
    msg = timeExceeded ? `Query did not complete within ${maxMinutes} min.`
      : `Query terminated successfully.`;
    logger(respSummary, terminated, startTime, maxMinutes, displayIter, maxIter, msg);
    iter += 1;
    if (maxIter === -1 || iter < maxIter) {
      let pk = await initiateQuery(service, qdata);
      setTimeout(biggy, submissionDelay * 1000, service, client, filters, qdata, logger, pk,
        submissionDelay, new Date(), maxMinutes, iter, maxIter);
    } else {
      // DO NOTHING; DIE
    }
  }
}

function jsonLogger(respSummary, terminated, startTime, maxMinutes, iter, maxIter, msg) {
  respSummary.start_time = startTime;
  respSummary.log_time = new Date();
  respSummary.elapsed_sec = Math.round((respSummary.log_time - respSummary.start_time) / 1000);
  respSummary.max_minutes = maxMinutes;
  respSummary.iter = iter;
  respSummary.maxIter = maxIter;
  respSummary.msg = msg;
  console.log(JSON.stringify(respSummary));
}


function textLogger(respSummary, terminated, startTime, maxMinutes, iter, maxIter, msg) {
  console.log("-=-=-=-");
  let elapsedSec = (new Date() - startTime) / 1000;
  let min = Math.floor(elapsedSec / 60);
  let sec = (elapsedSec % 60).toFixed(2);
  console.log(`
  Start time: ${startTime}
  Time spent: ${min}m${sec}s/${maxMinutes} min.
  Iteration: ${iter}/${maxIter}
  Message: ${msg}
  Details: ${JSON.stringify(respSummary, null, 2)}
  `);
}
/*
let resp = await client.collectAllResults('438c0ab7-78bf-4a25-af41-7c0c32495681', filters, true);
console.log(summarizeResponse(input[0], resp));
*/

input.forEach(async (e) => {
  let query =service.inputToQuery(e);
  console.log(`${JSON.stringify(query)}`);
  let pk = await initiateQuery(service, e);
  console.log(pk);
  biggy(service, client, filters, e, jsonLogger, pk,
    cli.flags.submissionDelay, new Date(), cli.flags.maxMinutes, 0, cli.flags.maxIter);
  /*let resp = await client.collectAllResults('28a96d16-5f01-4e45-b688-f6949cd15c6f', filters, true);
  //console.log(resp);
  console.log(summarizeResponse(e, resp));
  */
});
/*
let data = await service.getResults('28a96d16-5f01-4e45-b688-f6949cd15c6f', filters, true);
summarizeResponse(data);
*/
