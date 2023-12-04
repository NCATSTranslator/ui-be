'use strict'

import * as cmn from "./common.mjs";
export { ARSClient };

/* Data format:
{
  pk: 'e0e1a38d-c422-49b9-b39e-0ff9d76bfd07',
  completed: [
    {
      agent: 'ara-bte',
      uuid: '004867d5-d2c6-47d6-9dea-2d9ccbbe6d69',
      status: 'Done',
      code: 200,
      data: [Object] // Optional, only present if fetchCompleted=true
    },
    {
      agent: 'kp-cam',
      uuid: '7501c27e-c7a4-4295-8b6c-0dfa228210b1',
      status: 'Done',
      code: 200,
      data: [Object]
    }
  ],
  running: [ {...}, {...} ],
  errored: [ {...}, {...} ], // Object in this array may differ in format from what is described above
}

*/

class ARSError extends Error {
  constructor(message, upstreamError) {
    super(message);
    this.upstreamError = upstreamError;
  }
}

class ARSClient {
  constructor(origin, getPath, postPath, completeCodes=[200,206], runningCodes=[202]) {
    this.origin = origin;
    this.getURL = `${origin}${getPath}`;
    this.postURL = `${origin}${postPath}`;
    // Yes, 422 means the message is complete and valid. Specifically it means that there was some error in the scoring process.
    this.completeCodes = completeCodes;
    this.runningCodes = runningCodes;
  }

  async fetchMessage(uuid, doTrace=false) {
    let url = `${this.getURL}/${uuid}`;
    if (doTrace) {
      url += '?trace=y';
    }
    return cmn.sendRecvJSON2(url, 'GET');
  }

  async postQuery(query) {
    return cmn.sendRecvJSON2(this.postURL, 'POST', {}, query)
  }

  isComplete(code) {
    return this.completeCodes.includes(code);
  }

  isRunning(code) {
    return this.runningCodes.includes(code);
  }

  isErrored(code) {
    return !(this.isComplete(code) || this.isRunning(code));
  }

  constructFilterRegexes(filterArray) {
    return filterArray.map(e => {
      if (typeof e === 'string') {
        return new RegExp(e);
      } else {
        return e;
      }
    });
  }

  /* Given a list of agents, return a list that contains only those agents satisfying the filter conditions.
   * If only whitelist filters are specified, all agents matching either an explicit agent or a regex are returned.
   * If only blacklist filters are specified, only agents not matching an explicit agent and all regexes are returned.
   * If both blacklist and whitelist filters are specified, an intermediate result is built applying whitelist filters,
   * and then blacklist filters are applied to that intermediate result.
   */
  applyFilters(agentList, filters) {
    let retval = [...agentList];
    let hasWhiteList = false;
    if (filters.hasOwnProperty('whitelist')) {
      retval = retval.filter(e => filters.whitelist.includes(e));
      hasWhiteList = true;
    }
    if (filters.hasOwnProperty('whitelistRx')) {
      const whiteRxRes = agentList.filter(e => filters.whitelistRx.test(e));
      if (hasWhiteList) {
        retval = retval.concat(whiteRxRes)
      } else {
        retval = whiteRxRes;
      }
    }
    /* Uniqify the result so far. If no white filters were present,
     * retval is the same as masterList at this point. If white
     * filters were present, retval is the result of applying them,
     * and black filters should be applied to that result.
     */
    retval = [...new Set(retval)];

    if (filters.hasOwnProperty('blacklist')) {
      retval = retval.filter(e => !filters.blacklist.includes(e));
    }
    if (filters.hasOwnProperty('blacklistRx')) {
      retval = retval.filter(e => !filters.blacklistRx.test(e));
    }
    return retval;
  }
  /*
   * pkey: must be the UUID received upon submitting a query
   * fetchCompleted: if true, will fetch data for ARAs that have completed
   * filters: {
   *   whitelist: [ara1, ara2, ...],
   *   whitelistRx: <regexp>,
   *   blacklist: [ara1, ara2, ...],
   *   blacklistRx: <regexp>,
   * }
   *
   * Filters are applied on the list of children returned in the base ARS response, in the
   * following way:
   * - All agents exactly matching an element in the whitelist array are included
   * - All agents matching the whitelistRx are included
   * Note both filters are applied against the master list of agents. I.e, the
   * result is the union of agents matching whitelist array elements and matching
   * whitelistRx, not the intersection.
   * If only whitelist filters are specified, the results returned are as above.
   * If only blacklist filters are specified, all agents are returned except:
   * - Those exactly matching an element in the blacklist array
   * - Those matching blacklistRx
   * If BOTH whitelist and blacklist filters are specified
   * - The whitelists are first applied as described above, then
   * - The blacklists are applied to the list of agents matching the whitelists
   *
   */

  async collectAllResults(pkey, filters={}, fetchCompleted=false) {
    function extractFields(childMsg) {
      return {
        agent: childMsg.actor.agent,
        uuid: childMsg.message,
        status: childMsg.status,
        code: childMsg.code
      }
    }

    /* Fetch all results, divvy up by status, narrow down the ones that completed
     * to ones that match the specified filters (if any), and return full message
     * data for only those (and only if requested)
     */
    let retval = {};
    let [meta, baseResult] = await this.fetchMessage(pkey, true);

    /* Special case: if the ARS starts queueing requests, it will return status: running
     * and an empty children array. Catch this special case and exit early.
     */
    if (baseResult.children.length === 0 && baseResult.status === "Running") {
      return {
        meta: meta,
        pk: pkey,
        queuing: true,
        completed: [],
        running: [],
        errored: []
      };
    }

    let allChildrenAgents = baseResult.children.map(e => e.actor.agent);
    let filteredChildrenAgents = this.applyFilters(allChildrenAgents, filters);
    let filteredChildren = baseResult.children.filter(e => filteredChildrenAgents.includes(e.actor.agent));
    // use a hash vs an array for completed results to make it easier to correlate fetched data
    let completed = {};
    let running = [];
    let errored = [];
    // Divide results up by status
    for (const c of filteredChildren) {
      if (this.isComplete(c.code)) {
        completed[c.actor.agent] = extractFields(c);
      } else if (this.isRunning(c.code)) {
        running.push(extractFields(c));
      } else {
        try {
          errored.push(extractFields(c));
        } catch (err) {
          console.error(`Error extracting fields from ARS error response: '${err}'`);
          errored.push(c);
        }
      }
    }

    if (!fetchCompleted) {
      retval = {
        pk: pkey,
        completed: Object.values(completed),
        running: running,
        errored: errored
      };
    } else {
      let agents = Object.keys(completed);
      // Get uuids corresp. to these agents, fetch their results in parallel
      let toFetch = agents.map(e => completed[e].uuid);
      let start = new Date();
      const promises = toFetch.map(async (e) => {
        console.log(`kicking off fetch for ${e}`);
        return this.fetchMessage(e);
      });
      let finalCompleted = [];
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/allSettled#parameters
      await Promise.allSettled(promises).then(results => {
        results.forEach(item => {
          if (item.status === 'fulfilled') {
            let itemData = item.value[1];
            let agent = itemData.fields.name;
            let elem = completed[agent];
            elem.data = itemData.fields.data.message;
            elem.meta = item.value[0];
            finalCompleted.push(elem);
            console.log(`settled ${agent}`);
          } else {
            //
            console.error('Unexpected case of being unable to fetch a result for an agent that reported code=200');
            errored.push(item.value); // No idea what might be in this object
          }
        });
        console.log('done settling promises');
        retval = {
          pk: pkey,
          completed: finalCompleted,
          running: running,
          errored: errored
        };
      });
      meta.childrenProcessingMs = new Date() - start;
    }
    retval.queuing = false;
    retval.meta = meta;
    return retval;
  }

  // Get all results that have been pre-merged by the ARS
  // *=* TODO: Need to update w/ new metadata-having interface!
  async collectMergedResults(pkey, statusCheck = false) {
    // Get the top level message from the ARS
    const arsSummary = await this.fetchMessage(pkey);
    const mergedVersionList = arsSummary.fields.merged_versions_list;
    // If we don't have any merged versions yet, the data in the top level message
    // is the data we want
    let completed = [];
    let running = [];
    let errored = [];
    if (cmn.isArray(mergedVersionList) && mergedVersionList.length > 0) {
      const statusPromises = mergedVersionList.map((mergedEntry) => {
        const [uuid, agent] = mergedEntry;
        return this.fetchMessage(uuid, true);
      });

      await Promise.allSettled(statusPromises).then((promises) => {
        let i = 0;
        for (const promise of promises) {
          if (promise.status !== 'fulfilled') continue;

          const message = promise.value;
          const status = {
            agent: mergedVersionList[i][1],
            uuid: message.message,
            status: message.status
          };

          // We have to inject codes until the ARS is fixed
          if (message.status === 'Done' || message.status === 'Running') {
            status.code = 200;
            completed.push(status);
          } else {
            status.code = 500;
            errored.push(status);
          }
          i++;
        }
      });

      completed = completed.slice(completed.length-1,);
      if (!statusCheck) {
        const results = await this.fetchMessage(completed[0].uuid);
        completed[0].data = results.fields.data.message;
      }

      for (const mergedVersion of mergedVersionList) {
        const [uuid, agent] = mergedVersion;
        if (uuid === completed[0].uuid) break;

        completed.push({
          uuid: uuid,
          agent: agent
        });
      }
    }

    const status = arsSummary.fields.status;
    const message = {
      uuid: pkey,
      agent: arsSummary.fields.agent
    };

    if (status === 'Done') {
      completed.push(message);
    } else if (status === 'Running') {
      running.push(message);
    } else {
      errored.push(message);
    }

    return {
      pk: pkey,
      completed: completed,
      running: running,
      errored: errored
    };
  }
}

/* Testing stuff: ignore!

var pk = '28a96d16-5f01-4e45-b688-f6949cd15c6f';
var c = new ARSClient('https://ars-prod.transltr.io', '/ars/api/messages', '/ars/api/submit');
var filters = {whitelistRx: /^ara-/};
var res = await c.collectAllResults(pk, filters, true);

res.completed.forEach(x => {
  console.log(`${x.agent}: ${x.data && x.data.results? x.data.results.length : 'null'}; meta: ${JSON.stringify(x.meta)}`);
});

async function doit(pk) {
  res = await c.collectAllResults(pk, filters, true);
  res.completed.forEach(x => {
    console.log(`${x.agent}: ${x.data && x.data.results? x.data.results.length : 'null'}; meta: ${JSON.stringify(x.meta)}`);
  });
  console.log(res.meta);
};
export { c, res, doit};
*/
