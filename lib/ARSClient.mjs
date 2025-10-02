'use strict'
export { ARSClient };
import { logger } from "./logger.mjs";
import * as cmn from "./common.mjs";

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
  constructor(ars_config, api_key, completeCodes=[200,206,444], runningCodes=[202]) {
    this.clientId = ars_config.client_id;
    this.apiKey = api_key;
    const origin = `${ars_config.protocol}://${ars_config.host}`;
    this.getURL = `${origin}${ars_config.pull_uri}`;
    this.postURL = `${origin}${ars_config.post_uri}`;
    this.retainURL = `${origin}${ars_config.retain_uri}`;
    this.subscribeURL = `${origin}${ars_config.subscribe_uri}`;
    this.unsubscribeURL = `${origin}${ars_config.unsubscribe_uri}`;
    this.validateTrapi = ars_config.use_trapi_validation;
    this.useARSMerging = ars_config.use_ars_merging;
    this.completeCodes = completeCodes;
    this.runningCodes = runningCodes;
  }

  async postQuery(query) {
    query.validate = this.validateTrapi;
    logger.info(`Posting query to ARS: ${JSON.stringify(query)}`);
    return cmn.sendRecvJSON2(this.postURL, 'POST', {}, query)
  }

  async retainQuery(pkey) {
    return cmn.sendRecvHTTP2(`${this.retainURL}/${pkey}`, 'POST', {},
        null, 'application/json', {
      encode: JSON.stringify,
      decode: cmn.identity // TODO: change this when the ARS sends back valid JSON
    });
  }

  async getQueryStatus(pkey, filters) {
    if (this.useARSMerging) {
      return this._collectMergedResults(pkey);
    }

    return this._collectChildResults(pkey, filters);
  }

  async getQueryResults(pkey, filters) {
    if (this.useARSMerging) {
      return this._collectMergedResults(pkey, true);
    }

    return this._collectChildResults(pkey, filters, true);
  }

  async subscribeQueries(pkeys) {
    return this._queryCallbackRequest(this.subscribeURL, pkeys);
  }

  async subscribeQuery(pkey) {
    return this.subscribeQueries([pkey]);
  }

  async unsubscribeQueries(pkeys) {
    return this._queryCallbackRequest(this.unsubscribeURL, pkeys);
  }

  async unsubscribeQuery(pkey) {
    return this.unsunscribeQueries([pkey]);
  }

  async _fetchMessage(uuid, doTrace=false, compress=false) {
    let url = `${this.getURL}/${uuid}`;
    if (doTrace && compress) {
      throw new ARSError('Cannot specify both trace and compress', null);
    }

    const headers = {};
    if (doTrace) {
      url += '?trace=y';
    } else if (compress) {
      url += '?compress=y';
      headers['Accept-Encoding'] = 'zstd, gzip';
    }

    return cmn.sendRecvJSON2(url, 'GET', headers, null, compress);
  }

  _isComplete(code) {
    return this.completeCodes.includes(code);
  }

  _isRunning(code) {
    return this.runningCodes.includes(code);
  }

  /* Given a list of agents, return a list that contains only those agents satisfying the filter conditions.
   * If only whitelist filters are specified, all agents matching either an explicit agent or a regex are returned.
   * If only blacklist filters are specified, only agents not matching an explicit agent and all regexes are returned.
   * If both blacklist and whitelist filters are specified, an intermediate result is built applying whitelist filters,
   * and then blacklist filters are applied to that intermediate result.
   */
  _applyFilters(agentList, filters) {
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

  async _collectChildResults(pkey, filters={}, fetchCompleted=false) {
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
    let [meta, baseResult] = await this._fetchMessage(pkey, true);
    meta.timestamp = baseResult.timestamp || null;

    /* Special case: if the ARS starts queuing requests, it will return status: running
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
    let filteredChildrenAgents = this._applyFilters(allChildrenAgents, filters);
    let filteredChildren = baseResult.children.filter(e => filteredChildrenAgents.includes(e.actor.agent));
    // use a hash vs an array for completed results to make it easier to correlate fetched data
    let completed = {};
    let running = [];
    let errored = [];
    // Divide results up by status
    for (const c of filteredChildren) {
      if (this._isComplete(c.code)) {
        completed[c.actor.agent] = extractFields(c);
      } else if (this._isRunning(c.code)) {
        running.push(extractFields(c));
      } else {
        try {
          errored.push(extractFields(c));
        } catch (err) {
          logger.error(`Error extracting fields from ARS error response: '${err}'`);
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
        logger.info(`kicking off fetch for ${e}`);
        return this._fetchMessage(e);
      });
      let finalCompleted = [];
      // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/allSettled#parameters
      await Promise.allSettled(promises).then(results => {
        results.forEach(item => {
          if (item.status === 'fulfilled') {
            let itemData = item.value[1];
            let agent = itemData.fields.name;
            let elem = completed[agent];
            elem.data = {};
            if (itemData.fields.data && itemData.fields.data.message) {
              elem.data = itemData.fields.data.message;
            }

            elem.meta = item.value[0];
            finalCompleted.push(elem);
            logger.info(`settled ${agent}`);
          } else {
            //
            logger.error('Unexpected case of being unable to fetch a result for an agent that reported code=200');
            errored.push(item.value); // No idea what might be in this object
          }
        });
        logger.info('done settling promises');
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

  async _collectMergedResults(pkey, fetchCompleted=false) {
    const ltype = fetchCompleted ? 'query-result' : 'query-status';

    // Get the top level message from the ARS. This contains the list of currently
    // merged PKs.
    const [meta, arsResp] = await this._fetchMessage(pkey, true);
    meta.timestamp = arsResp.timestamp || null;

    /* Special case: if the ARS starts queuing requests, it will return status: running
     * and an empty children array. Catch this special case and exit early.
     */
    if (arsResp.children.length === 0 && arsResp.status === "Running") {
      logger.warn({ltype: ltype, pk: pkey}, 'Queuing')
      return {
        meta: meta,
        pk: pkey,
        queuing: true,
        completed: [],
        running: [],
        errored: []
      };
    }

    let mergedVersionsList = arsResp.merged_versions_list ?? null;
    if (!mergedVersionsList || mergedVersionsList === "None") {
      mergedVersionsList = [];
    } else if (typeof mergedVersionsList === 'string') {
      // Currently in the trace=y response, ARS incorrectly encodes MVL as a string instead of an actual array
      mergedVersionsList = JSON.parse(mergedVersionsList.replace(/'/g, '"'));
    }

    let completed = [];
    let running = [];
    let errored = [];
    let n_mv_incomplete = 0;
    let n_mv_complete = 0;
    let first_complete_mv_idx = null;
    let fetch_completed_meta = null;

    if (cmn.isArray(mergedVersionsList) && mergedVersionsList.length > 0) {
      // Fetch all the merged version statuses to get the most recent version
      // that is also complete.
      mergedVersionsList.reverse(); // First element should be the most recent
      const statusPromises = mergedVersionsList.map((mergedEntry) => {
        const [uuid, agent] = mergedEntry;
        return this._fetchMessage(uuid, true);
      });

      await Promise.allSettled(statusPromises).then((promises) => {
        for (let i = 0; i < promises.length; i++) {
          const promise = promises[i];
          if (promise.status !== 'fulfilled') {
            logger.warn(promise, `Unexpected rejection when fetching message`);
            continue;
          }
          const message = promise.value[1]; // We do not care about status metadata
          logger.debug(message, 'this is the promise value');
          const status = {
            agent: mergedVersionsList[i][1],
            uuid: message.message,
            status: message.status
          };

          // We have to inject the status codes because we expect them but the ARS does not
          // provide them
          if (message.status === 'Done' ||
              (message.status === 'Error' && message.code === 444)) {
            if (message.code === 444) {
              logger.warn(message, 'An unfatal error with annotation occured upstream');
            }
            status.code = 200;
            completed.push(status);
            n_mv_complete += 1;
            if (first_complete_mv_idx === null) {
              first_complete_mv_idx = i;
            }
            logger.debug(status, 'donezo');
          } else if (message.status === 'Running') {
            status.code = 202;
            n_mv_incomplete += 1;
            running.push(status);
            logger.debug(status, 'a runner');
          } else {
            status.code = 500;
            n_mv_incomplete += 1;
            errored.push(status);
            logger.debug(status, 'i am error');
          }
        }
      });

      const mostRecentCompleted = completed[0] || null;
      const pastCompleted = completed.slice(1,);
      if (fetchCompleted && mostRecentCompleted !== null) {
        // Finally get the actual result of the most recent complete merged version
        const [meta, results] = await this._fetchMessage(mostRecentCompleted.uuid, false, true);
        mostRecentCompleted.data = results.message;
        mostRecentCompleted.meta = meta;
        fetch_completed_meta = meta;
      }

      // Bookkeeping so the FE can keep track of which ARAs have completed
      completed = pastCompleted.map((versionStatus) => {
        return {
          uuid: versionStatus.uuid,
          agent: versionStatus.agent,
          data: []
        };
      });

      if (mostRecentCompleted !== null) {
        completed.unshift(mostRecentCompleted);
      }
    }

    logger.info({
      ltype: ltype,
      pk: pkey,
      status: arsResp.status,
      code: arsResp.code,
      merged_versions_list: mergedVersionsList,
      first_complete_mv_idx: first_complete_mv_idx,
      n_mv_complete: n_mv_complete,
      n_mv_incomplete: n_mv_incomplete,
      ...this._summarizeChildren(arsResp, /^ara-/),
      ars_metadata: meta,
      fetch_result: fetchCompleted,
      ars_fetch_result_metadata: fetch_completed_meta
    }, `${ltype} ${pkey} ${arsResp.status} with ${n_mv_complete} merge(s) available`);

    return {
      pk: pkey,
      completed: completed,
      running: running,
      errored: errored,
      meta: meta
    };
  }

  _summarizeChildren(traceResponse, regex) {

    // Catch queuing here

    // Filter children based on the regex match for actor.agent
    const filteredChildren = traceResponse.children.filter(child =>
      regex.test(child.actor.agent)
    );

    // Helper functions to classify children
    const isComplete = child => child.status === "Done";
    const isRunning = child => child.status === "Running";
    const isErrored = child => /^4\d\d|^5\d\d/.test(child.code);

    // Aggregate the data
    const nComplete = filteredChildren.filter(isComplete).length;
    const nRunning = filteredChildren.filter(isRunning).length;
    const nErrored = filteredChildren.filter(isErrored).length;

    // Construct arrays for each status
    const complete = filteredChildren
      .filter(isComplete)
      .map(child => ({
        uuid: child.message, // maybe remove this?
        ara: child.actor.agent,
        result_count: child.result_count ?? null,
      }));

    const running = filteredChildren
      .filter(isRunning)
      .map(child => child.actor.agent);

    const errored = filteredChildren
      .filter(isErrored)
      .map(child => child.actor.agent);

    // Return the summarized object
    return {
      n_ara_complete: nComplete,
      n_ara_running: nRunning,
      n_ara_errored: nErrored,
      ara_complete: complete,
      ara_running: running,
      ara_errored: errored
    };
  }

  _queryCallbackRequest(url, pkeys) {
    const body = {
      client_id: this.clientId,
      pks:       pkeys,
      timestamp: new Date()
    };
    console.log(JSON.stringify(body));
    const headers = {};
    headers[_CUSTOM_HEADERS.X_EVENT_SIG] = cmn.generateHMACSignature(JSON.stringify(body), this.apiKey);
    return cmn.sendRecvJSON2(url, 'POST', headers, body);
  }
}

const _CUSTOM_HEADERS = Object.freeze({
  X_EVENT_SIG: 'x-event-signature'
});
