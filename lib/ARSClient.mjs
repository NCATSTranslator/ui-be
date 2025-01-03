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
  constructor(origin, getPath, postPath, retainPath, useARSMerging, completeCodes=[200,206,444], runningCodes=[202]) {
    this.origin = origin;
    this.getURL = `${origin}${getPath}`;
    this.postURL = `${origin}${postPath}`;
    this.retainURL = `${origin}${retainPath}`;
    this.completeCodes = completeCodes;
    this.runningCodes = runningCodes;
    this.useARSMerging = useARSMerging;
  }

  async postQuery(query) {
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
      return await this._collectMergedResults(pkey, true);
    }

    return await this._collectChildResults(pkey, filters);
  }

  async getQueryResults(pkey, filters) {
    if (this.useARSMerging) {
      return await this._collectMergedResults(pkey);
    }

    return await this._collectChildResults(pkey, filters, true);
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
      headers['Accept-Encoding'] = 'gzip';
    }

    return cmn.sendRecvJSON2(url, 'GET', headers, null, compress);
  }

  processQueryUpdate(arsUpdate) {
    const eventType = cmn.jsonGet(arsUpdate, 'event type');
    const update = {
      pk: cmn.jsonGet(arsUpdate, 'pk'),
      timestamp: cmn.jsonGet(arsUpdate, 'timestamp'),
      status: null,
      aras: []
    };
    switch (eventType) {
      case _CALLBACK_EVENT.MV_AVAILABLE:
        update.aras = cmn.jsonGet(arsUpdate, 'merged_versions_list').map(idAraPair => idAraPair[1]);
        update.aras.reverse();
        if (cmn.jsonGet(arsUpdate, 'complete')) {
          update.status = cmn.QUERY_STATUS.COMPLETE;
        } else {
          update.status = cmn.QUERY_STATUS.RUNNING;
        }
        break;
      case _CALLBACK_EVENT.MV_BEGUN:
      case _CALLBACK_EVENT.ARA_COMPLETE:
        break; // We can ignore these events for now
      case _CALLBACK_EVENT.ERROR:
        logger.error(`Received ARS callback error for query with PK ${update.pk}: ${arsUpdate.message}`);
        update.status = cmn.QUERY_STATUS.ERROR;
        break;
      default:
        throw new Error(`Unknown ARS callback event type: ${eventType}`);
    }
    return update;
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

  async _collectMergedResults(pkey, statusCheck = false) {
    // Get the top level message from the ARS. This contains the list of currently
    // merged PKs.
    const [meta, arsSummary] = await this._fetchMessage(pkey);
    meta.timestamp = arsSummary.fields.timestamp || null;
    const mergedVersionList = arsSummary.fields.merged_versions_list;
    let completed = [];
    let running = [];
    let errored = [];
    if (cmn.isArray(mergedVersionList) && mergedVersionList.length > 0) {
      // Fetch all the merged version statuses to get the most recent version
      // that is also complete.
      mergedVersionList.reverse(); // First element should be the most recent
      const statusPromises = mergedVersionList.map((mergedEntry) => {
        const [uuid, agent] = mergedEntry;

        return this._fetchMessage(uuid, true);
      });

      await Promise.allSettled(statusPromises).then((promises) => {
        let i = 0;
        for (const promise of promises) {
          if (promise.status !== 'fulfilled') continue;

          const message = promise.value[1]; // We do not care about status metadata
          const status = {
            agent: mergedVersionList[i][1],
            uuid: message.message,
            status: message.status
          };

          // We have to inject the status codes because we expect them but the ARS does not
          // provide them
          if (message.status === 'Done' ||
              (message.status === 'Error' && message.code === 444)) {
            status.code = 200;
            completed.push(status);
          } else if (message.status === 'Running') {
            status.code = 202;
            running.push(status);
          } else {
            status.code = 500;
            errored.push(status);
          }
          i++;
        }
      });

      const mostRecentCompleted = completed[0] || null;
      const pastCompleted = completed.slice(1,);
      if (!statusCheck && mostRecentCompleted !== null) {
        // Finally get the actual result of the most recent complete merged version
        const [meta, results] = await this._fetchMessage(mostRecentCompleted.uuid, false, true);
        mostRecentCompleted.data = results.message;
        mostRecentCompleted.meta = meta;
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

    const parentStatus = arsSummary.fields.status;
    const message = {
      uuid: pkey,
      agent: arsSummary.fields.name,
      data: []
    };

    if (parentStatus === 'Done') {
      completed.push(message);
    } else if (parentStatus === 'Running') {
      running.push(message);
    } else {
      errored.push(message);
    }

    return {
      pk: pkey,
      completed: completed,
      running: running,
      errored: errored,
      meta: meta
    };
  }
}

const _CALLBACK_EVENT = Object.freeze({
  MV_AVAILABLE: 'merged_version_available',
  MV_BEGUN: 'merged_version_begun',
  ARA_COMPLETE: 'ara_response_complete',
  ERROR: 'ars_error'
});
