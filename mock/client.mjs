export { ARSClient };
import { logger } from "../lib/logger.mjs";
import * as cmn from "../lib/common.mjs";
import * as fs from "node:fs";

class ARSClient {
  constructor(base_path) {
    this.data = {};
    const query_dirs = fs.readdirSync(base_path);
    for (const dir of query_dirs) {
      this.data[dir] = {
        trace: JSON.parse(fs.readFileSync(`${base_path}/${dir}/trace.json`)),
        results: JSON.parse(fs.readFileSync(`${base_path}/${dir}/results.json`))
      };
    }
  }

  async postQuery(query) {
    throw new Error("Not implemented");
  }

  async retainQuery(pkey) {
    throw new Error("Not implemented");
  }

  async getQueryStatus(pkey, filters) {
    return this._collectMergedResults(pkey);
  }

  async getQueryResults(pkey, filters) {
    return this._collectMergedResults(pkey, true);
  }

  async subscribeQueries(pkeys) {
    throw new Error("Not implemented");
  }

  async subscribeQuery(pkey) {
    throw new Error("Not implemented");
  }

  async unsubscribeQueries(pkeys) {
    throw new Error("Not implemented");
  }

  async unsubscribeQuery(pkey) {
    throw new Error("Not implemented");
  }

  async _fetchMessage(uuid, doTrace=false, compress=false) {
    const meta = {
      fetchMs: 0,
      parseMs: 0,
      status: "done",
      headers: ""
    };
    if (doTrace) {
      return [meta, this.data[uuid].trace];
    }
    return [meta, this.data[uuid].results];
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
}
