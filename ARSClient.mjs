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

  // Get all results that have been pre-merged by the ARS
  // *=* TODO: Need to update w/ new metadata-having interface!
  async collectMergedResults(pkey, statusCheck = false) {
    // Get the top level message from the ARS. This contains the list of currently
    // merged PKs.
    const [meta, arsSummary] = await this.fetchMessage(pkey);
    meta.timestamp = arsSummary.fields.timestamp || null;
    const mergedVersionList = arsSummary.fields.merged_versions_list;
    let completed = [];
    let running = [];
    let errored = [];
    if (cmn.isArray(mergedVersionList) && mergedVersionList.length > 0) {
      // Fetch all the merged version statuses to get the most recent version
      // that is also complete.
      const statusPromises = mergedVersionList.map((mergedEntry) => {
        const [uuid, agent] = mergedEntry;
        return this.fetchMessage(uuid, true);
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
          if (message.status === 'Done') {
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

      const mostRecentCompleted = completed[completed.length-1] || null;
      const pastCompleted = completed.slice(0, completed.length-1);
      if (!statusCheck && mostRecentCompleted !== null) {
        // Finally get the actual result of the most recent complete merged version
        const [meta, results] = await this.fetchMessage(mostRecentCompleted.uuid);
        mostRecentCompleted.data = results.fields.data.message;
        mostRecentCompleted.meta = meta;
      }

      // Bookkeeping so the FE can keep track of which ARAs have completed
      completed = pastCompleted.map((versionStatus) => {
        return {
          uuid: versionStatus.uuid,
          agent: versionStatus.agent
        };
      });

      if (mostRecentCompleted !== null) {
        completed.unshift(mostRecentCompleted);
      }
    }

    const status = arsSummary.fields.status;
    const message = {
      uuid: pkey,
      agent: arsSummary.fields.name
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
      errored: errored,
      meta: meta
    };
  }
}
