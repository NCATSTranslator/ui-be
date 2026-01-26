'use strict';
import { logger } from '../lib/logger.mjs';
import * as arsmsg from '../lib/ARSMessages.mjs';
import * as trapi from '../lib/trapi.mjs';

export { TranslatorService };

/* Implements:
 * - inputToQuery(input);
 * - submitQuery(query)
 * - getQueryStatus(queryId)
 * - getResults(queryId, [filters])
 */

class QueryClientError extends Error {
  constructor(message, qid, clientOp, upstreamError) {
    super(message);
    this.name = 'QueryClientError';
    this.qid = qid;
    this.op = clientOp;
    this.upstreamError = upstreamError;
  }
}

class TranslatorService
{
  constructor(queryClient)
  {
    this.queryClient = queryClient;
  }

  inputToQuery(input)
  {
    return trapi.clientReqToTrapiQuery(input);
  }

  async submitQuery(query)
  {
    try
    {
      let [meta, res] = await this.queryClient.postQuery(query);
      if (arsmsg.isAcceptedQuery(res))
      {
        return res;
      }
      else
      {
        throw new QueryClientError(`Upstream service rejected query with response: ${JSON.stringify(res)}`, null, 'query', null);
      }
    } catch (err)
    {
      logger.error(`An error occured while attempting to post a query: ${err}`);
      throw new QueryClientError(`Error posting query`, null, 'query', err);
    }
  }

  async retainQuery(queryId) {
    try {
      const resp = await this.queryClient.retainQuery(queryId);
      return resp;
    } catch (err) {
      logger.error(err);
      throw new QueryClientError(`Error retaining query for ${queryId}`, queryId, 'retain', err);
    }
  }

  async getQueryStatus(queryId, filters={})
  {
    try
    {
      let res = await this.queryClient.getQueryStatus(queryId, filters);
      return res;
    }
    catch (err)
    {
      throw new QueryClientError(`Error querying status for ${queryId}`, queryId, 'status', err);
    }
  }

  async getResults(queryId, filters={})
  {
    try
    {
      let res = await this.queryClient.getQueryResults(queryId, filters);
      return res;
    }
    catch (err)
    {
      logger.error(`Error retrieving results for ${queryId}: '${err}'`);
      throw new QueryClientError(`Error retrieving results for ${queryId}`, queryId, 'result', err);
    }
  }

  async subscribeQuery(queryId) {
    try {
      const res = await this.queryClient.subscribeQuery(queryId);
      return res;
    } catch (err) {
      logger.error(`Error subscribing to query ${queryId}: '${err}'`);
      return null;
    }
  }

  async unsubscribeQuery(queryId) {
    try {
      const res = await this.queryClient.unsubscribeQuery(queryId);
    } catch (err) {
      logger.error(`Error unsubscribing to query ${queryId}: '${err}'`);
      throw new QueryClientError(`Error unsubscribing to query ${queryId}`, queryId, 'unsubscribe', err);
    }
  }

  async get_notification_statuses(pks) {
    try {
      return await this.queryClient.get_notification_statuses(pks);
    } catch (err) {
      logger.error(`Error getting notification statuses. Got error:'${err}'`);
      throw new QueryClientError(`Error getting notfication statuses for pks: ${pks}`);
    }
  }
}
/*
  // testing stuff
var aa = new ARSClient('https://ars-prod.transltr.io', '/ars/api/messages', '/ars/api/submit');
var t = new TranslatorService(aa);
var mondo = 'MONDO:0005148';
export { t, aa, mondo };
*/
