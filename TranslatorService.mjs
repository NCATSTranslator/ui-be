'use strict';

import * as arsmsg from './ARSMessages.mjs';
import * as trapi from './trapi.mjs';

export { TranslatorService };

/* Implements:
 * - inputToQuery(input);
 * - submitQuery(query)
 * - getQueryStatus(queryId)
 * - getResults(queryId, [filters])
 */
class TranslatorService
{
  constructor(queryClient, outputAdapter)
  {
    this.queryClient = queryClient;
    this.outputAdapter = outputAdapter;
  }

  inputToQuery(input)
  {
    return trapi.queryToCreativeQuery(input);
  }

  async submitQuery(query)
  {
    try
    {
      let res = await this.queryClient.postQuery(query);
      if (arsmsg.isAcceptedQuery(res))
      {
        return res;
      }
      else
      {
        throw new Error(`ARS rejected query with response: ${JSON.stringify(res)}`);
      }
    } catch (err)
    {
      console.error(`Error posting query: '${err}' [${JSON.stringify(query)}]`);
      throw new Error(err);
    }
  }

  async getQueryStatus(queryId, filters={})
  {
    try
    {
      let res = await this.queryClient.collectAllResults(queryId, filters);
      return res;
    }
    catch (err)
    {
      console.error(`Error querying status for ${queryId}: '${err}'`);
      throw new Error(err);
    }
  }

  async getResults(queryId, filters={})
  {
    try
    {
      let res = await this.queryClient.collectAllResults(queryId, filters, true);
      return res;
    }
    catch (err)
    {
      console.error(`Error retrieving results for ${queryId}: '${err}'`);
      throw new Error(err);
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
