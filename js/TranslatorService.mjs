'use strict';


import { ARSClient } from "./ARSClient.mjs";
import * as arsmsg from './ARSMessages.mjs';
import * as trapi from './trapi.mjs';

export { TranslatorService };
export { t, aa, mondo };


/* Implements:
 * - inputToQuery(input);
 * - submitQuery(query)
 * - getQueryStatus(queryId)
 * - getResults(queryId, [filters])
 */
class TranslatorService {
    constructor(client) {
        this.client = client;
    }

    inputToQuery(input) {
        return trapi.diseaseToCreativeQuery(input);
    }

    async submitQuery(query) {
        try {
            let res = await this.client.postQuery(query);
            if (arsmsg.isAcceptedQuery(res)) {
                return res;
            } else {
                throw new Error(`ARS rejected query with response: ${JSON.stringify(res)}`);
            }
        } catch (err) {
            console.error(`Error posting query: '${err}' [${JSON.stringify(query)}]`);
            throw new Error(err);
        }

    }
    async getQueryStatus(queryId) {
        try {
            let res = await this.client.collectAllResults(queryId);
            return res;
        } catch (err) {
            console.error(`Error querying status for ${queryId}: '${err}'`);
            throw new Error(err);
        }
    }

    async getResults(queryId, filters={}) {
        try {
            let res = await this.client.collectAllResults(queryId, true, filters);
            return res;
        } catch (err) {
            console.error(`Error retrieving results for ${queryId}: '${err}'`);
            throw new Error(err);
        }
    }
}

var aa = new ARSClient('https://ars-prod.transltr.io', '/ars/api/messages', '/ars/api/submit');
var t = new TranslatorService(aa);
var mondo = 'MONDO:0005148';
