'use strict'

// export { ARSClient, aa, m0 ,m1 ,m2 ,m3 ,m4 ,m5 ,m6 ,m7 ,m8 ,m9 ,m10,m11 };
export { ARSClient }

// 6d7ce863-e4d3-4cf8-8a38-3b7191d17964
// 26394fad-bfd9-4e32-bb90-ef9d5044f593

class ARSClient {
    constructor(origin, getPath, postPath) {
        this.origin = origin;
        this.getURL = `${origin}${getPath}`;
        this.postURL = `${origin}${postPath}`;
    }

    async fetchResultByKey(uuid, doTrace=false) {
        let url = `${this.getURL}/${uuid}`;
        if (doTrace) {
            url += '?trace=y';
        }
        //console.log(`fetching ${url}`);
        return this.sendRecv(url, 'GET');
    }

    async postQuery(query) {
        return this.sendRecv(this.postURL, 'POST', {}, query)
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
     * Filters are relevant only when fetchCompleted = true (specifying filters
     * when fetchCompleted=false has no effect). Filters are applied on the list
     * of completed (status = 200) agents in the following way:
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
    async collectAllResults(pkey, fetchCompleted=false, filters={}) {

        function extractFields(child) {
            return {
                agent: child.actor.agent,
                uuid: child.message,
                status: child.status,
                code: child.code
            }
        }

        function applyFilters(masterList, filters) {
            let retval = [...masterList];
            let hasWhiteList = false;
            if (filters.hasOwnProperty('whitelist')) {
                retval = retval.filter(e => filters.whitelist.includes(e));
                hasWhiteList = true;
            }
            if (filters.hasOwnProperty('whitelistRx')) {
                let whiteRxRes = masterList.filter(e => filters.whitelistRx.test(e));
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

        /* Fetch all results, divvy up by status, narrow down the ones that completed
         * to ones that match the specified filters (if any), and return full message
         * data for only those (and only if requested)
         */
        let retval = {};
        let baseResult = await this.fetchResultByKey(pkey, true);
        let completed = {}; // use a hash as a placeholder to make fetching data easier
        let running = [];
        let errored = [];
        // Divide results up by status
        for (const c of baseResult.children) {
            switch (c.code) {
                case 200:
                    completed[c.actor.agent] = extractFields(c);
                    break;
                case 202:
                    running.push(extractFields(c));
                    break;
                default:
                    errored.push(extractFields(c));
            }
        }
        if (!fetchCompleted) {
            retval = {
                "completed": Object.values(completed),
                "running": running,
                "errored": errored
            };
        } else {
            let agents = applyFilters(Object.keys(completed), filters);
            // Get uuids corresp. to these agents, fetch their results in parallel
            let toFetch = agents.map(e => completed[e].uuid);
            const promises = toFetch.map(async (e) => this.fetchResultByKey(e));
            let finalCompleted = [];
            // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/allSettled#parameters
            await Promise.allSettled(promises).then(results => {
                results.forEach(item => {
                    if (item.status === 'fulfilled') {
                        let agent = item.value.fields.name;
                        let elem = completed[agent];
                        elem.data = item.value.fields.data.message;
                        finalCompleted.push(elem);
                    } else {
                        // Unexpected case of being unable to fetch a result for an agent with code = 200
                        errored.push(item.value); // No idea what might be in this object
                    }
                });
                //console.log('done settling promises');
                retval = {
                    "completed": finalCompleted,
                    "running": running,
                    "errored": errored
                };
            });
        }
        return retval;
    }

    async sendRecv(url, method='GET', headers={}, body=null) {
        let options = {
            method: method,
            headers: {...headers}
        };
        options.headers['Content-type'] = 'application/json';
        if (body) {
            options.body = JSON.stringify(body);
        }
        let resp = await fetch(url, options);
        if (resp.ok) {
            return resp.json();
        } else {
            let errmsg = `ERROR: status: ${resp.status}; msg: '${resp.statusText}'`;
            throw new Error(errmsg);
        }
    }
}

/*
testing stuff
var aa = new ARSClient('https://ars-prod.transltr.io', '/ars/api/messages', '/ars/api/submit');
var m0 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0002251"],"categories":["biolink:Disease"]}}}}}';
var m1 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0004952"],"categories":["biolink:Disease"]}}}}}';
var m2 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0004975"],"categories":["biolink:Disease"]}}}}}';
var m3 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005002"],"categories":["biolink:Disease"]}}}}}';
var m4 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005147"],"categories":["biolink:Disease"]}}}}}';
var m5 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005148"],"categories":["biolink:Disease"]}}}}}';
var m6 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005155"],"categories":["biolink:Disease"]}}}}}';
var m7 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005377"],"categories":["biolink:Disease"]}}}}}';
var m8 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0005812"],"categories":["biolink:Disease"]}}}}}';
var m9 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0007947"],"categories":["biolink:Disease"]}}}}}';
var m10 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0008170"],"categories":["biolink:Disease"]}}}}}';
var m11 = '{"message":{"query_graph":{"edges":{"treats":{"knowledge_type":"inferred","predicates":["biolink:treats"],"subject":"drug","object":"disease"}},"nodes":{"drug":{"categories":["biolink:ChemicalEntity"]},"disease":{"ids":["MONDO:0010200"],"categories":["biolink:Disease"]}}}}}';
*/
