'use strict';

import * as arsmsg from './ARSMessages.mjs';
import * as trapi from './trapi.mjs';

/* Translate messages coming from the Translator Service into the formats that the Frontend (FE) app expects */
/* This module should not contain logic that goes beyond message transformations */
export { queryStatusToFE, queryResultsToFE, querySubmitToFE };
// msg: ARS client message with trace=y
function queryStatusToFE(msg) {
    return {
        status: msg.running.length > 0 ? "running" : "success",
        data: {
            qid: msg.pk,
            aras: msg.completed.map(e => e.agent)
        }
    };
}

// msg: an ARS client message w/ results
function queryResultsToFE(msg, maxHops) {
    // Omit ARA results where the actual results array is empty
    // Need to account for the ARS returning both null and []
    let data = msg.completed.filter(e => Array.isArray(e.data.results) && e.data.results.length > 0).map(
        e => { return {
                agent: e.agent,
                message: e.data
            }
        });

    return {
        status: msg.running.length > 0 ? "running" : "success",
        data: trapi.creativeAnswersToSummary(msg.pk, data, maxHops)
    };
}

// Currently the FE doesn't expect this message to be handling failure conditions
// msg: The standard ARS message returned when you post a query
function querySubmitToFE(msg) {
    return {
        status: 'success',
        data: arsmsg.msgId(msg)
    }
}
