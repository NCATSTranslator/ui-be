'use strict';

import * as arsmsg from '../lib/ARSMessages.mjs';
import * as sm from '../lib/summarization.mjs';

/* Translate messages coming from the Translator Service into the formats that the Frontend (FE) app expects */
/* This module should not contain logic that goes beyond message transformations */
export { TranslatorServicexFEAdapter };
// msg: ARS client message with trace=y

function determineStatus(msg) {
  if (msg.queuing) {
    return "running";
  }
  else {
    return msg.running.length > 0 ? "running" : "success";
  }
}

class TranslatorServicexFEAdapter {

  querySubmitToFE(msg) {
    return {
      status: 'success',
      data: arsmsg.msgId(msg)
    }
  }

  queryStatusToFE(msg) {
    return {
      status: determineStatus(msg),
      data: {
        qid: msg.pk,
        aras: msg.completed.map(e => e.agent),
        timestamp: msg.meta.timestamp
      }
    };
  }

  async queryResultsToFE(msg, maxHops) {
    // Omit ARA results where the actual results array is empty
    // Need to account for the ARS returning both null and []
    const data = msg.completed.filter(e => {
      return !!e.data;
    }).map(e => {
      return {
        agent: e.agent,
        trapi: e.data
      }
    });

    const smry = await sm.answersToSmry(
      msg.pk,
      data,
      maxHops);
    smry.meta.timestamp = msg.meta.timestamp;

    return {
      status: determineStatus(msg),
      data: smry
    };
  }
}

// msg: an ARS client message w/ results

// Currently the FE doesn't expect this message to be handling failure conditions
// msg: The standard ARS message returned when you post a query
