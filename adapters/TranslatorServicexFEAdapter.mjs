'use strict';

import * as cmn from '../lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';
import * as arsmsg from '../lib/ARSMessages.mjs';
import * as sm from '../lib/summarization/summarization.mjs';

/* Translate messages coming from the Translator Service into the formats that the Frontend (FE) app expects */
/* This module should not contain logic that goes beyond message transformations */
export { TranslatorServicexFEAdapter };
// msg: ARS client message with trace=y

class TranslatorServicexFEAdapter {
  constructor(feature_config) {
    this.feature_config = feature_config;
  }

  querySubmitToFE(msg) {
    return {
      status: cmn.QUERY_STATUS.COMPLETE,
      data: arsmsg.msgId(msg)
    }
  }

  queryStatusToFE(msg) {
    return {
      status: _determine_status(msg),
      data: {
        qid: msg.pk,
        query: trapi.query_graph_to_client_request(msg.query_graph),
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
        message: e.data
      }
    });

    const summary = await sm.answers_to_summary(
      msg.pk,
      data,
      this.feature_config);
    summary.set_timestamp(msg.meta.timestamp);

    return {
      status: _determine_status(msg),
      data: summary
    };
  }
}

function _determine_status(msg) {
  if (msg.queuing) {
    return cmn.QUERY_STATUS.RUNNING;
  }
  else {
    return msg.running.length > 0 ? cmn.QUERY_STATUS.RUNNING : cmn.QUERY_STATUS.COMPLETE;
  }
}
