'use strict';

import * as cmn from '../lib/common.mjs';
import * as arsmsg from '../lib/ARSMessages.mjs';
import * as sm from '../lib/summarization/core.mjs';

/* Translate messages coming from the Translator Service into the formats that the Frontend (FE) app expects */
/* This module should not contain logic that goes beyond message transformations */
export { TranslatorServicexFEAdapter };
// msg: ARS client message with trace=y

class TranslatorServicexFEAdapter {
  constructor(feature_config, node_signing_secret = null) {
    this.feature_config = feature_config;
    this.node_signing_secret = node_signing_secret;
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
        aras: msg.completed.map(e => e.agent),
        timestamp: msg.meta.timestamp
      }
    };
  }

  async queryResultsToFE(msg, maxHops) {
    // Omit ARA results where the actual results array is empty
    // Need to account for the ARS returning both null and []
    const data = msg.completed.filter(e => {
      return e.data && !(Array.isArray(e.data) && e.data.length === 0);
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
    this._sign_nodes(summary);

    return {
      status: _determine_status(msg),
      data: summary
    };
  }

  _sign_nodes(summary) {
    if (!this.node_signing_secret) return;
    for (const node of Object.values(summary.nodes)) {
      node.signature = cmn.sign_entity_data(node.to_raw_obj(), this.node_signing_secret);
    }
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
