export {
  Summary,
  SummaryMetadata
}

import * as cmn from '../common.mjs';

class Summary {
  constructor(meta, results, paths, nodes, edges, pubs, trials, provenance, tags, errors) {
    this.meta = meta || {};
    this.results = results || [];
    this.paths = paths || {};
    this.nodes = nodes || {};
    this.edges = edges || {};
    this.trials = trials || {};
    this.provenance = provenance ?? {};
    this.tags = tags || {};
    this.errors = errors || {};
    this.publications = pubs ?? {};
    Object.keys(this.publications).forEach(pid => {
      this.publications[pid] = this.publications[pid].to_obj();
    });
  }

  set_timestamp(timestamp) {
    this.meta.timestamp = timestamp;
    for (const node of Object.values(this.nodes)) {
      node.source_time = timestamp;
    }
    for (const edge of Object.values(this.edges)) {
      edge.source_time = timestamp;
    }
  }
}

class SummaryMetadata {
  constructor(qid, agents) {
    if (qid === undefined || !cmn.is_string(qid)) {
      throw new TypeError(`Expected argument qid to be of type string, got: ${qid}`);
    }

    if (agents === undefined || !cmn.is_array(agents)) {
      throw new TypeError(`Expected argument agents to be type array, got: ${agents}`);
    }

    this.qid = qid;
    this.aras = agents;
    this.timestamp = null
  }
}
