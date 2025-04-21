export { SummaryEdge }

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';

class SummaryEdge {
  constructor(agents, supPaths, isRootPath) {
    this.aras = agents || [];
    this.support = supPaths || [];
    this.is_root = isRootPath || false;
    this.knowledge_level = null;
    this.type = null;
    this.subject = null;
    this.object = null;
    this.predicate = null;
    this.predicate_url = null;
    this.provenance = [];
    this.publications = {};
    this.metadata = null;
    this.trials = [];
  }

  // TODO: Should this be a property of the path instead of the edge?
  get isRootPath() { return this.is_root; }
  set isRootPath(isRootPath) { this.is_root = isRootPath; }
  get supPaths() { return this.support; }
  set supPaths(supPaths) { this.support = supPaths; }
  get knowledgeLevel() { return this.knowledge_level; }
  set knowledgeLevel(kl) { this.knowledge_level = kl; }
  get predUrl() { return this.predicate_url; }
  set predUrl(predUrl) { this.predicate_url = predUrl; }

  hasSupport() { return !cmn.isArrayEmpty(this.supPaths); }

  isInverted() {
    return this.metadata !== null
      && this.metadata.inverted_id !== null
  }

  extendAgents(agents) {
    this.aras.concat(agents)
  }

  extendSupPaths(paths) {
    this.supPaths.push(...paths);
  }

  merge(smryEdge) {
    if (this.metadata === null) {
      this.metadata = smryEdge.metadata;
    } else if (this.metadata.inverted_id !== smryEdge.metadata.inverted_id) {
      throw Error(`Metadata when merging edges is different\n  ${JSON.stringify(this.metadata,null,2)}\n  ${JSON.stringify(smryEdge.metadata,null,2)}`);
    }
    if (smryEdge.metadata !== null) {
      this.metadata.edge_bindings.push(...smryEdge.metadata.edge_bindings);
    }
    this.extendAgents(smryEdge.aras);
    this.extendSupPaths(smryEdge.supPaths);
    this.isRootPath = this.isRootPath || smryEdge.isRootPath;
    this.knowledgeLevel = this.knowledgeLevel || smryEdge.knowledgeLevel;
    this.type = this.type || smryEdge.type;
    // TODO: This needs to be fixed
    // Change the type of the edge if we get support graphs
    if (smryEdge.type === trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT) {
      this.type = smryEdge.type;
    }
    this.subject = this.subject || smryEdge.subject;
    this.predicate = this.predicate || smryEdge.predicate;
    this.object = this.object || smryEdge.object;
    this.predUrl = this.predUrl || smryEdge.predUrl;
    this.provenance.push(...smryEdge.provenance);
    this.trials.push(...smryEdge.trials);
    Object.keys(smryEdge.publications).forEach((kl) => {
      if (!this.publications[kl]) {
        this.publications[kl] = [];
      }

      this.publications[kl].push(...smryEdge.publications[kl]);
    });

    return this
  }
}

