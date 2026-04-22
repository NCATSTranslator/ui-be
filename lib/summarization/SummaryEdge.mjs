export {
  SummaryEdge,
  SummaryEdgeCreationError
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi/core.mjs';
import * as taglib from '../taglib.mjs';

class SummaryEdge {
  static REQUIRED_PROPERTIES = Object.freeze([
    'id',
    'aras',
    'support',
    'is_root',
    'knowledge_level',
    'description',
    'type',
    'subject',
    'object',
    'predicate',
    'predicate_url',
    'provenance',
    'publications',
    'metadata',
    'trials',
    'tags'
  ]);

  static from_object(obj) {
    if (cmn.is_missing(obj) || typeof obj !== 'object') {
      throw new SummaryEdgeCreationError('SummaryEdge source must be an object');
    }
    cmn.require_defined(obj, SummaryEdgeCreationError, ...SummaryEdge.REQUIRED_PROPERTIES);
    const edge = new SummaryEdge(obj.id, obj.aras, obj.support, obj.is_root);
    edge.knowledge_level = obj.knowledge_level;
    edge.description = obj.description;
    edge.type = obj.type;
    edge.subject = obj.subject;
    edge.object = obj.object;
    edge.predicate = obj.predicate;
    edge.predicate_url = obj.predicate_url;
    edge.provenance = obj.provenance;
    edge.publications = obj.publications;
    edge.metadata = obj.metadata;
    edge.trials = obj.trials;
    taglib.tags_from_raw_obj(edge, obj.tags);
    return edge;
  }

  constructor(id, agents, supPaths, isRootPath) {
    this.id = id;
    this.aras = agents || [];
    this.support = supPaths || [];
    this.is_root = isRootPath || false;
    this.knowledge_level = null;
    this.description = null;
    this.type = null;
    this.subject = null;
    this.object = null;
    this.predicate = null;
    this.predicate_url = null;
    this.provenance = [];
    this.publications = {};
    this.metadata = null;
    this.trials = [];
    taglib.make_taggable(this);
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

  hasSupport() { return !cmn.is_array_empty(this.supPaths); }

  isInverted() {
    return this.metadata !== null
      && this.metadata.inverted_id !== null
  }

  extendAgents(agents) {
    this.aras = this.aras.concat(agents)
  }

  extendSupPaths(paths) {
    this.supPaths.push(...paths);
  }

  merge(smryEdge) {
    if (this.metadata === null) {
      this.metadata = smryEdge.metadata;
    }
    if (smryEdge.metadata !== null) {
      this.metadata.edge_bindings.push(...smryEdge.metadata.edge_bindings);
    }
    this.extendAgents(smryEdge.aras);
    this.extendSupPaths(smryEdge.supPaths);
    this.isRootPath = this.isRootPath || smryEdge.isRootPath;
    this.description = this.description || smryEdge.description;
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

  to_raw_obj() {
    return {
      id: this.id,
      aras: this.aras,
      support: this.support,
      is_root: this.is_root,
      knowledge_level: this.knowledge_level,
      description: this.description,
      type: this.type,
      subject: this.subject,
      object: this.object,
      predicate: this.predicate,
      predicate_url: this.predicate_url,
      provenance: this.provenance,
      publications: this.publications,
      metadata: this.metadata,
      trials: this.trials,
      tags: taglib.tags_to_raw_obj(this)
    };
  }
}

class SummaryEdgeCreationError extends Error {
  constructor(msg) {
    super(msg);
    this.name = 'SummaryEdgeCreationError';
  }
}
