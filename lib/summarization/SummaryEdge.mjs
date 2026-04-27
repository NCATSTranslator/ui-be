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

  constructor(id, aras, support, is_root) {
    this.id = id;
    this.aras = aras || [];
    this.support = support || [];
    this.is_root = is_root || false;
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

  has_support() {
    return !cmn.is_array_empty(this.support);
  }

  is_inverted() {
    return this.metadata !== null
      && this.metadata.inverted_id !== null
  }

  merge(summary_edge) {
    if (this.metadata === null) {
      this.metadata = summary_edge.metadata;
    }
    if (summary_edge.metadata !== null) {
      this.metadata.edge_bindings.push(...summary_edge.metadata.edge_bindings);
    }
    this.aras.push(...summary_edge.aras);
    this.support.push(...summary_edge.support);
    this.is_root = this.is_root || summary_edge.is_root;
    this.description = this.description || summary_edge.description;
    this.knowledge_level = this.knowledge_level || summary_edge.knowledge_level;
    this.type = this.type || summary_edge.type;
    // TODO: This needs to be fixed
    // Change the type of the edge if we get support graphs
    if (summary_edge.type === trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT) {
      this.type = summary_edge.type;
    }
    this.subject = this.subject || summary_edge.subject;
    this.predicate = this.predicate || summary_edge.predicate;
    this.object = this.object || summary_edge.object;
    this.predicate_url = this.predicate_url || summary_edge.predicate_url;
    this.provenance.push(...summary_edge.provenance);
    this.trials.push(...summary_edge.trials);
    Object.keys(summary_edge.publications).forEach((kl) => {
      if (!this.publications[kl]) {
        this.publications[kl] = [];
      }

      this.publications[kl].push(...summary_edge.publications[kl]);
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
