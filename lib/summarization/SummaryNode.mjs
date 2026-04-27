export {
  SummaryNode,
  SummaryNodeCreationError
}

import * as cmn from '../common.mjs';
import * as taglib from '../taglib.mjs';
import * as bl from '../biolink-model.mjs';

class SummaryNode {
  static REQUIRED_PROPERTIES = Object.freeze([
    'id',
    'aras',
    'descriptions',
    'names',
    'types',
    'synonyms',
    'curies',
    'provenance',
    'tags'
  ]);

  static from_object(obj) {
    if (cmn.is_missing(obj) || typeof obj !== 'object') {
      throw new SummaryNodeCreationError('SummaryNode source must be an object');
    }
    cmn.require_defined(obj, SummaryNodeCreationError, ...SummaryNode.REQUIRED_PROPERTIES);
    const node = new SummaryNode(obj.id, obj.aras);
    node.descriptions = obj.descriptions;
    node.names = obj.names;
    node.types = obj.types;
    node.synonyms = obj.synonyms;
    node.curies = obj.curies;
    node.provenance = obj.provenance;
    taglib.tags_from_raw_obj(node, obj.tags);
    return node;
  }

  constructor(id, agents) {
    this.id = id;
    this.aras = agents || [];
    this.descriptions = [];
    this.names = [];
    this.types = [];
    this.synonyms = [];
    this.curies = [];
    this.provenance = [];
    taglib.make_taggable(this);
  }

  name() {
    if (!cmn.is_array_empty(this.names)) return this.names[0];
    if (!cmn.is_array_empty(this.curies)) return this.curies[0];
    return null;
  }

  get_specific_type() {
    if (!cmn.is_array_empty(this.types)) return bl.sanitizeBiolinkItem(this.types[0]);
    return 'Named Thing';
  }

  set_provenance(provenance) {
    this.provenance = [provenance];
  }

  to_raw_obj() {
    return {
      id: this.id,
      aras: this.aras,
      descriptions: this.descriptions,
      names: this.names,
      types: this.types,
      synonyms: this.synonyms,
      curies: this.curies,
      provenance: this.provenance,
      tags: taglib.tags_to_raw_obj(this)
    };
  }
}

class SummaryNodeCreationError extends Error {
  constructor(msg) {
    super(msg);
    this.name = 'SummaryNodeCreationError';
  }
}
