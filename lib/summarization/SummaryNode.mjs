export {
  SummaryNode
}

import * as cmn from '../common.mjs';
import * as taglib from '../taglib.mjs';
import * as bl from '../biolink-model.mjs';

class SummaryNode {
  constructor(agents) {
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
    // TODO: Should be a biolink constant
    if (!cmn.is_array_empty(this.types)) return bl.sanitizeBiolinkItem(this.types[0]);
    return 'Named Thing';
  }
}
