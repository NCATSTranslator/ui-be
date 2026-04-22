export {
  SummaryNode
}

import * as cmn from '../common.mjs';
import * as taglib from '../taglib.mjs';
import * as bl from '../biolink-model.mjs';

class SummaryNode {
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
    return cmn.is_array_empty(this.names) ? this.curies[0] : this.names[0];
  }

  get_specific_type() {
    // TODO: We should inject 'Named Thing' as a type if its empty
    if (cmn.is_array_empty(this.types)) {
      return 'Named Thing'; // TODO: Should be a biolink constant
    }

    return bl.sanitizeBiolinkItem(this.types[0]);
  }

  set_provenance(provenance) {
    this.provenance = [provenance];
  }
}
