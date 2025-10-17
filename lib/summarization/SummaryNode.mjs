export { SummaryNode }

import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';

class SummaryNode {
  constructor(agents) {
    this.aras = agents || [];
    this.curies = [];
    this.descriptions = [];
    this.names = [];
    this.other_names = [];
    this.provenance = []
    this.tags = {};
    this.types = [];
    this.species = null;
  }

  name() {
    return cmn.isArrayEmpty(this.names) ? this.curies[0] : this.names[0];
  }

  get_specific_type() {
    // TODO: We should inject 'Named Thing' as a type if its empty
    if (cmn.isArrayEmpty(this.types)) {
      return 'Named Thing'; // TODO: Should be a biolink constant
    }

    return bl.sanitizeBiolinkItem(this.types[0]);
  }

  set_provenance(provenance) {
    this.provenance = [provenance];
  }
}

