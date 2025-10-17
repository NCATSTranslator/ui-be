export {
  CONSTANTS,
  SummaryNode
}

import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';

const CONSTANTS = Object.freeze({
  TYPE: {
    GENE: "gene",
    DISEASE: "disease",
    DRUG: "drug"
  }
});

class SummaryNode {
  constructor(agents) {
    this.aras = agents || [];
    this.descriptions = [];
    this.names = [];
    this.types = [];
    this.synonyms = [];
    this.curies = [];
    this.provenance = [];
    this.tags = {};
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

  set_specific_node(type, kwargs) {
    this[type] = _CONSTRUCTOR_MAP[type](kwargs);
  }
}

class _GeneNode {
  constructor(init = {}) {
    const {tdl, species, name} = init;
    if (tdl === undefined || species === undefined || name === undefined) {
      throw new cmn.DeveloperError('SummaryNode', '_GeneNode constructor', `Unexpected arguments given: ${JSON.stringify(init)}`);
    }
    this.tdl = tdl;
    this.species = species;
    this.name = name;
  }
}

class _DiseaseNode {
  constructor(init = {}) {
    const {clinical_trials} = init;
    if (clinical_trials === undefined) {
      throw new cmn.DeveloperError('SummaryNode', '_DiseaseNode constructor', `Unexpected arguments given: ${JSON.stringify(init)}`);
    }
    this.clinical_trials = clinical_trials;
  }
}

class _DrugNode {
  constructor(init = {}) {
    const {commercial_names, generic_names, clinical_trials, chemical_classes} = init;
    if (commercial_names === undefined
        || generic_names === undefined
        || clinical_trials === undefined
        || chemical_classes === undefined) {
      throw new cmn.DeveloperError('SummaryNode', '_DrugNode constructor', `Unexpected arguments given: ${JSON.stringify(init)}`);
    }
    this.commercial_names = commercial_names;
    this.generic_names = generic_names;
    this.clinical_trials = clinical_trials;
    this.chemical_classes = chemical_classes;
  }
}

const _CONSTRUCTOR_MAP = Object.freeze({
  [CONSTANTS.TYPE.GENE]:    _GeneNode,
  [CONSTANTS.TYPE.DISEASE]: _DiseaseNode,
  [CONSTANTS.TYPE.DRUG]:    _DrugNode
});

