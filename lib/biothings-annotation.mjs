export {
  is_chemical,
  is_disease,
  is_gene,
  make_rule_collect_chemical_attributes,
  make_rule_collect_gene_attributes,
  make_rule_collect_disease_attributes,
  make_rule_collect_other_attributes
}

import * as cmn from './common.mjs';
import * as chebi from './chebi.mjs';

function is_chemical(node) {
  return _is_type(node, _CONSTANTS.TYPE.CHEMICAL);
}

function is_disease(node) {
  return _is_type(node, _CONSTANTS.TYPE.DISEASE);
}

function is_gene(node) {
  return _is_type(node, _CONSTANTS.TYPE.GENE);
}

function _get(annotation, kpath, fallback = null) {
  return cmn.jsonGetFromKpath(annotation, kpath, fallback);
}

function getDiseaseDescription(annotation) {
  let description = cmn.jsonGetFromKpath(annotation, ['disease_ontology', 'def'], null);
  if (description !== null) {
    description = description.split('[')[0];
  }

  return description;
}

function getDiseaseMeshCuries(annotation) {
  const paths = [
    ['mondo', 'xrefs', 'mesh'],
    ['disease_ontology', 'xrefs', 'mesh']
  ];
  const curies = [];
  for (const path of paths) {
    const curie = cmn.jsonGetFromKpath(annotation, path, null);
    if (curie) {
      curies.push(`MESH:${curie}`);
    }
  }
  return curies;
}

function getChemicalNames(annotation) {
  const ndc = cmn.jsonGetFromKpath(annotation, ['ndc'], []);
  const commercialNames = new Set();
  const genericNames = new Set();
  for (const entry of ndc) {
    let commercialName = cmn.jsonGet(entry, 'proprietaryname', null);
    let genericName = cmn.jsonGet(entry, 'nonproprietaryname', null);
    if (commercialName !== null) {
      commercialNames.add(commercialName.toLowerCase());
    }

    if(genericName !== null) {
      genericNames.add(genericName.toLowerCase());
    }
  }

  return {
    commercial: [...commercialNames],
    generic: [...genericNames]
  };
}

function getChemicalDescription(annotation) {
  let description = cmn.jsonGetFromKpath(annotation, ['unii', 'ncit_description'], null);
  if (description === null) {
    description = cmn.jsonGetFromKpath(annotation, ['chebi', 'definition'], null);
  }
  return description;
}

function getChemicalChebiRoles(annotation) {
  let ids = cmn.jsonGetFromKpath(annotation, ['chebi', 'relationship', 'has_role'], null);
  if (ids === null) {
    return null;
  }

  if (!cmn.is_array(ids)) {
    ids = [ids];
  }

  const roles = [];
  ids.forEach((id) => {
    const highLevelId = chebi.getHighLevelRole(id);
    if (highLevelId !== null) {
      roles.push({ id: highLevelId, name: chebi.getName(highLevelId) });
    }
  });

  return roles;
}

function _get_chemical_fda_approval(annotation) {
  return _get(annotation, _CONSTANTS.CHEMICAL.FDA, 0);
}

function _get_chemical_drug_indications(annotation) {
  const indication_entries = _get(annotation, _CONSTANTS.CHEMICAL.INDICATION.ID, []);
  const indications = [];
  for (const entry of indication_entries) {
    const id = _get(entry, _CONSTANTS.CHEMICAL.INDICATION.ENTRY_ID, false);
    if (id) {
      indications.push(id);
    }
  }
  return indications;
}

function _get_gene_chemical_otc_status(annotation) {
  const otc_id = _get(annotation, _CONSTANTS.CHEMICAL.OTC.ID, -1);
  return _CONSTANTS.CHEMICAL.OTC.MAP_ID_TAG[otc_id] ?? null;
}

function _get_gene_description(annotation) {
  return _get(annotation, _CONSTANTS.GENE.SUMMARY);
}

function _get_gene_species(annotation) {
  return _species_id_to_string(_get(annotation, _CONSTANTS.GENE.TAXONOMY.ID));
}

function _get_gene_tdl(annotation) {
  return _get(annotation, _CONSTANTS.GENE.TDL);
}

function _species_id_to_string(id) {
  return _CONSTANTS.GENE.TAXONOMY.MAP_ID_STRING[id] ?? null;
}

function _is_type(node, type) {
  // TODO: Implement
  throw new cmn.DeveloperError('lib/biothings-annotations.mjs', '_is_type', 'Not Implemented');
}

const _CONSTANTS = Object.freeze({
  TYPE: Object.freeze({
    DISEASE: [['disease_ontology']],
    GENE: [['symbol']],
    CHEMICAL: [['chebi'],['chembl'],['ndc']]
  }),
  GENE: Object.freeze({
    TDL: ['pharos', 'tdl'],
    TAXONOMY: Object.freeze({
      ID: ['taxid'],
      MAP_ID_STRING: Object.freeze({
        7955: 'Zebrafish',
        10090: 'Mouse',
        10116: 'Rat'
      })
    }),
    SUMMARY: ['summary']
  }),
  CHEMICAL: Object.freeze({
    OTC: Object.freeze({
      ID: ['chembl', 'availability_type'],
      MAP_ID_TAG: Object.freeze({
        2: new taglib.Tag({id: 'r/otc/t', name: 'Over the counter'}),
        1: new taglib.Tag({id: 'r/otc/f', name: 'Prescription only'}),
        0: new taglib.Tag({id: 'r/otc/d', name: 'Discontinued'}),
        -1: new taglib.Tag({id: 'r/otc/o', name: 'Other'}),
        -2: new taglib.Tag({id: 'r/otc/w', name: 'Withdrawn'})
      })
    }),
    INDICATION: Object.freeze({
      ID: ['chembl', 'drug_indications'],
      ENTRY_ID: ['mesh_id']
    }),
    FDA: ['chembl', 'max_phase']
  }
});
