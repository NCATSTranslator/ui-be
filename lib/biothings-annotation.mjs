export {
  is_chemical,
  is_disease,
  is_gene,
  make_rule_collect_chemical_annotations,
  make_rule_collect_gene_annotations,
  make_rule_collect_disease_annotations,
  make_rule_collect_other_annotations
}

import * as cmn from "#lib/common.mjs";
import * as chebi from "#lib/chebi.mjs";
import * as trapi_rules from "#lib/trapi/property-rules.mjs";

function is_chemical(node) {
  return _is_type(node, _CONSTANTS.TYPE.CHEMICAL);
}

function is_disease(node) {
  return _is_type(node, _CONSTANTS.TYPE.DISEASE);
}

function is_gene(node) {
  return _is_type(node, _CONSTANTS.TYPE.GENE);
}

// TODO: fill out updates
function make_rule_collect_chemical_annoations() {
  return _make_annotation_rule([
    [_get_chemical_names, (target, names) => {
    }],
    [_get_chemical_descriptions, (target, description) => {
    }],
    [_get_chebi_roles, (target, roles) => {
    }],
    [_get_fda_approval, (target, approval) => {
    }],
    [_get_drug_indications, (target, indications) => {
    }],
    [_get_otc_status, (target, otc_status) => {
    }],
  ]);
}

// TODO: Fill out updaters
function make_rule_collect_gene_annotations() {
  return _make_annotation_rule([
    [_get_gene_description, (target, description) => {
    }],
    [_get_gene_species, (target, species) => {
    }],
    [_get_tdl, (target, tdl) => {
    }]
  ]);
}

// TODO: Fill out updaters
function make_rule_collect_disease_annotations() {
  return _make_annotation_rule([
    [_get_disease_description, (target, description) => {
    }],
    [_get_disease_mesh_curies, (target, curies) => {
    }]
  ]);
}

function _get(annotation, kpath, fallback = null) {
  return cmn.jsonGetFromKpath(annotation, kpath, fallback);
}

function _get_match(annotation, matches, fallback = null) {
  return cmn.get_kpath_match(annotation, matches, fallback);
}

function _get_many(annotation, matches) {
  const vals = [];
  for (const match of matches) {
    const val = _get(annotation, match, fallback);
    if (cmn.is_missing(val)) continue;
    vals.push(val);
  }
  return vals;
}

function _get_disease_description(annotation) {
  const description = _get(annotation, _CONSTANTS.DISEASE.DESCRIPTION);
  if (cmn.is_missing(description)) return null;
  return description.split("[")[0];
}

function _get_disease_mesh_curies(annotation) {
  const paths = [
    _CONSTANTS.DISEASE.MESH.MONDO,
    _CONSTANTS.DISEASE.MESH.DISEASE_ONTOLOGY
  ];
  const curies = _get_many(annotation, paths)
  return curies.map(curie => `MESH:${curie}`);
}

function _get_chemical_names(annotation) {
  const ndc_entries = _get(annotation, _CONSTANTS.CHEMICAL.NAME.ID, []);
  const commercial = new Set();
  const generic = new Set();
  for (const entry of ndc_entries) {
    const commercial_name = _get(entry, _CONSTANTS.CHEMICAL.NAME.PROPRIETARY);
    const generic_name = _get(entry, _CONSTANTS.CHEMICAL.NAME.NONPROPRIETARY);
    if (!cmn.is_missing(commercial_name)) {
      commercial.add(commercial_name.toLowerCase());
    }
    if (!cmn.is_missing(generic_name)) {
      generic.add(generic_name.toLowerCase());
    }
  }
  return {
    commercial: [...commercial],
    generic: [...generic]
  };
}

function _get_chemical_description(annotation) {
  const paths = [
    _CONSTANTS.CHEMICAL.DESCRIPTION.NCIT,
    _CONSTANTS.CHEMICAL.DESCRIPTION.CHEBI
  ];
  return _get_match(annotation, paths);
}

function _get_chebi_roles(annotation) {
  let chebi_role_ids = _get(annotation, _CONSTANTS.CHEMICAL.CHEBI_ROLE);
  if (chebi_role_ids === null) return null;
  chebi_role_ids = cmn.coerce_array(chebi_role_ids);
  const roles = [];
  for (const id of chebi_role_ids) {
    const root_id = chebi.getHighLevelRole(id);
    if (!cmn.is_missing(root_id)) {
      roles.push({ id: root_id, name: chebi.rold_id_to_name(root_id) });
    }
  }
  return roles;
}

function _get_fda_approval(annotation) {
  return _get(annotation, _CONSTANTS.CHEMICAL.FDA, 0);
}

function _get_drug_indications(annotation) {
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

function _get_otc_status(annotation) {
  const otc_id = _get(annotation, _CONSTANTS.CHEMICAL.OTC.ID, -1);
  return _CONSTANTS.CHEMICAL.OTC.MAP_ID_TAG[otc_id] ?? null;
}

function _get_gene_description(annotation) {
  return _get(annotation, _CONSTANTS.GENE.SUMMARY);
}

function _get_gene_species(annotation) {
  return _species_id_to_string(_get(annotation, _CONSTANTS.GENE.TAXONOMY.ID));
}

function _get_tdl(annotation) {
  return _get(annotation, _CONSTANTS.GENE.TDL);
}

function _species_id_to_string(id) {
  return _CONSTANTS.GENE.TAXONOMY.MAP_ID_STRING[id] ?? null;
}

function _is_type(node, type) {
  // TODO: Implement
  throw new cmn.DeveloperError("lib/biothings-annotations.mjs", "_is_type", "Not Implemented");
}

function _make_annotation_rule(transform_update_pairs) {
  return trapi_rules.make_rule_transform_attribute_value({
    attr_id: _CONSTANTS.ANNOTATIONS_KEY,
    transform: annotation => {
      return transform_update_pair.map(tup => {
        return tup[0](annotation);
      });
    },
    update: (target, vals) => {
      return transform_update_pair.map((tup, i) => {
        return tup[1](target, vals[i]);
      });
    }
  });
}

const _CONSTANTS = Object.freeze({
  ANNOTATION_KEY: "biothings_annotations",
  TYPE: Object.freeze({
    DISEASE: [["disease_ontology"]],
    GENE: [["symbol"]],
    CHEMICAL: [["chebi"],["chembl"],["ndc"]]
  }),
  GENE: Object.freeze({
    TDL: ["pharos", "tdl"],
    TAXONOMY: Object.freeze({
      ID: ["taxid"],
      MAP_ID_STRING: Object.freeze({
        7955: "Zebrafish",
        10090: "Mouse",
        10116: "Rat"
      })
    }),
    SUMMARY: ["summary"]
  }),
  CHEMICAL: Object.freeze({
    OTC: Object.freeze({
      ID: ["chembl", "availability_type"],
      MAP_ID_TAG: Object.freeze({
        2: new taglib.Tag({id: "r/otc/t", name: "Over the counter"}),
        1: new taglib.Tag({id: "r/otc/f", name: "Prescription only"}),
        0: new taglib.Tag({id: "r/otc/d", name: "Discontinued"}),
        -1: new taglib.Tag({id: "r/otc/o", name: "Other"}),
        -2: new taglib.Tag({id: "r/otc/w", name: "Withdrawn"})
      })
    }),
    INDICATION: Object.freeze({
      ID: ["chembl", "drug_indications"],
      ENTRY_ID: ["mesh_id"]
    }),
    FDA: ["chembl", "max_phase"],
    CHEBI_ROLE: ["chebi", "relationship", "has_role"],
    DESCRIPTION: Object.freeze({
      NCIT: ["unii", "ncit_description"],
      CHEBI: ["chebi", "definition"]
    }),
    NAME: {
      ID: ["ndc"],
      PROPRIETARY: ["proprietaryname"],
      NONPROPRIETARY: ["nonproprietaryname"]
    }
  },
  DISEASE: Object.freeze({
    MESH: Object.freeze({
      MONDO: ["mondo", "xrefs", "mesh"],
      DISEASE_ONTOLOGY: ["disease_ontology", "xrefs", "mesh"]
    }),
    DESCRIPTION: ["disease_ontology", "def"]
  })
});
