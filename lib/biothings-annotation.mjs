export {
  is_chemical,
  is_disease,
  is_gene,
  make_section,
  make_source,
  make_clinical_trials_section,
  make_rule_collect_chemical_annotations,
  make_rule_collect_gene_annotations,
  make_rule_collect_disease_annotations,
  make_rule_collect_other_annotations,
  SOURCES
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

function make_rule_collect_chemical_annotations() {
  return _make_annotation_rule([
    [_get_chemical_names, _assign_section("synonyms")],
    [_get_chemical_descriptions, _assign_section("descriptions")],
    [_get_chebi_roles, _assign_section("roles")],
    [_get_fda_approval, _assign_section("approval")],
    [_get_drug_indications, _assign_section("indications")],
    [_get_otc_status, _assign_section("otc_status")],
    [_get_clinical_trials, _assign_section("clinical_trials")]
  ], _make_chemical_annotation);
}

function make_rule_collect_gene_annotations() {
  return _make_annotation_rule([
    [_get_gene_description, _assign_section("descriptions")],
    [_get_gene_name, _assign_section("name")],
    [_get_gene_species, _assign_section("species")],
    [_get_gene_tdl, _assign_section("tdl")]
  ], _make_gene_annotation);
}

function make_rule_collect_disease_annotations() {
  return _make_annotation_rule([
    [_get_disease_descriptions, _assign_section("descriptions")],
    [_get_disease_mesh_curies, _assign_section("curies")],
    [_get_disease_synonyms, _assign_section("synonyms")]
  ], _make_disease_annotation);
}

function make_rule_collect_other_annotations() {
  throw new cmn.DeveloperError("#lib/biothings-annotation.mjs", "make_rule_collect_other_annotations", "Not implemented");
}

function _get(obj, kpath, fallback = null) {
  return cmn.json_get_from_kpath(obj, kpath, fallback);
}

function make_section(value, sources = null) {
  return { value: value, metadata: { sources: sources } };
}

function make_clinical_trials_section(trial_ids) {
  if (cmn.is_array_empty(trial_ids)) return _empty_section();
  return make_section(trial_ids);
}

function make_source(source, id = null) {
  const url = (!cmn.is_missing(id) && source.link) ? source.link(id) : source.url;
  return { id: source.id, url: url };
}

function _empty_section() {
  return null;
}

function _assign_section(field) {
  return (target, section) => {
    if (section !== null) {
      target[field] = section;
    }
    return target;
  };
}

function _get_from_sources(annotation, source_entries) {
  const values = [];
  const sources = [];
  for (const [path, source, id_path] of source_entries) {
    const val = _get(annotation, path);
    if (cmn.is_missing(val)) continue;
    values.push(...cmn.coerce_array(val));
    sources.push(make_source(source, id_path ? _get(annotation, id_path) : null));
  }
  return { values, sources };
}

function _get_match(annotation, matches, fallback = null) {
  return cmn.get_kpath_match(annotation, matches, fallback);
}

function _get_disease_descriptions(annotation) {
  const { values, sources } = _get_from_sources(annotation, [
    [_CONSTANTS.DISEASE.DESCRIPTION.MONDO, SOURCES.MONDO, _CONSTANTS.DISEASE.ID.MONDO],
    [_CONSTANTS.DISEASE.DESCRIPTION.DISEASE_ONTOLOGY, SOURCES.DISEASE_ONTOLOGY, _CONSTANTS.DISEASE.ID.DISEASE_ONTOLOGY]
  ]);
  if (cmn.is_array_empty(values)) return _empty_section();
  const descriptions = values.map(description => description.split("[")[0].replaceAll("\"", ""));
  return make_section(descriptions, sources);
}

function _get_disease_mesh_curies(annotation) {
  const { values, sources } = _get_from_sources(annotation, [
    [_CONSTANTS.DISEASE.MESH.MONDO, SOURCES.MONDO, _CONSTANTS.DISEASE.ID.MONDO],
    [_CONSTANTS.DISEASE.MESH.DISEASE_ONTOLOGY, SOURCES.DISEASE_ONTOLOGY, _CONSTANTS.DISEASE.ID.DISEASE_ONTOLOGY]
  ]);
  if (cmn.is_array_empty(values)) return _empty_section();
  return make_section(values.map(curie => `MESH:${curie}`), sources);
}

function _get_disease_synonyms(annotation) {
  const contributions = [
    _get_mondo_labels(annotation),
    _get_disease_ontology_labels(annotation)
  ];
  const synonyms = _distinct_labels(contributions.flatMap(contribution => contribution.labels));
  if (cmn.is_array_empty(synonyms)) return _empty_section();
  return make_section(synonyms,
    cmn.distinct_array(contributions.flatMap(contribution => contribution.sources ?? [])));
}

function _get_mondo_labels(annotation) {
  const labels = _collect_grouped_strings(_get(annotation, _CONSTANTS.DISEASE.SYNONYM.MONDO));
  if (cmn.is_array_empty(labels)) return { labels: [], sources: [] };
  return { labels: labels, sources: null };
}

function _get_disease_ontology_labels(annotation) {
  const entry_paths = _CONSTANTS.DISEASE.DISEASE_ONTOLOGY_ENTRY;
  const entries = cmn.coerce_array(_get(annotation, entry_paths.ROOT, []));
  const labels = [];
  let doid = null;
  for (const entry of entries) {
    if (cmn.is_missing(entry)) continue;
    const label = _get(entry, entry_paths.LABEL);
    if (!cmn.is_missing(label)) labels.push(label);
    labels.push(..._collect_grouped_strings(_get(entry, entry_paths.SYNONYM)));
    if (cmn.is_missing(doid)) doid = _get(entry, entry_paths.DOID);
  }
  if (cmn.is_array_empty(labels)) return { labels: [], sources: [] };
  return { labels: labels, sources: null };
}

function _collect_grouped_strings(grouped_strings) {
  if (cmn.is_missing(grouped_strings)) return [];
  const strings = [];
  for (const group of Object.values(grouped_strings)) {
    for (const value of cmn.coerce_array(group)) {
      if (typeof value === 'string') strings.push(value);
    }
  }
  return strings;
}

function _distinct_labels(labels) {
  const trimmed = labels.map(label => label.trim()).filter(label => label !== '');
  return cmn.distinct_array(trimmed, label => label.toLowerCase());
}

function _get_chemical_names(annotation) {
  const pharm_trade_names = cmn.coerce_array(_get(annotation, _CONSTANTS.CHEMICAL.NAME.PHARM.ID, []));
  const commercial = new Set(pharm_trade_names.map(name => name.toLowerCase()));
  const ndc_entries = _get(annotation, _CONSTANTS.CHEMICAL.NAME.NDC.ID, []);
  if (!cmn.is_array(ndc_entries)) {
    const commercial_list = [...commercial];
    commercial_list.push(...ndc_entries.proprietaryname.split(',').map(s => s.trim()));
    return make_section({
      commercial: commercial_list,
      generic: ndc_entries.nonproprietaryname.split(',').map(s => s.trim())
    });
  }
  const generic = new Set();
  let ndc_contributed = false;
  for (const entry of ndc_entries) {
    const commercial_name = _get(entry, _CONSTANTS.CHEMICAL.NAME.NDC.PROPRIETARY);
    const generic_name = _get(entry, _CONSTANTS.CHEMICAL.NAME.NDC.NONPROPRIETARY);
    if (!cmn.is_missing(commercial_name)) {
      commercial.add(commercial_name.toLowerCase());
      ndc_contributed = true;
    }
    if (!cmn.is_missing(generic_name)) {
      generic.add(generic_name.toLowerCase());
      ndc_contributed = true;
    }
  }
  if (commercial.size === 0 && generic.size === 0) return _empty_section();
  return make_section({
    commercial: [...commercial],
    generic: [...generic]
  });
}

function _get_chemical_descriptions(annotation) {
  const { values, sources } = _get_from_sources(annotation, [
    [_CONSTANTS.CHEMICAL.DESCRIPTION.NCIT, SOURCES.NCIT, _CONSTANTS.CHEMICAL.NCIT_CODE],
    [_CONSTANTS.CHEMICAL.DESCRIPTION.CHEBI, SOURCES.CHEBI, _CONSTANTS.CHEMICAL.CHEBI_ID]
  ]);
  if (cmn.is_array_empty(values)) return _empty_section();
  return make_section(values, sources);
}

function _get_chebi_roles(annotation) {
  let chebi_role_ids = _get(annotation, _CONSTANTS.CHEMICAL.CHEBI_ROLE);
  if (cmn.is_missing(chebi_role_ids)) return _empty_section();
  chebi_role_ids = cmn.coerce_array(chebi_role_ids);
  const roles = [];
  for (const id of chebi_role_ids) {
    const root_id = chebi.getHighLevelRole(id);
    if (!cmn.is_missing(root_id)) {
      roles.push({ id: root_id, name: chebi.role_id_to_name(root_id) });
    }
  }
  return make_section(roles);
}

function _get_fda_approval(annotation) {
  const approval = _get(annotation, _CONSTANTS.CHEMICAL.FDA);
  if (cmn.is_missing(approval)) return _empty_section();
  return make_section(approval, [make_source(SOURCES.CHEMBL, _get(annotation, _CONSTANTS.CHEMICAL.CHEMBL_ID))]);
}

function _get_drug_indications(annotation) {
  const approval_entries = _get(annotation, _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.ID);
  if (cmn.is_missing(approval_entries)) return _empty_section();
  const indications = new Map();
  for (const entry of approval_entries) {
    const status = _get(entry, _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.STATUS);
    if (status !== _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.APPROVED) continue;
    const indication = _clinical_approval_indication(entry);
    if (cmn.is_missing(indication)) continue;
    let existing = indications.get(indication.name);
    if (existing === undefined) {
      existing = { name: indication.name, ids: [], urls: [] };
      indications.set(indication.name, existing);
    }
    existing.ids.push(indication.id);
    existing.urls.push(...indication.urls);
  }
  if (indications.size === 0) return _empty_section();
  for (const indication of indications.values()) {
    indication.ids = cmn.distinct_array(indication.ids);
    indication.urls = cmn.distinct_array(indication.urls);
  }
  return make_section([...indications.values()]);
}

function _clinical_approval_indication(entry) {
  const disease = _get(entry, _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.DISEASE);
  if (cmn.is_missing(disease)) return null;
  let id = null;
  let name = null;
  for (const [key, value] of Object.entries(disease)) {
    if (key === _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.DISEASE_NAME) {
      name = value;
    } else {
      id = value;
    }
  }
  if (cmn.is_missing(id)) return null;
  const urls = _get(entry, _CONSTANTS.CHEMICAL.CLINICAL_APPROVAL.SOURCE_URLS);
  return { id: id, name: name ?? id, urls: cmn.is_missing(urls) ? [] : cmn.coerce_array(urls) };
}

function _get_otc_status(annotation) {
  const otc_status = _get(annotation, _CONSTANTS.CHEMICAL.OTC.ID);
  if (cmn.is_missing(otc_status)) return _empty_section();
  return make_section({
    code: otc_status,
    label: _CONSTANTS.CHEMICAL.OTC.MAP_ID_STRING(otc_status)
  }, [make_source(SOURCES.CHEMBL, _get(annotation, _CONSTANTS.CHEMICAL.CHEMBL_ID))]);
}

function _get_clinical_trials(annotation) {
  const clinical_trials = _get(annotation, _CONSTANTS.CHEMICAL.CT.ID);
  if (cmn.is_missing(clinical_trials)) return _empty_section();
  const trials = clinical_trials.map(record => {
    return { id: record.id, disease_ids: _clinical_trial_disease_ids(record) };
  });
  return make_section(trials);
}

function _clinical_trial_disease_ids(record) {
  const disease = _get(record, _CONSTANTS.CHEMICAL.CT.DISEASE);
  if (cmn.is_missing(disease)) return [];
  const ids = [];
  for (const [key, value] of Object.entries(disease)) {
    if (key === _CONSTANTS.CHEMICAL.CT.DISEASE_NAME) continue;
    if (typeof value === 'string') ids.push(value);
  }
  return ids;
}

function _get_gene_description(annotation) {
  const gene_summary = _get(annotation, _CONSTANTS.GENE.SUMMARY);
  if (cmn.is_missing(gene_summary)) return _empty_section();
  return make_section(cmn.coerce_array(gene_summary), [make_source(SOURCES.NCBI_GENE, _get(annotation, _CONSTANTS.GENE.ID))]);
}

function _get_gene_name(annotation) {
  const name = _get(annotation, _CONSTANTS.GENE.NAME);
  if (cmn.is_missing(name)) return _empty_section();
  return make_section(name);
}

function _get_gene_species(annotation) {
  const species = _species_id_to_string(_get(annotation, _CONSTANTS.GENE.TAXONOMY.ID));
  if (cmn.is_missing(species)) return _empty_section();
  return make_section(species, [make_source(SOURCES.NCBI_TAXONOMY, _get(annotation, _CONSTANTS.GENE.TAXONOMY.ID))]);
}

function _get_gene_tdl(annotation) {
  const tdl = _get(annotation, _CONSTANTS.GENE.TDL);
  if (cmn.is_missing(tdl)) return _empty_section();
  return make_section(cmn.coerce_array(tdl), [make_source(SOURCES.PHAROS, _get(annotation, _CONSTANTS.GENE.PHAROS_ID))]);
}

function _species_id_to_string(id) {
  return _CONSTANTS.GENE.TAXONOMY.MAP_ID_STRING[id] ?? null;
}

function _is_annotation_empty(node, type) {
  const annotation = _get(node, type);
  if (cmn.is_missing(annotation)) return true;
  for (const section of Object.values(annotation)) {
    if (!cmn.is_missing(section?.value)) return false;
  }
  return true;
}

function _is_type(node, type) {
  return !_is_annotation_empty(node, type);
}

function _make_annotation_rule(transform_update_pairs, annotation_constructor) {
  return trapi_rules.make_rule_transform_attribute_value({
    attr_id: _CONSTANTS.ANNOTATION_ID,
    transform: annotation => {
      return transform_update_pairs.map(tup => {
        return tup[0](annotation[0]);
      });
    },
    update: (target, vals) => {
      if (cmn.is_missing(target.annotations)) {
        target.annotations = {};
      }
      const annotation_instance = annotation_constructor(target.annotations);
      if (vals === null) return target;
      transform_update_pairs.forEach((tup, i) => {
        tup[1](annotation_instance, vals[i]);
      });
      return target;
    }
  });
}

function _make_chemical_annotation(annotation_target) {
  annotation_target.chemical = {
    approval: _empty_section(),
    descriptions: _empty_section(),
    indications: _empty_section(),
    synonyms: _empty_section(),
    roles: _empty_section(),
    otc_status: _empty_section(),
    clinical_trials: _empty_section()
  };
  return annotation_target.chemical;
}

function _make_disease_annotation(annotation_target) {
  annotation_target.disease = {
    descriptions: _empty_section(),
    curies: _empty_section(),
    synonyms: _empty_section(),
    clinical_trials: _empty_section()
  };
  return annotation_target.disease;
}

function _make_gene_annotation(annotation_target) {
  annotation_target.gene = {
    descriptions: _empty_section(),
    name: _empty_section(),
    species: _empty_section(),
    tdl: _empty_section()
  };
  return annotation_target.gene;
}

const SOURCES = Object.freeze({
  CHEBI: Object.freeze({
    id: "chebi",
    url: "https://www.ebi.ac.uk/chebi/",
    link: (id) => `https://www.ebi.ac.uk/chebi/searchId.do?chebiId=${id}`
  }),
  CHEMBL: Object.freeze({
    id: "chembl",
    url: "https://www.ebi.ac.uk/chembl/",
    link: (id) => `https://www.ebi.ac.uk/chembl/compound_report_card/${id}/`
  }),
  DISEASE_ONTOLOGY: Object.freeze({
    id: "disease_ontology",
    url: "https://disease-ontology.org/",
    link: (id) => `https://disease-ontology.org/?id=${id}`
  }),
  DRUG_APPROVALS: Object.freeze({
    id: "drug_approvals",
    url: "https://github.com/NCATSTranslator/Translator-All/wiki/Multiomics-Drug-Approvals-KP"
  }),
  MONDO: Object.freeze({
    id: "mondo",
    url: "https://obofoundry.org/ontology/mondo.html",
    link: (id) => `https://monarchinitiative.org/${id}`
  }),
  NCBI_GENE: Object.freeze({
    id: "ncbi_gene",
    url: "https://www.ncbi.nlm.nih.gov/gene/",
    link: (id) => `https://www.ncbi.nlm.nih.gov/gene/${id}`
  }),
  NCBI_TAXONOMY: Object.freeze({
    id: "ncbi_taxonomy",
    url: "https://www.ncbi.nlm.nih.gov/taxonomy/",
    link: (id) => `https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=${id}`
  }),
  NCIT: Object.freeze({
    id: "ncit",
    url: "https://ncithesaurus.nci.nih.gov/ncitbrowser/",
    link: (id) => `https://ncithesaurus.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&code=${id}`
  }),
  NDC: Object.freeze({
    id: "ndc",
    url: "https://www.fda.gov/drugs/drug-approvals-and-databases/national-drug-code-directory"
  }),
  PHARMGKB: Object.freeze({
    id: "pharmgkb",
    url: "https://www.pharmgkb.org/"
  }),
  PHAROS: Object.freeze({
    id: "tdl",
    url: "https://opendata.ncats.nih.gov/public/pharos/pharos400_readme.html#:~:text=Target%20Development%20Level%20Definitions"
  })
});

const _CONSTANTS = Object.freeze({
  ANNOTATION_ID: "biothings_annotations",
  CHEMICAL: Object.freeze({
    CT: Object.freeze({
      ID: ["clinical_trials"],
      DISEASE: ["disease"],
      DISEASE_NAME: "name"
    }),
    OTC: Object.freeze({
      ID: ["chembl", "availability_type"],
      MAP_ID_STRING: (otc_code) => {
        switch(otc_code) {
          case 2: return "Over the Counter";
          case 1: return "Prescription";
          case 0: return "Discontinued";
          case -2: return "Withdrawn";
          case -1: return "Unknown";
        }
        throw Error(`Unknown OTC Code: ${otc_code}`);
      }
    }),
    CLINICAL_APPROVAL: Object.freeze({
      ID: ["clinical_approval"],
      STATUS: ["status"],
      DISEASE: ["disease"],
      DISEASE_NAME: "name",
      SOURCE_URLS: ["source_record_urls"],
      APPROVED: "approved_for_condition"
    }),
    FDA: ["chembl", "max_phase"],
    CHEMBL_ID: ["chembl", "molecule_chembl_id"],
    CHEBI_ID: ["chebi", "id"],
    NCIT_CODE: ["unii", "ncit"],
    CHEBI_ROLE: ["chebi", "relationship", "has_role"],
    DESCRIPTION: Object.freeze({
      NCIT: ["unii", "ncit_description"],
      CHEBI: ["chebi", "definition"]
    }),
    NAME: {
      NDC: {
        ID: ["ndc"],
        PROPRIETARY: ["proprietaryname"],
        NONPROPRIETARY: ["nonproprietaryname"]
      },
      PHARM: {
        ID: ["pharmgkb", "trade_names"]
      }
    }
  }),
  DISEASE: Object.freeze({
    ID: Object.freeze({
      MONDO: ["mondo", "mondo"],
      DISEASE_ONTOLOGY: ["disease_ontology", "doid"]
    }),
    MESH: Object.freeze({
      MONDO: ["mondo", "xrefs", "mesh"],
      DISEASE_ONTOLOGY: ["disease_ontology", "xrefs", "mesh"]
    }),
    DESCRIPTION: Object.freeze({
      MONDO: ["mondo", "definition"],
      DISEASE_ONTOLOGY: ["disease_ontology", "def"]
    }),
    SYNONYM: Object.freeze({
      MONDO: ["mondo", "synonym"]
    }),
    DISEASE_ONTOLOGY_ENTRY: Object.freeze({
      ROOT: ["disease_ontology"],
      DOID: ["doid"],
      LABEL: ["name"],
      SYNONYM: ["synonyms"]
    })
  }),
  GENE: Object.freeze({
    ID: ["_id"],
    NAME: ["name"],
    SUMMARY: ["summary"],
    TDL: ["pharos", "tdl"],
    PHAROS_ID: ["pharos", "uniprot"],
    TAXONOMY: Object.freeze({
      ID: ["taxid"],
      MAP_ID_STRING: Object.freeze({
        7955: "Zebrafish",
        10090: "Mouse",
        10116: "Rat"
      })
    })
  }),
  TYPE: Object.freeze({
    CHEMICAL: ["annotations", "chemical"],
    DISEASE: ["annotations", "disease"],
    GENE: ["annotations", "gene"]
  })
});
