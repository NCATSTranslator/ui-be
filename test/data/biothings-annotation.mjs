export { suite }

import * as test from "#test/lib/common.mjs";
import * as cmn from "#lib/common.mjs";
import { make_section } from "#lib/biothings-annotation.mjs";

const ANNOTATION_ID = "biothings_annotations";

const SRC = {
  CHEBI: (id) => ({ name: "Chemical Entity of Biological Interest", url: `https://www.ebi.ac.uk/chebi/searchId.do?chebiId=${id}` }),
  CHEMBL: (id) => ({ name: "ChEMBL", url: `https://www.ebi.ac.uk/chembl/compound_report_card/${id}/` }),
  CLINICAL_TRIALS: { name: "ClinicalTrials.gov", url: "https://clinicaltrials.gov/" },
  DISEASE_ONTOLOGY: (id) => ({ name: "Disease Ontology", url: `https://disease-ontology.org/?id=${id}` }),
  DRUG_APPROVALS: { name: "Drug Approvals Knowledge Provider", url: "https://github.com/NCATSTranslator/Translator-All/wiki/Multiomics-Drug-Approvals-KP" },
  MONDO: (id) => ({ name: "MONDO Disease Ontology", url: `https://monarchinitiative.org/${id}` }),
  NCBI_GENE: (id) => ({ name: "National Center for Biotechnology Information Gene", url: `https://www.ncbi.nlm.nih.gov/gene/${id}` }),
  NCBI_TAXONOMY: (id) => ({ name: "NCBI Taxonomy", url: `https://www.ncbi.nlm.nih.gov/Taxonomy/Browser/wwwtax.cgi?id=${id}` }),
  NCIT: (id) => ({ name: "National Cancer Institute Thesaurus", url: `https://ncithesaurus.nci.nih.gov/ncitbrowser/ConceptReport.jsp?dictionary=NCI_Thesaurus&code=${id}` }),
  NDC: { name: "National Drug Code Directory", url: "https://www.fda.gov/drugs/drug-approvals-and-databases/national-drug-code-directory" },
  PHARMGKB: { name: "Pharmacogenomics Knowledgebase", url: "https://www.pharmgkb.org/" },
  PHAROS: (id) => ({ name: "Pharos", url: `https://pharos.nih.gov/targets/${id}` })
};

const suite = {
  tests: {
    is_chemical: _test_is_chemical(),
    is_disease: _test_is_disease(),
    is_gene: _test_is_gene(),
    make_section: _test_make_section(),
    make_rule_collect_chemical_annotations: _test_make_rule_collect_chemical_annotations(),
    make_rule_collect_gene_annotations: _test_make_rule_collect_gene_annotations(),
    make_rule_collect_disease_annotations: _test_make_rule_collect_disease_annotations(),
    make_rule_collect_other_annotations: _test_make_rule_collect_other_annotations()
  }
};

function _test_is_chemical() {
  return test.make_function_test({
    "chemical_with_populated_field_is_chemical": {
      args: [{ annotations: { chemical: { approval: make_section(3, []) } } }],
      expected: true
    },
    "chemical_with_all_null_fields_is_not_chemical": {
      args: [{ annotations: { chemical: { approval: null, descriptions: null } } }],
      expected: false
    },
    "missing_annotations_is_not_chemical": {
      args: [{}],
      expected: false
    },
    "disease_annotation_is_not_chemical": {
      args: [{ annotations: { disease: { descriptions: make_section(["d"], []) } } }],
      expected: false
    }
  });
}

function _test_is_disease() {
  return test.make_function_test({
    "disease_with_populated_field_is_disease": {
      args: [{ annotations: { disease: { descriptions: make_section(["d"], []) } } }],
      expected: true
    },
    "disease_with_all_null_fields_is_not_disease": {
      args: [{ annotations: { disease: { descriptions: null, curies: null } } }],
      expected: false
    },
    "missing_annotations_is_not_disease": {
      args: [{}],
      expected: false
    }
  });
}

function _test_is_gene() {
  return test.make_function_test({
    "gene_with_populated_field_is_gene": {
      args: [{ annotations: { gene: { name: make_section("BRCA1", []) } } }],
      expected: true
    },
    "gene_with_all_null_fields_is_not_gene": {
      args: [{ annotations: { gene: { name: null, species: null, tdl: null, descriptions: null } } }],
      expected: false
    },
    "missing_annotations_is_not_gene": {
      args: [{}],
      expected: false
    }
  });
}

function _test_make_section() {
  return test.make_function_test({
    "wraps_value_and_sources_as_metadata": {
      args: [3, [{ name: "ChEMBL", url: "https://www.ebi.ac.uk/chembl/" }]],
      expected: { value: 3, metadata: { sources: [{ name: "ChEMBL", url: "https://www.ebi.ac.uk/chembl/" }] } }
    },
    "supports_no_sources": {
      args: [["a"], []],
      expected: { value: ["a"], metadata: { sources: [] } }
    }
  });
}

function _test_make_rule_collect_chemical_annotations() {
  return test.make_function_test({
    "full_chemical_annotation": {
      args: [],
      expected: {
        annotations: {
          chemical: {
            approval: make_section(3, [SRC.CHEMBL("CHEMBL001")]),
            descriptions: make_section(["NCIT description", "CHEBI definition"], [SRC.NCIT("C001"), SRC.CHEBI("CHEBI:001")]),
            indications: make_section([{ name: "disease one", ids: ["MONDO:001", "UMLS:001"] }], [SRC.DRUG_APPROVALS]),
            other_names: make_section({ commercial: ["aspirin"], generic: ["acetylsalicylic acid"] }, [SRC.PHARMGKB, SRC.NDC]),
            roles: null,
            otc_status: make_section({ code: 2, label: "Over the Counter" }, [SRC.CHEMBL("CHEMBL001")]),
            clinical_trials: make_section(["NCT001"], [SRC.CLINICAL_TRIALS])
          }
        }
      },
      context: {
        source: _make_attr_source({
          chembl: {
            molecule_chembl_id: "CHEMBL001",
            availability_type: 2,
            max_phase: 3
          },
          clinical_approval: [
            { disease: { mondo: "MONDO:001", name: "disease one" }, status: "approved_for_condition" },
            { disease: { umls: "UMLS:001", name: "disease one" }, status: "approved_for_condition" },
            { disease: { mondo: "MONDO:001", name: "disease one" }, status: "approved_for_condition" },
            { disease: { hp: "HP:001", name: "phenotype two" }, status: "not_approved_for_condition" }
          ],
          unii: { ncit: "C001", ncit_description: "NCIT description" },
          chebi: { id: "CHEBI:001", definition: "CHEBI definition" },
          pharmgkb: { trade_names: ["Aspirin"] },
          ndc: [{ proprietaryname: "Aspirin", nonproprietaryname: "Acetylsalicylic Acid" }],
          clinical_trials: [{ id: "NCT001" }]
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "otc_status_prescription_label": {
      args: [],
      expected: {
        annotations: {
          chemical: {
            approval: null,
            descriptions: null,
            indications: null,
            other_names: null,
            roles: null,
            otc_status: make_section({ code: 1, label: "Prescription" }, [{ name: "ChEMBL", url: "https://www.ebi.ac.uk/chembl/" }]),
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          chembl: { availability_type: 1 }
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "missing_biothings_attribute_produces_null_chemical_record": {
      args: [],
      expected: {
        annotations: {
          chemical: {
            approval: null,
            descriptions: null,
            indications: null,
            other_names: null,
            roles: null,
            otc_status: null,
            clinical_trials: null
          }
        }
      },
      context: {
        source: { attributes: [] },
        target: {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_collect_gene_annotations() {
  return test.make_function_test({
    "full_gene_annotation_known_species": {
      args: [],
      expected: {
        annotations: {
          gene: {
            descriptions: make_section(["A gene summary"], [SRC.NCBI_GENE(1050)]),
            name: make_section("BRCA1", [SRC.NCBI_GENE(1050)]),
            species: make_section("Mouse", [SRC.NCBI_TAXONOMY(10090)]),
            tdl: make_section(["Tclin"], [SRC.PHAROS("P001")])
          }
        }
      },
      context: {
        source: _make_attr_source({
          _id: 1050,
          summary: "A gene summary",
          name: "BRCA1",
          taxid: 10090,
          pharos: { tdl: "Tclin", uniprot: "P001" }
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "unknown_taxid_yields_null_species": {
      args: [],
      expected: {
        annotations: {
          gene: {
            descriptions: null,
            name: make_section("GENE1", [SRC.NCBI_GENE(2222)]),
            species: null,
            tdl: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          _id: 2222,
          name: "GENE1",
          taxid: 9606
        }),
        target: {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_collect_disease_annotations() {
  return test.make_function_test({
    "description_strips_brackets_and_quotes": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: make_section(["Disease text "], [SRC.DISEASE_ONTOLOGY("DOID:001")]),
            curies: make_section(["MESH:D001", "MESH:D002"], [SRC.MONDO("MONDO:001"), SRC.DISEASE_ONTOLOGY("DOID:001")])
          }
        }
      },
      context: {
        source: _make_attr_source({
          disease_ontology: {
            doid: "DOID:001",
            def: '"Disease text" [SOURCE:123]',
            xrefs: { mesh: "D002" }
          },
          mondo: {
            mondo: "MONDO:001",
            xrefs: { mesh: "D001" }
          }
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "missing_disease_fields_yield_nulls": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: null,
            curies: null
          }
        }
      },
      context: {
        source: _make_attr_source({}),
        target: {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_collect_other_annotations() {
  return test.make_function_test({
    "throws_developer_error_because_not_implemented": {
      args: [],
      expected: Error
    }
  });
}

function _make_attr_source(annotation) {
  return {
    attributes: [{
      attribute_type_id: ANNOTATION_ID,
      value: [annotation]
    }]
  };
}
