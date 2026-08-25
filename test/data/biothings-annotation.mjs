export { suite }

import * as test from "#test/lib/common.mjs";
import * as cmn from "#lib/common.mjs";
import { make_section, make_source, SOURCES } from "#lib/biothings-annotation.mjs";

const ANNOTATION_ID = "biothings_annotations";

const suite = {
  tests: {
    is_chemical: _test_is_chemical(),
    is_disease: _test_is_disease(),
    is_gene: _test_is_gene(),
    make_section: _test_make_section(),
    make_source: _test_make_source(),
    make_rule_collect_chemical_annotations: _test_make_rule_collect_chemical_annotations(),
    make_rule_collect_gene_annotations: _test_make_rule_collect_gene_annotations(),
    make_rule_collect_disease_annotations: _test_make_rule_collect_disease_annotations(),
    make_rule_collect_other_annotations: _test_make_rule_collect_other_annotations()
  },
  skip: {
    SOURCES: true
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
      args: [3, [{ id: "chembl", url: "https://www.ebi.ac.uk/chembl/" }]],
      expected: { value: 3, metadata: { sources: [{ id: "chembl", url: "https://www.ebi.ac.uk/chembl/" }] } }
    },
    "supports_no_sources": {
      args: [["a"], []],
      expected: { value: ["a"], metadata: { sources: [] } }
    }
  });
}

function _test_make_source() {
  return test.make_function_test({
    "identifies_the_source_and_links_to_the_entity": {
      args: [SOURCES.CHEBI, "CHEBI:001"],
      expected: { id: "chebi", url: "https://www.ebi.ac.uk/chebi/searchId.do?chebiId=CHEBI:001" }
    },
    "falls_back_to_the_source_url_without_an_id": {
      args: [SOURCES.CHEBI],
      expected: { id: "chebi", url: "https://www.ebi.ac.uk/chebi/" }
    },
    "falls_back_to_the_source_url_when_it_has_no_entity_link": {
      args: [SOURCES.PHARMGKB, "PA448497"],
      expected: { id: "pharmgkb", url: "https://www.pharmgkb.org/" }
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
            approval: make_section(3, [make_source(SOURCES.CHEMBL, "CHEMBL001")]),
            descriptions: make_section(["NCIT description", "CHEBI definition"], [make_source(SOURCES.NCIT, "C001"), make_source(SOURCES.CHEBI, "CHEBI:001")]),
            indications: make_section([{
              name: "disease one",
              ids: ["MONDO:001", "UMLS:001"],
              urls: ["https://kp.example/mondo001a", "https://kp.example/umls001", "https://kp.example/mondo001b"]
            }]),
            synonyms: make_section({ commercial: ["aspirin"], generic: ["acetylsalicylic acid"] }),
            roles: null,
            otc_status: make_section({ code: 2, label: "Over the Counter" }, [make_source(SOURCES.CHEMBL, "CHEMBL001")]),
            clinical_trials: make_section([
              { id: "NCT001", disease_ids: ["MONDO:001"] },
              { id: "NCT002", disease_ids: [] },
              { id: "NCT003", disease_ids: [] }
            ])
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
            { disease: { mondo: "MONDO:001", name: "disease one" }, status: "approved_for_condition", source_record_urls: ["https://kp.example/mondo001a"] },
            { disease: { umls: "UMLS:001", name: "disease one" }, status: "approved_for_condition", source_record_urls: ["https://kp.example/umls001"] },
            { disease: { mondo: "MONDO:001", name: "disease one" }, status: "approved_for_condition", source_record_urls: ["https://kp.example/mondo001b"] },
            { disease: { hp: "HP:001", name: "phenotype two" }, status: "not_approved_for_condition", source_record_urls: ["https://kp.example/hp001"] }
          ],
          unii: { ncit: "C001", ncit_description: "NCIT description" },
          chebi: { id: "CHEBI:001", definition: "CHEBI definition" },
          pharmgkb: { trade_names: ["Aspirin"] },
          ndc: [{ proprietaryname: "Aspirin", nonproprietaryname: "Acetylsalicylic Acid" }],
          clinical_trials: [
            { id: "NCT001", disease: { mondo: "MONDO:001", name: "disease one" } },
            { id: "NCT002", disease: { name: "disease two" } },
            { id: "NCT003" }
          ]
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
            synonyms: null,
            roles: null,
            otc_status: make_section({ code: 1, label: "Prescription" }, [make_source(SOURCES.CHEMBL)]),
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
            synonyms: null,
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
            descriptions: make_section(["A gene summary"], [make_source(SOURCES.NCBI_GENE, 1050)]),
            name: make_section("BRCA1"),
            species: make_section("Mouse", [make_source(SOURCES.NCBI_TAXONOMY, 10090)]),
            tdl: make_section(["Tclin"], [make_source(SOURCES.PHAROS, "P001")])
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
            name: make_section("GENE1"),
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
            descriptions: make_section(["Disease text "], [make_source(SOURCES.DISEASE_ONTOLOGY, "DOID:001")]),
            curies: make_section(["MESH:D001", "MESH:D002"], [make_source(SOURCES.MONDO, "MONDO:001"), make_source(SOURCES.DISEASE_ONTOLOGY, "DOID:001")]),
            synonyms: null,
            clinical_trials: null
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
            curies: null,
            synonyms: null,
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({}),
        target: {}
      },
      post: test.apply_rule
    },
    "synonyms_merge_both_sources_and_deduplicate": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: null,
            curies: null,
            synonyms: make_section(
              ["diabetes", "diabetes mellitus", "DM", "sugar diabetes"],
              []
            ),
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          mondo: {
            mondo: "MONDO:001",
            synonym: { exact: ["diabetes", "diabetes mellitus"], related: ["DM"] }
          },
          disease_ontology: {
            doid: "DOID:001",
            name: "diabetes mellitus",
            synonyms: { exact: ["diabetes", "sugar diabetes"] }
          }
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "synonyms_deduplicate_case_insensitively_keeping_first_casing": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: null,
            curies: null,
            synonyms: make_section(["DM", "Sugar Diabetes"], []),
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          mondo: {
            mondo: "MONDO:001",
            synonym: { exact: ["DM", "  dm  ", "Sugar Diabetes"], related: ["sugar diabetes", "   "] }
          }
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "synonyms_collected_from_array_shaped_disease_ontology": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: null,
            curies: null,
            synonyms: make_section(
              ["pyloric stenosis", "gastric outlet obstruction", "gastric outflow obstruction"],
              []
            ),
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          disease_ontology: [
            { doid: "DOID:001", name: "pyloric stenosis", synonyms: {} },
            { doid: "DOID:002", name: "gastric outlet obstruction", synonyms: { exact: ["gastric outflow obstruction"] } }
          ]
        }),
        target: {}
      },
      post: test.apply_rule
    },
    "synonyms_exclude_identifiers": {
      args: [],
      expected: {
        annotations: {
          disease: {
            descriptions: null,
            curies: make_section(["MESH:D001"], [make_source(SOURCES.MONDO, "MONDO:001")]),
            synonyms: make_section(["diabetes"], []),
            clinical_trials: null
          }
        }
      },
      context: {
        source: _make_attr_source({
          mondo: {
            mondo: "MONDO:001",
            synonym: { exact: ["diabetes"] },
            xrefs: { mesh: "D001", doid: ["DOID:9351"], umls: ["C0011849"] }
          }
        }),
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
