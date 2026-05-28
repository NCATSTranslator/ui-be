export { suite }

import * as test from "#test/lib/common.mjs";
import * as cmn from "#lib/common.mjs";

const ANNOTATION_ID = "biothings_annotations";

const suite = {
  tests: {
    is_chemical: _test_is_chemical(),
    is_disease: _test_is_disease(),
    is_gene: _test_is_gene(),
    make_rule_collect_chemical_annotations: _test_make_rule_collect_chemical_annotations(),
    make_rule_collect_gene_annotations: _test_make_rule_collect_gene_annotations(),
    make_rule_collect_disease_annotations: _test_make_rule_collect_disease_annotations(),
    make_rule_collect_other_annotations: _test_make_rule_collect_other_annotations()
  }
};

function _test_is_chemical() {
  return test.make_function_test({
    "chemical_with_populated_field_is_chemical": {
      args: [{ annotations: { chemical: { approval: 3 } } }],
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
      args: [{ annotations: { disease: { descriptions: ["d"] } } }],
      expected: false
    }
  });
}

function _test_is_disease() {
  return test.make_function_test({
    "disease_with_populated_field_is_disease": {
      args: [{ annotations: { disease: { descriptions: ["d"] } } }],
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
      args: [{ annotations: { gene: { name: "BRCA1" } } }],
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

function _test_make_rule_collect_chemical_annotations() {
  return test.make_function_test({
    "full_chemical_annotation": {
      args: [],
      expected: {
        annotations: {
          chemical: {
            approval: 3,
            descriptions: ["NCIT description"],
            indications: ["D000001"],
            other_names: { commercial: ["aspirin"], generic: ["acetylsalicylic acid"] },
            roles: null,
            otc_status: { code: 2, label: "Over the Counter" },
            clinical_trials: ["NCT001"]
          }
        }
      },
      context: {
        source: _make_attr_source({
          chembl: {
            availability_type: 2,
            max_phase: 3,
            drug_indications: [
              { mesh_id: "D000001" },
              { mesh_id: false }
            ]
          },
          unii: { ncit_description: "NCIT description" },
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
            otc_status: { code: 1, label: "Prescription" },
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
            descriptions: ["A gene summary"],
            name: "BRCA1",
            species: "Mouse",
            tdl: "Tclin"
          }
        }
      },
      context: {
        source: _make_attr_source({
          summary: "A gene summary",
          name: "BRCA1",
          taxid: 10090,
          pharos: { tdl: "Tclin" }
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
            name: "GENE1",
            species: null,
            tdl: null
          }
        }
      },
      context: {
        source: _make_attr_source({
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
            descriptions: ["Disease text "],
            curies: ["MESH:D001", "MESH:D002"]
          }
        }
      },
      context: {
        source: _make_attr_source({
          disease_ontology: {
            def: '"Disease text" [SOURCE:123]',
            xrefs: { mesh: "D002" }
          },
          mondo: {
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
