export { suite }

import * as test from "#test/lib/common.mjs";
import { load_biolink } from "#lib/biolink-model.mjs";

const suite = {
  tests: {
    gen_canonical_qualified_predicate: _test_gen_canonical_qualified_predicate(),
    get_kedge_qualifiers: _test_get_kedge_qualifiers()
  },
  skip: {
    gen_qualified_predicate: true,
    get_most_specific_predicate: true,
    is_predicate_inverted: true
  }
};

function _test_gen_canonical_qualified_predicate() {
  return test.make_function_test({
    "canonical_simple_predicate_unchanged": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:affects", null],
      "expected": "affects"
    },
    "noncanonical_simple_predicate_inverted_to_canonical": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:affected_by", null],
      "expected": "affects"
    },
    "symmetric_predicate_unchanged": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:related_to", null],
      "expected": "related to"
    },
    "canonical_qualified_predicate": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [
        "biolink:affects",
        {
          "qualified predicate": "causes",
          "object aspect qualifier": "activity",
          "object direction qualifier": "increased"
        }
      ],
      "expected": "causes increased activity of"
    },
    "causal_mechanism_special_case_canonical": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [
        "biolink:affects",
        { "causal mechanism qualifier": "inhibition" }
      ],
      "expected": "inhibits"
    }
  });
}

function _test_get_kedge_qualifiers() {
  return test.make_function_test({
    "qualifiers_keyed_by_type": {
      "args": [{
        subject: "CHEBI:1",
        predicate: "biolink:affects",
        object: "NCBIGene:1",
        qualifiers: [
          { qualifier_type_id: "biolink:qualified_predicate", qualifier_value: "biolink:causes" },
          { qualifier_type_id: "biolink:object_aspect_qualifier", qualifier_value: "activity_or_abundance" },
          { qualifier_type_id: "biolink:object_direction_qualifier", qualifier_value: "increased" }
        ]
      }],
      "expected": {
        "qualified predicate": "causes",
        "object aspect qualifier": "activity or abundance",
        "object direction qualifier": "increased"
      }
    },
    "empty_qualifiers": {
      "args": [{
        subject: "CHEBI:1",
        predicate: "biolink:affects",
        object: "NCBIGene:1",
        qualifiers: []
      }],
      "expected": null
    },
    "missing_qualifiers": {
      "args": [{
        subject: "CHEBI:1",
        predicate: "biolink:affects",
        object: "NCBIGene:1"
      }],
      "expected": null
    },
    "null_qualifiers": {
      "args": [{
        subject: "CHEBI:1",
        predicate: "biolink:affects",
        object: "NCBIGene:1",
        qualifiers: null
      }],
      "expected": null
    },
    "qualifier_without_value": {
      "args": [{
        subject: "CHEBI:1",
        predicate: "biolink:affects",
        object: "NCBIGene:1",
        qualifiers: [
          { qualifier_type_id: "biolink:object_aspect_qualifier" }
        ]
      }],
      "expected": null
    }
  });
}

function _test_biolink_config() {
  return {
    "version": "4.4.3",
    "support_deprecated_predicates": false,
    "infores_catalog": "infores-catalog-v1.1.8.json",
    "prefix_catalog": {
      "path": "prefix-catalog.json",
      "exclude": ["VANDF"]
    }
  };
}
