export { suite }

import * as test from "#test/lib/common.mjs";
import { load_biolink } from "#lib/biolink-model.mjs";

const suite = {
  tests: {
    gen_canonical_qualified_predicate: _test_gen_canonical_qualified_predicate()
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
      "args": [{ predicate: "biolink:affects" }],
      "expected": "affects"
    },
    "noncanonical_simple_predicate_inverted_to_canonical": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{ predicate: "biolink:affected_by" }],
      "expected": "affects"
    },
    "symmetric_predicate_unchanged": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{ predicate: "biolink:related_to" }],
      "expected": "related to"
    },
    "canonical_qualified_predicate": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{
        predicate: "biolink:affects",
        qualifiers: [
          { qualifier_type_id: "biolink:qualified_predicate", qualifier_value: "biolink:causes" },
          { qualifier_type_id: "biolink:object_aspect_qualifier", qualifier_value: "activity" },
          { qualifier_type_id: "biolink:object_direction_qualifier", qualifier_value: "increased" }
        ]
      }],
      "expected": "causes increased activity of"
    },
    "causal_mechanism_special_case_canonical": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{
        predicate: "biolink:affects",
        qualifiers: [
          { qualifier_type_id: "biolink:causal_mechanism_qualifier", qualifier_value: "inhibition" }
        ]
      }],
      "expected": "inhibits"
    }
  });
}

function _test_biolink_config() {
  return {
    "version": "4.2.1",
    "support_deprecated_predicates": false,
    "infores_catalog": "infores-catalog-v1.1.4.json",
    "prefix_catalog": {
      "path": "prefix-catalog.json",
      "exclude": ["VANDF"]
    }
  };
}
