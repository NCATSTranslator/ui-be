export { suite }

import * as test from "#test/lib/common.mjs";
import { load_biolink } from "#lib/biolink-model.mjs";

const suite = {
  tests: {
    sanitize_biolink_item: _test_sanitize_biolink_item(),
    tag_biolink: _test_tag_biolink(),
    biolinkify_class: _test_biolinkify_class(),
    split_curie: _test_split_curie(),
    get_curie_prefix: _test_get_curie_prefix(),
    pred_to_url: _test_pred_to_url(),
    class_rank: _test_class_rank(),
    is_biolink_pred: _test_is_biolink_pred(),
    is_deprecated_pred: _test_is_deprecated_pred(),
    invert_biolink_pred: _test_invert_biolink_pred(),
    get_predicate_description: _test_get_predicate_description(),
    get_node_type_description: _test_get_node_type_description(),
    is_valid_curie: _test_is_valid_curie(),
    curie_to_url: _test_curie_to_url(),
    curie_to_normalized_url: _test_curie_to_normalized_url(),
    infores_to_provenance: _test_infores_to_provenance(),
    source_to_provenance: _test_source_to_provenance(),
    biolink_class_cmp_fn: _test_biolink_class_cmp_fn(),
    MissingInforesError: _test_MissingInforesError()
  },
  skip: {
    load_biolink: true,
    deprecated_pred_to_pred_and_qualifiers: true
  }
};

function _test_sanitize_biolink_item() {
  return test.make_function_test({
    "biolinkified_predicate": {
      "args": ["biolink:treats"],
      "expected": "treats"
    },
    "biolinkified_underscored_predicate": {
      "args": ["biolink:related_to_at_instance_level"],
      "expected": "related to at instance level"
    },
    "bare_underscored_predicate": {
      "args": ["related_to"],
      "expected": "related to"
    },
    "already_sanitized": {
      "args": ["related to"],
      "expected": "related to"
    },
    "empty_string": {
      "args": [""],
      "expected": ""
    }
  });
}

function _test_tag_biolink() {
  return test.make_function_test({
    "spaces_become_underscores": {
      "args": ["related to"],
      "expected": "biolink:related_to"
    },
    "single_word": {
      "args": ["treats"],
      "expected": "biolink:treats"
    },
    "already_biolinkified_is_idempotent": {
      "args": ["biolink:treats"],
      "expected": "biolink:treats"
    },
    "already_biolinkified_with_spaces": {
      "args": ["biolink:related to"],
      "expected": "biolink:related_to"
    }
  });
}

function _test_biolinkify_class() {
  return test.make_function_test({
    "two_word_class": {
      "args": ["chemical entity"],
      "expected": "biolink:ChemicalEntity"
    },
    "single_word_class": {
      "args": ["disease"],
      "expected": "biolink:Disease"
    },
    "three_word_class": {
      "args": ["named thing"],
      "expected": "biolink:NamedThing"
    }
  });
}

function _test_split_curie() {
  return test.make_function_test({
    "simple_curie": {
      "args": ["CHEBI:1234"],
      "expected": ["CHEBI", "1234"]
    },
    "biolink_curie": {
      "args": ["biolink:treats"],
      "expected": ["biolink", "treats"]
    },
    "no_colon": {
      "args": ["abc"],
      "expected": ["abc"]
    },
    "multiple_colons": {
      "args": ["A:B:C"],
      "expected": ["A", "B", "C"]
    }
  });
}

function _test_get_curie_prefix() {
  return test.make_function_test({
    "simple_curie": {
      "args": ["CHEBI:1234"],
      "expected": "CHEBI"
    },
    "biolink_curie": {
      "args": ["biolink:treats"],
      "expected": "biolink"
    },
    "no_colon_returns_whole_string": {
      "args": ["abc"],
      "expected": "abc"
    }
  });
}

function _test_pred_to_url() {
  return test.make_function_test({
    "spaces_replaced_with_underscores": {
      "args": ["related to"],
      "expected": "https://biolink.github.io/biolink-model/related_to/"
    },
    "already_underscored": {
      "args": ["treats"],
      "expected": "https://biolink.github.io/biolink-model/treats/"
    },
    "multiword_predicate": {
      "args": ["related to at instance level"],
      "expected": "https://biolink.github.io/biolink-model/related_to_at_instance_level/"
    }
  });
}

function _test_class_rank() {
  const classes = {
    "biolink:Root": { is_a: null },
    "biolink:Mid": { is_a: "biolink:Root" },
    "biolink:Leaf": { is_a: "biolink:Mid" }
  };
  return test.make_function_test({
    "root_is_rank_zero": {
      "args": ["biolink:Root", classes],
      "expected": 0
    },
    "mid_is_rank_one": {
      "args": ["biolink:Mid", classes],
      "expected": 1
    },
    "leaf_is_rank_two": {
      "args": ["biolink:Leaf", classes],
      "expected": 2
    },
    "unknown_target_throws": {
      "args": ["biolink:Missing", classes],
      "expected": Error
    }
  });
}

function _test_is_biolink_pred() {
  return test.make_function_test({
    "known_predicate_sanitized": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["treats"],
      "expected": true
    },
    "known_predicate_biolinkified": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:treats"],
      "expected": true
    },
    "known_predicate_with_spaces": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["related to"],
      "expected": true
    },
    "unknown_predicate": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["this is not a predicate"],
      "expected": false
    }
  });
}

function _test_is_deprecated_pred() {
  return test.make_function_test({
    "no_map_loaded_returns_false": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["treats"],
      "expected": false
    },
    "no_map_loaded_unknown_pred_returns_false": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["bogus"],
      "expected": false
    }
  });
}

function _test_invert_biolink_pred() {
  return test.make_function_test({
    "asymmetric_pair_superclass_of": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["superclass of"],
      "expected": "subclass of"
    },
    "asymmetric_pair_reverse": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["subclass of"],
      "expected": "superclass of"
    },
    "symmetric_predicate_inverts_to_itself": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["related to"],
      "expected": "related to"
    },
    "biolinkified_input": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:superclass_of", true],
      "expected": "biolink:subclass_of"
    }
  });
}

function _test_get_predicate_description() {
  return test.make_function_test({
    "treats_description_starts_correctly": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["treats"],
      "expected": "*"
    },
    "biolinkified_input_works": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:related_to"],
      "expected": "*"
    },
    "unknown_predicate": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["unknown predicate"],
      "expected": null
    }
  });
}

function _test_get_node_type_description() {
  return test.make_function_test({
    "known_class": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:Entity"],
      "expected": "Root Biolink Model class for all things and informational relationships, real or imagined."
    },
    "unknown_class_returns_null": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:NotAClass"],
      "expected": null
    }
  });
}

function _test_is_valid_curie() {
  return test.make_function_test({
    "known_prefix": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["CHEBI:1234"],
      "expected": true
    },
    "unknown_prefix": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["NOTAPREFIX:1234"],
      "expected": false
    },
    "excluded_prefix": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["VANDF:1234"],
      "expected": false
    },
    "no_colon_is_invalid": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["bareword"],
      "expected": false
    },
    "non_string_is_invalid": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [12345],
      "expected": false
    }
  });
}

function _test_curie_to_url() {
  return test.make_function_test({
    "chebi_curie": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["CHEBI:1234"],
      "expected": "http://purl.obolibrary.org/obo/CHEBI_1234"
    },
    "mondo_curie": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["MONDO:0005015"],
      "expected": "http://purl.obolibrary.org/obo/MONDO_0005015"
    },
    "unknown_prefix_returns_false": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["NOTAPREFIX:1"],
      "expected": false
    }
  });
}

function _test_curie_to_normalized_url() {
  return test.make_function_test({
    "non_umls_uses_input_directly": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["CHEBI:1234", ["CHEBI:1234", "MESH:D000001"]],
      "expected": "http://purl.obolibrary.org/obo/CHEBI_1234"
    },
    "umls_only_returns_umls_url": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["UMLS:C000001", ["UMLS:C000001"]],
      "expected": "http://identifiers.org/umls/C000001"
    },
    "umls_with_mesh_picks_mesh": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["UMLS:C000001", ["UMLS:C000001", "MESH:D000001"]],
      "expected": "http://id.nlm.nih.gov/mesh/D000001"
    }
  });
}

function _test_infores_to_provenance() {
  return test.make_function_test({
    "known_infores": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["infores:rnacentral"],
      "expected": {
        "name": "RNAcentral",
        "wiki": "https://rnacentral.org",
        "knowledge_level": "trusted"
      }
    },
    "unknown_infores_returns_null": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["infores:not-a-real-infores"],
      "expected": null
    }
  });
}

function _test_source_to_provenance() {
  return test.make_function_test({
    "known_source_with_records": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{
        "id": "infores:rnacentral",
        "records": ["http://example.com/record/1"]
      }],
      "expected": {
        "name": "RNAcentral",
        "wiki": "https://rnacentral.org",
        "knowledge_level": "trusted",
        "infores": "infores:rnacentral",
        "url": "http://example.com/record/1"
      }
    },
    "known_source_without_records": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{
        "id": "infores:rnacentral",
        "records": []
      }],
      "expected": {
        "name": "RNAcentral",
        "wiki": "https://rnacentral.org",
        "knowledge_level": "trusted",
        "infores": "infores:rnacentral"
      }
    },
    "unknown_source_returns_false": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": [{
        "id": "infores:not-a-real-infores",
        "records": []
      }],
      "expected": false
    }
  });
}

function _test_biolink_class_cmp_fn() {
  return test.make_function_test({
    "more_specific_class_sorts_before_less_specific": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:Disease", "biolink:Entity"],
      "expected": -4
    },
    "less_specific_class_sorts_after_more_specific": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:Entity", "biolink:Disease"],
      "expected": 4
    },
    "equal_rank_returns_zero": {
      config_loader: () => load_biolink(_test_biolink_config()),
      "args": ["biolink:Disease", "biolink:Disease"],
      "expected": 0
    }
  });
}

function _test_MissingInforesError() {
  return test.make_class_test({
    "exposes_infores_property": {
      "class_constructor": {
        "args": ["infores:test"]
      },
      "steps": [
        {
          "get": "infores",
          "expected": "infores:test"
        },
        {
          "get": "message",
          "expected": "Missing infores: infores:test"
        }
      ]
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
