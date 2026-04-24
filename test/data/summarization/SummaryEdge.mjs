export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    SummaryEdge: _test_SummaryEdge()
  }
};

function _test_SummaryEdge() {
  const default_state = {
    aras: [],
    support: [],
    is_root: false,
    knowledge_level: null,
    description: null,
    type: null,
    subject: null,
    object: null,
    predicate: null,
    predicate_url: null,
    provenance: [],
    publications: {},
    metadata: null,
    trials: []
  };
  return test.make_class_test({
    default_class_constructor: {
      class_constructor: {
        args: [],
        expected: default_state
      }
    },
    null_args_default_to_empty: {
      class_constructor: {
        args: [null, null, null],
        expected: default_state
      }
    },
    with_agents: {
      class_constructor: {
        args: [["ara1", "ara2"]],
        expected: {
          ...default_state,
          aras: ["ara1", "ara2"]
        }
      }
    },
    with_support_and_is_root: {
      class_constructor: {
        args: [[], ["edge_id_1", "edge_id_2"], true],
        expected: {
          ...default_state,
          support: ["edge_id_1", "edge_id_2"],
          is_root: true
        }
      }
    },
    has_support_empty_is_false: {
      class_constructor: { args: [] },
      steps: [
        { method: "has_support", args: [], expected: false }
      ]
    },
    has_support_with_support_is_true: {
      class_constructor: { args: [[], ["edge_id_1"]] },
      steps: [
        { method: "has_support", args: [], expected: true }
      ]
    },
    is_inverted_null_metadata_is_false: {
      class_constructor: { args: [] },
      steps: [
        { method: "is_inverted", args: [], expected: false }
      ]
    }
  });
}
