export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    SummaryEdge: _test_SummaryEdge()
  }
};

function _test_SummaryEdge() {
  const EDGE_ID = "edge_test";
  const default_state = {
    id: EDGE_ID,
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
  const default_other = {
    id: EDGE_ID,
    metadata: null,
    aras: [],
    support: [],
    is_root: false,
    description: null,
    knowledge_level: null,
    type: null,
    subject: null,
    predicate: null,
    object: null,
    predicate_url: null,
    provenance: [],
    trials: [],
    publications: {}
  };
  return test.make_class_test({
    default_class_constructor: {
      class_constructor: {
        args: [EDGE_ID],
        expected: default_state
      }
    },
    null_args_default_to_empty: {
      class_constructor: {
        args: [EDGE_ID, null, null, null],
        expected: default_state
      }
    },
    with_agents: {
      class_constructor: {
        args: [EDGE_ID, ["ara1", "ara2"]],
        expected: {
          ...default_state,
          aras: ["ara1", "ara2"]
        }
      }
    },
    with_support_and_is_root: {
      class_constructor: {
        args: [EDGE_ID, [], ["edge_id_1", "edge_id_2"], true],
        expected: {
          ...default_state,
          support: ["edge_id_1", "edge_id_2"],
          is_root: true
        }
      }
    },
    has_support_empty_is_false: {
      class_constructor: { args: [EDGE_ID] },
      steps: [
        { method: "has_support", args: [], expected: false }
      ]
    },
    has_support_with_support_is_true: {
      class_constructor: { args: [EDGE_ID, [], ["edge_id_1"]] },
      steps: [
        { method: "has_support", args: [], expected: true }
      ]
    },
    is_inverted_null_metadata_is_false: {
      class_constructor: { args: [EDGE_ID] },
      steps: [
        { method: "is_inverted", args: [], expected: false }
      ]
    },
    is_inverted_with_inverted_id_is_true: {
      injected: {
        id: EDGE_ID,
        metadata: { inverted_id: "EDGE_INV_1", edge_bindings: [] }
      },
      steps: [
        { method: "is_inverted", args: [], expected: true }
      ]
    },
    is_inverted_with_null_inverted_id_is_false: {
      injected: {
        id: EDGE_ID,
        metadata: { inverted_id: null, edge_bindings: [] }
      },
      steps: [
        { method: "is_inverted", args: [], expected: false }
      ]
    },
    merge_accumulates_aras_support_and_is_root: {
      class_constructor: {
        args: [EDGE_ID, ["ara1"], ["edge1"], true]
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            aras: ["ara2"],
            support: ["edge2", "edge3"],
            is_root: false
          }],
          expected: {
            ...default_state,
            aras: ["ara1", "ara2"],
            support: ["edge1", "edge2", "edge3"],
            is_root: true
          }
        }
      ]
    },
    merge_preserves_existing_scalar_fields: {
      injected: {
        id: EDGE_ID,
        description: "existing description",
        knowledge_level: "primary",
        type: "direct",
        subject: "subj_a",
        predicate: "pred_a",
        object: "obj_a",
        predicate_url: "http://example.com/a"
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            description: "ignored description",
            knowledge_level: "ignored",
            type: "direct",
            subject: "ignored_subj",
            predicate: "ignored_pred",
            object: "ignored_obj",
            predicate_url: "http://ignored"
          }],
          expected: {
            ...default_state,
            description: "existing description",
            knowledge_level: "primary",
            type: "direct",
            subject: "subj_a",
            predicate: "pred_a",
            object: "obj_a",
            predicate_url: "http://example.com/a"
          }
        }
      ]
    },
    merge_fills_missing_scalar_fields_from_other: {
      class_constructor: { args: [EDGE_ID] },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            description: "other description",
            knowledge_level: "primary",
            type: "direct",
            subject: "subj_b",
            predicate: "pred_b",
            object: "obj_b",
            predicate_url: "http://example.com/b"
          }],
          expected: {
            ...default_state,
            description: "other description",
            knowledge_level: "primary",
            type: "direct",
            subject: "subj_b",
            predicate: "pred_b",
            object: "obj_b",
            predicate_url: "http://example.com/b"
          }
        }
      ]
    },
    merge_indirect_type_overrides_direct: {
      injected: {
        id: EDGE_ID,
        type: "direct"
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            type: "indirect"
          }],
          expected: {
            ...default_state,
            type: "indirect"
          }
        }
      ]
    },
    merge_appends_edge_bindings_when_both_have_metadata: {
      injected: {
        id: EDGE_ID,
        metadata: { inverted_id: "EDGE_INV_1", edge_bindings: ["eb1"] }
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            metadata: { inverted_id: "EDGE_INV_2", edge_bindings: ["eb2", "eb3"] }
          }],
          expected: {
            ...default_state,
            metadata: { inverted_id: "EDGE_INV_1", edge_bindings: ["eb1", "eb2", "eb3"] }
          }
        }
      ]
    },
    merge_combines_publications_by_knowledge_level: {
      injected: {
        id: EDGE_ID,
        publications: { primary: ["pub1"] }
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            publications: { primary: ["pub2"], secondary: ["pub3"] }
          }],
          expected: {
            ...default_state,
            publications: { primary: ["pub1", "pub2"], secondary: ["pub3"] }
          }
        }
      ]
    },
    merge_concatenates_provenance_and_trials: {
      injected: {
        id: EDGE_ID,
        provenance: ["prov1"],
        trials: ["trial1"]
      },
      steps: [
        {
          method: "merge",
          args: [{
            ...default_other,
            provenance: ["prov2", "prov3"],
            trials: ["trial2"]
          }],
          expected: {
            ...default_state,
            provenance: ["prov1", "prov2", "prov3"],
            trials: ["trial1", "trial2"]
          }
        }
      ]
    }
  });
}
