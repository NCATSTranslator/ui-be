export { suite }

import * as test from "#test/lib/common.mjs";
import { UserEntityRequestError } from "#model/UserEntity.mjs";

const suite = {
  tests: {
    make_user_node_from_req: _test_make_user_node_from_req(),
    make_user_edge_from_req: _test_make_user_edge_from_req(),
    make_user_node_update_from_req: _test_make_user_node_update_from_req(),
    make_user_edge_update_from_req: _test_make_user_edge_update_from_req()
  },
  skip: {
    UserNode: true,
    UserEdge: true,
    UserEntityRequestError: true,
    USER_NODE_DEFAULT_TYPE: true
  }
};

function _test_make_user_node_from_req() {
  return test.make_function_test({
    "type_defaults_to_other": {
      "args": ["user-1", { label: "My Node", data: { note: "hi" } }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "My Node",
        type: "Other",
        data: { note: "hi" },
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "explicit_type": {
      "args": ["user-1", { label: "Gene X", type: "biolink:Gene", data: {} }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "Gene X",
        type: "biolink:Gene",
        data: {},
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "unknown_fields_throw": {
      "args": ["user-1", { label: "N", data: {}, id: 9, user_id: "someone-else" }],
      "expected": UserEntityRequestError
    },
    "missing_throws": {
      "args": ["user-1", undefined],
      "expected": UserEntityRequestError
    },
    "missing_label_throws": {
      "args": ["user-1", { data: {} }],
      "expected": UserEntityRequestError
    },
    "non_string_label_throws": {
      "args": ["user-1", { label: 1, data: {} }],
      "expected": UserEntityRequestError
    },
    "missing_data_throws": {
      "args": ["user-1", { label: "N" }],
      "expected": UserEntityRequestError
    },
    "array_data_throws": {
      "args": ["user-1", { label: "N", data: [1, 2] }],
      "expected": UserEntityRequestError
    },
    "null_data_throws": {
      "args": ["user-1", { label: "N", data: null }],
      "expected": UserEntityRequestError
    },
    "empty_type_throws": {
      "args": ["user-1", { label: "N", type: "", data: {} }],
      "expected": UserEntityRequestError
    },
    "non_string_type_throws": {
      "args": ["user-1", { label: "N", type: 3, data: {} }],
      "expected": UserEntityRequestError
    }
  });
}

function _test_make_user_edge_from_req() {
  return test.make_function_test({
    "ref_endpoints": {
      "args": ["user-1", {
        label: "relates to", data: { weight: 2 }, subject_ref: "MONDO:1", object_ref: "CHEBI:2"
      }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "relates to",
        data: { weight: 2 },
        subject_ref: "MONDO:1",
        subject_user_node_id: null,
        object_ref: "CHEBI:2",
        object_user_node_id: null,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "user_node_and_ref_endpoints": {
      "args": ["user-1", { label: "E", data: {}, subject_user_node_id: 4, object_ref: "MONDO:1" }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "E",
        data: {},
        subject_ref: null,
        subject_user_node_id: 4,
        object_ref: "MONDO:1",
        object_user_node_id: null,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "type_throws": {
      "args": ["user-1", { label: "E", type: "biolink:treats", data: {}, subject_ref: "A:1", object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "missing_throws": {
      "args": ["user-1", null],
      "expected": UserEntityRequestError
    },
    "missing_label_throws": {
      "args": ["user-1", { data: {}, subject_ref: "A:1", object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "string_data_throws": {
      "args": ["user-1", { label: "E", data: "text", subject_ref: "A:1", object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "missing_subject_throws": {
      "args": ["user-1", { label: "E", data: {}, object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "missing_object_throws": {
      "args": ["user-1", { label: "E", data: {}, subject_ref: "A:1" }],
      "expected": UserEntityRequestError
    },
    "both_subject_forms_throws": {
      "args": ["user-1", { label: "E", data: {}, subject_ref: "A:1", subject_user_node_id: 4, object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "empty_ref_throws": {
      "args": ["user-1", { label: "E", data: {}, subject_ref: "", object_ref: "B:2" }],
      "expected": UserEntityRequestError
    },
    "non_integer_user_node_id_throws": {
      "args": ["user-1", { label: "E", data: {}, subject_ref: "A:1", object_user_node_id: "4" }],
      "expected": UserEntityRequestError
    }
  });
}

function _test_make_user_node_update_from_req() {
  return test.make_function_test({
    "label_only": {
      "args": [{ label: "Renamed" }],
      "expected": { label: "Renamed" }
    },
    "type_only": {
      "args": [{ type: "biolink:Disease" }],
      "expected": { type: "biolink:Disease" }
    },
    "data_only": {
      "args": [{ data: { a: 1 } }],
      "expected": { data: { a: 1 } }
    },
    "all_fields": {
      "args": [{ label: "L", type: "T", data: {} }],
      "expected": { label: "L", type: "T", data: {} }
    },
    "unknown_fields_throw": {
      "args": [{ label: "L", user_id: "someone-else", time_deleted: null }],
      "expected": UserEntityRequestError
    },
    "empty_update_throws": {
      "args": [{}],
      "expected": UserEntityRequestError
    },
    "missing_throws": {
      "args": [undefined],
      "expected": UserEntityRequestError
    },
    "array_data_throws": {
      "args": [{ data: [] }],
      "expected": UserEntityRequestError
    },
    "empty_type_throws": {
      "args": [{ type: "" }],
      "expected": UserEntityRequestError
    }
  });
}

function _test_make_user_edge_update_from_req() {
  return test.make_function_test({
    "label_only": {
      "args": [{ label: "Renamed" }],
      "expected": { label: "Renamed" }
    },
    "data_only": {
      "args": [{ data: { a: 1 } }],
      "expected": { data: { a: 1 } }
    },
    "type_only_throws": {
      "args": [{ type: "biolink:treats" }],
      "expected": UserEntityRequestError
    },
    "endpoint_change_throws": {
      "args": [{ label: "L", subject_ref: "A:1" }],
      "expected": UserEntityRequestError
    },
    "null_endpoint_change_throws": {
      "args": [{ object_user_node_id: null }],
      "expected": UserEntityRequestError
    },
    "empty_update_throws": {
      "args": [{}],
      "expected": UserEntityRequestError
    },
    "non_string_label_throws": {
      "args": [{ label: false }],
      "expected": UserEntityRequestError
    }
  });
}
