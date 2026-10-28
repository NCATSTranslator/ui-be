export { suite }

import * as test from "#test/lib/common.mjs";
import { QueryViewRequestError } from "#model/QueryView.mjs";

const suite = {
  tests: {
    make_query_view_from_req: _test_make_query_view_from_req()
  },
  skip: {
    QueryView: true,
    QueryViewRequestError: true
  }
};

function _test_make_query_view_from_req() {
  return test.make_function_test({
    "existing_filter_state": {
      "args": ["user-1", { filter_state_id: 7 }],
      "expected": { filter_state_id: 7, filter_state: null }
    },
    "new_filter_state": {
      "args": ["user-1", { version: 1, state: [{ id: "n/fda/4", value: "", negated: false }] }],
      "expected": {
        filter_state_id: null,
        filter_state: {
          id: null,
          user_id: "user-1",
          label: null,
          hidden: true,
          version: 1,
          state: [{ id: "n/fda/4", value: "", negated: false }],
          time_created: "*",
          time_updated: "*",
          time_deleted: null
        }
      }
    },
    "new_filter_state_with_null_state": {
      "args": ["user-1", { version: 0, state: null }],
      "expected": {
        filter_state_id: null,
        filter_state: {
          id: null,
          user_id: "user-1",
          label: null,
          hidden: true,
          version: 0,
          state: null,
          time_created: "*",
          time_updated: "*",
          time_deleted: null
        }
      }
    },
    "missing_throws": {
      "args": ["user-1", undefined],
      "expected": QueryViewRequestError
    },
    "array_body_throws": {
      "args": ["user-1", [{ filter_state_id: 7 }]],
      "expected": QueryViewRequestError
    },
    "empty_throws": {
      "args": ["user-1", {}],
      "expected": QueryViewRequestError
    },
    "string_filter_state_id_throws": {
      "args": ["user-1", { filter_state_id: "7" }],
      "expected": QueryViewRequestError
    },
    "zero_filter_state_id_throws": {
      "args": ["user-1", { filter_state_id: 0 }],
      "expected": QueryViewRequestError
    },
    "float_filter_state_id_throws": {
      "args": ["user-1", { filter_state_id: 1.5 }],
      "expected": QueryViewRequestError
    },
    "unsafe_filter_state_id_throws": {
      "args": ["user-1", { filter_state_id: 9007199254740993 }],
      "expected": QueryViewRequestError
    },
    "filter_state_id_with_state_throws": {
      "args": ["user-1", { filter_state_id: 7, version: 1, state: {} }],
      "expected": QueryViewRequestError
    },
    "missing_version_throws": {
      "args": ["user-1", { state: {} }],
      "expected": QueryViewRequestError
    },
    "missing_state_throws": {
      "args": ["user-1", { version: 1 }],
      "expected": QueryViewRequestError
    },
    "negative_version_throws": {
      "args": ["user-1", { version: -1, state: {} }],
      "expected": QueryViewRequestError
    },
    "label_throws": {
      "args": ["user-1", { label: "L", version: 1, state: {} }],
      "expected": QueryViewRequestError
    },
    "hidden_throws": {
      "args": ["user-1", { hidden: false, version: 1, state: {} }],
      "expected": QueryViewRequestError
    },
    "unknown_field_throws": {
      "args": ["user-1", { version: 1, state: {}, user_id: "someone-else" }],
      "expected": QueryViewRequestError
    }
  });
}
