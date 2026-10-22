export { suite }

import * as test from "#test/lib/common.mjs";
import { FilterStateRequestError } from "#model/FilterState.mjs";

const suite = {
  tests: {
    make_filter_state_from_req: _test_make_filter_state_from_req(),
    make_filter_state_update_from_req: _test_make_filter_state_update_from_req()
  },
  skip: {
    FilterState: true,
    FilterStateRequestError: true,
    FILTER_STATE_MAX_BYTES: true
  }
};

function _test_make_filter_state_from_req() {
  return test.make_function_test({
    "visible_preset": {
      "args": ["user-1", { label: "FDA approved", version: 1, state: [{ id: "n/fda/4", value: "", negated: false }] }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "FDA approved",
        hidden: false,
        version: 1,
        state: [{ id: "n/fda/4", value: "", negated: false }],
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "hidden_without_label": {
      "args": ["user-1", { hidden: true, version: 2, state: { filters: [] } }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: null,
        hidden: true,
        version: 2,
        state: { filters: [] },
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "hidden_with_label": {
      "args": ["user-1", { label: "Later", hidden: true, version: 0, state: "raw" }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "Later",
        hidden: true,
        version: 0,
        state: "raw",
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "null_state": {
      "args": ["user-1", { label: "Empty", version: 1, state: null }],
      "expected": {
        id: null,
        user_id: "user-1",
        label: "Empty",
        hidden: false,
        version: 1,
        state: null,
        time_created: "*",
        time_updated: "*",
        time_deleted: null
      }
    },
    "unknown_fields_throw": {
      "args": ["user-1", { label: "L", version: 1, state: {}, id: 9, user_id: "someone-else" }],
      "expected": FilterStateRequestError
    },
    "missing_throws": {
      "args": ["user-1", undefined],
      "expected": FilterStateRequestError
    },
    "array_body_throws": {
      "args": ["user-1", [{ label: "L", version: 1, state: {} }]],
      "expected": FilterStateRequestError
    },
    "visible_without_label_throws": {
      "args": ["user-1", { version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "explicit_visible_without_label_throws": {
      "args": ["user-1", { hidden: false, version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "empty_label_throws": {
      "args": ["user-1", { label: "", version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "non_string_label_throws": {
      "args": ["user-1", { label: 7, version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "hidden_non_string_label_throws": {
      "args": ["user-1", { label: 7, hidden: true, version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "non_boolean_hidden_throws": {
      "args": ["user-1", { label: "L", hidden: "yes", version: 1, state: {} }],
      "expected": FilterStateRequestError
    },
    "missing_version_throws": {
      "args": ["user-1", { label: "L", state: {} }],
      "expected": FilterStateRequestError
    },
    "string_version_throws": {
      "args": ["user-1", { label: "L", version: "1", state: {} }],
      "expected": FilterStateRequestError
    },
    "float_version_throws": {
      "args": ["user-1", { label: "L", version: 1.5, state: {} }],
      "expected": FilterStateRequestError
    },
    "negative_version_throws": {
      "args": ["user-1", { label: "L", version: -1, state: {} }],
      "expected": FilterStateRequestError
    },
    "missing_state_throws": {
      "args": ["user-1", { label: "L", version: 1 }],
      "expected": FilterStateRequestError
    }
  });
}

function _test_make_filter_state_update_from_req() {
  return test.make_function_test({
    "label_only": {
      "args": [{ label: "Renamed" }],
      "expected": { label: "Renamed" }
    },
    "hidden_only": {
      "args": [{ hidden: true }],
      "expected": { hidden: true }
    },
    "label_and_hidden": {
      "args": [{ label: "Shown", hidden: false }],
      "expected": { label: "Shown", hidden: false }
    },
    "missing_throws": {
      "args": [null],
      "expected": FilterStateRequestError
    },
    "empty_throws": {
      "args": [{}],
      "expected": FilterStateRequestError
    },
    "version_throws": {
      "args": [{ version: 2 }],
      "expected": FilterStateRequestError
    },
    "state_throws": {
      "args": [{ state: {} }],
      "expected": FilterStateRequestError
    },
    "empty_label_throws": {
      "args": [{ label: "" }],
      "expected": FilterStateRequestError
    },
    "null_label_throws": {
      "args": [{ label: null }],
      "expected": FilterStateRequestError
    },
    "non_boolean_hidden_throws": {
      "args": [{ hidden: 1 }],
      "expected": FilterStateRequestError
    }
  });
}
