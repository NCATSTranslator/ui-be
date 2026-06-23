export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    id_to_type_and_url: _test_id_to_type_and_url()
  },
  skip: {
    is_valid_id: true,
    is_go_ref: true,
    sanitize: true,
    is_clinical_trial: true,
    is_publication: true,
    is_ontological: true
  }
};

function _test_id_to_type_and_url() {
  return test.make_function_test({
    "dailymed_setid": {
      "args": ["dailymed:e1ada3ec-c2c0-4f43-9b4b-1234567890ab"],
      "expected": ["dailymed", "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=e1ada3ec-c2c0-4f43-9b4b-1234567890ab"]
    },
    "dailymed_missing_setid": {
      "args": ["dailymed:"],
      "expected": [null, null]
    },
    "dailymed_prefix_without_colon": {
      "args": ["dailymed"],
      "expected": [null, null]
    }
  });
}
