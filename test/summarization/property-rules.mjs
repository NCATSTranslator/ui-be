export {test_summarization_property_rules}

import * as test from "#test/lib/common.mjs";

function test_summarization_property_rules() {
  test.module_test({
    module_path: "#lib/summarization/property-rules.mjs",
    suite_path: "#test/data/summarization/property-rules.mjs"
  });
}
