export {test_identifiers}

import * as test from "#test/lib/common.mjs";

async function test_identifiers(root_path) {
  await test.module_test({
    "module_path": "#lib/summarization/identifiers.mjs",
    "suite_path": "#test/data/summarization/identifiers.mjs"
  });
}
