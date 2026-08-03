export { test_predicates }

import * as test from "#test/lib/common.mjs";

async function test_predicates() {
  await test.module_test({
    "module_path": "#lib/summarization/predicates.mjs",
    "suite_path": "#test/data/summarization/predicates.mjs"
  });
}
