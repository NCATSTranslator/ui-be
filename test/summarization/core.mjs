export {test_summarization_core}

import * as test from "#test/lib/common.mjs";

async function test_summarization_core() {
  await test.module_test({
    "module_path": "#lib/summarization/core.mjs",
    "suite_path": "#test/data/summarization/core.mjs"
  });
}
