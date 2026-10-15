export {test_summary}

import * as test from "#test/lib/common.mjs";

async function test_summary() {
  await test.module_test({
    "module_path": "#lib/summarization/Summary.mjs",
    "suite_path": "#test/data/summarization/Summary.mjs"
  });
}
