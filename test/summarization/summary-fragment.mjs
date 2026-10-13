export {test_summary_fragment}

import * as test from "#test/lib/common.mjs";

async function test_summary_fragment() {
  await test.module_test({
    "module_path": "#lib/summarization/summary-fragment.mjs",
    "suite_path": "#test/data/summarization/summary-fragment.mjs"
  });
}
