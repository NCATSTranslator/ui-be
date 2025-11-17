export {test_summary_analysis}

import * as test from "#test/lib/common.mjs";

async function test_summary_analysis(root_path) {
  await test.module_test({
    "module_path": "#lib/summarization/summary-analysis.mjs",
    "suite_path": "#test/data/summarization/summary-analysis.mjs"
  });
}
