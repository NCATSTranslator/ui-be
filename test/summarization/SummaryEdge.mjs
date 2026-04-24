export {test_summary_edge}

import * as test from "#test/lib/common.mjs";

async function test_summary_edge() {
  await test.module_test({
    "module_path": "#lib/summarization/SummaryEdge.mjs",
    "suite_path": "#test/data/summarization/SummaryEdge.mjs"
  });
}
