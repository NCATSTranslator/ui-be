export {test_summary_node}

import * as test from "#test/lib/common.mjs";

async function test_summary_node() {
  await test.module_test({
    "module_path": "#lib/summarization/SummaryNode.mjs",
    "suite_path": "#test/data/summarization/SummaryNode.mjs"
  });
}
