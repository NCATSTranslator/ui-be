export {test_edge_finalization}

import * as test from "#test/lib/common.mjs";

async function test_edge_finalization() {
  await test.module_test({
    "module_path": "#lib/summarization/edge-finalization.mjs",
    "suite_path": "#test/data/summarization/edge-finalization.mjs"
  });
}
