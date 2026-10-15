export {test_node_finalization}

import * as test from "#test/lib/common.mjs";

async function test_node_finalization() {
  await test.module_test({
    "module_path": "#lib/summarization/node-finalization.mjs",
    "suite_path": "#test/data/summarization/node-finalization.mjs"
  });
}
