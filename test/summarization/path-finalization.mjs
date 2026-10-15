export {test_path_finalization}

import * as test from "#test/lib/common.mjs";

async function test_path_finalization() {
  await test.module_test({
    "module_path": "#lib/summarization/path-finalization.mjs",
    "suite_path": "#test/data/summarization/path-finalization.mjs"
  });
}
