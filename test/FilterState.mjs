export { test_filter_state }

import * as test from "#test/lib/common.mjs";

async function test_filter_state() {
  await test.module_test({
    "module_path": "#model/FilterState.mjs",
    "suite_path": "#test/data/FilterState.mjs"
  });
}
