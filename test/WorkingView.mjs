export { test_working_view }

import * as test from "#test/lib/common.mjs";

async function test_working_view() {
  await test.module_test({
    "module_path": "#model/WorkingView.mjs",
    "suite_path": "#test/data/WorkingView.mjs"
  });
}
