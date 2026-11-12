export { test_query_view }

import * as test from "#test/lib/common.mjs";

async function test_query_view() {
  await test.module_test({
    "module_path": "#model/QueryView.mjs",
    "suite_path": "#test/data/QueryView.mjs"
  });
}
