export { test_biolink_model }

import * as test from "#test/lib/common.mjs";

async function test_biolink_model() {
  await test.module_test({
    "module_path": "#lib/biolink-model.mjs",
    "suite_path": "#test/data/biolink-model.mjs"
  });
}
