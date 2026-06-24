export { test_evidence }

import * as test from "#test/lib/common.mjs";

async function test_evidence() {
  await test.module_test({
    "module_path": "#lib/evidence.mjs",
    "suite_path": "#test/data/evidence.mjs"
  });
}
