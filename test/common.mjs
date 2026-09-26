export {test_common}

import * as test from '#test/lib/common.mjs';

async function test_common() {
  await test.module_test({
    module_path: "#lib/common.mjs",
    suite_path: "#test/data/common.mjs"
  });
}
