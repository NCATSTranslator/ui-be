export { test_user_entity }

import * as test from "#test/lib/common.mjs";

async function test_user_entity() {
  await test.module_test({
    "module_path": "#model/UserEntity.mjs",
    "suite_path": "#test/data/UserEntity.mjs"
  });
}
