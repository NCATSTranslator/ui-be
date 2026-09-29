export {test_api_key}

import * as test from '#test/lib/common.mjs';

async function test_api_key() {
  await test.module_test({
    module_path: "#model/ApiKey.mjs",
    suite_path: "#test/data/ApiKey.mjs"
  });
}
