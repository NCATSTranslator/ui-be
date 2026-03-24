export {test_taglib}

import * as test from '#test/lib/common.mjs';

async function test_taglib() {
  await test.module_test({
    module_path: "#lib/taglib.mjs",
    suite_path: "#test/data/taglib.mjs"
  });
}
