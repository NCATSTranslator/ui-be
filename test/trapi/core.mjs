export {test_trapi_core};

import * as test from '#test/lib/common.mjs';

async function test_trapi_core() {
  await test.module_test({
    module_path: '#lib/trapi/core.mjs',
    suite_path: '#test/data/trapi/core.mjs'
  });
}
