export {test_trapi_property_rules};

import * as test from '#test/lib/common.mjs';

async function test_trapi_property_rules() {
  await test.module_test({
    module_path: '#lib/trapi/property-rules.mjs',
    suite_path: '#test/data/trapi/property-rules.mjs'
  });
}
