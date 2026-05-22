export { test_biothings_annotation }

import * as test from "#test/lib/common.mjs";

async function test_biothings_annotation() {
  await test.module_test({
    "module_path": "#lib/biothings-annotation.mjs",
    "suite_path": "#test/data/biothings-annotation.mjs"
  });
}
