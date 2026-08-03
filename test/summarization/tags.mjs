export { test_tags }

import * as test from "#test/lib/common.mjs";

async function test_tags() {
  await test.module_test({
    "module_path": "#lib/summarization/tags.mjs",
    "suite_path": "#test/data/summarization/tags.mjs"
  });
}
