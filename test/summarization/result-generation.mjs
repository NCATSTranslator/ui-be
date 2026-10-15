export {test_result_generation}

import * as test from "#test/lib/common.mjs";

async function test_result_generation() {
  await test.module_test({
    "module_path": "#lib/summarization/result-generation.mjs",
    "suite_path": "#test/data/summarization/result-generation.mjs"
  });
}
