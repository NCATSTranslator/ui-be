export { test_canvas }

import * as test from "#test/lib/common.mjs";

async function test_canvas() {
  await test.module_test({
    "module_path": "#model/Canvas.mjs",
    "suite_path": "#test/data/canvas.mjs"
  });
}
