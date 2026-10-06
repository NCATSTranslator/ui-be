export { run_test_files }

import { spawn } from "node:child_process";
import { readdirSync } from "node:fs";
import { join } from "node:path";

function _run_test_file(path, forwarded_args) {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [path, ...forwarded_args], { stdio: "inherit" });
    child.on("exit", (code) => resolve(code ?? 1));
    child.on("error", (err) => {
      console.error(`  ✗ failed to launch ${path}: ${err.message}`);
      resolve(1);
    });
  });
}

async function run_test_files(suite_name, dir, forwarded_args) {
  const files = readdirSync(dir).filter((name) => name.endsWith(".mjs")).sort();
  console.log(`# ${suite_name} test suite  (${files.length} file(s) in ${dir})\n`);
  const results = [];
  for (const file of files) {
    console.log(`\n=== ${file} ===`);
    const code = await _run_test_file(join(dir, file), forwarded_args);
    results.push({ file: file, passed: code === 0 });
  }
  const failed = results.filter((result) => !result.passed);
  console.log("\n========================================");
  console.log(`${suite_name} test suite: ${results.length - failed.length}/${results.length} file(s) passed`);
  for (const result of results) {
    console.log(`  ${result.passed ? "✓" : "✗"} ${result.file}`);
  }
  console.log("========================================\n");
  return failed.length === 0;
}
