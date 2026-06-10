/* Runner for the API tests in test/api/.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   npm run test-api
 *
 * Any flags/env are forwarded to each test, so `npm run test-api -- --verbose` and API_BASE_URL=...
 * work as they do for an individual test.
 */

import { spawn } from 'node:child_process';
import { readdirSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

const apiDir = join(dirname(fileURLToPath(import.meta.url)), 'api');
const forwardedArgs = process.argv.slice(2);

const tests = readdirSync(apiDir)
  .filter((name) => name.endsWith('.mjs'))
  .sort();

function runTest(file) {
  return new Promise((resolve) => {
    const child = spawn(process.execPath, [join(apiDir, file), ...forwardedArgs], {
      stdio: 'inherit',
    });
    child.on('exit', (code) => resolve(code ?? 1));
    child.on('error', (err) => {
      console.error(`  ✗ failed to launch ${file}: ${err.message}`);
      resolve(1);
    });
  });
}

console.log(`# api test suite  (${tests.length} file(s) in ${apiDir})\n`);

const results = [];
for (const file of tests) {
  console.log(`\n=== ${file} ===`);
  const code = await runTest(file);
  results.push({ file, passed: code === 0 });
}

const failed = results.filter((r) => !r.passed);

console.log('\n========================================');
console.log(`api test suite: ${results.length - failed.length}/${results.length} file(s) passed`);
for (const { file, passed } of results) {
  console.log(`  ${passed ? '✓' : '✗'} ${file}`);
}
console.log('========================================\n');

process.exit(failed.length === 0 ? 0 : 1);
