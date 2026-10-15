/* Runner for the API tests in test/api/.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   npm run test-api
 *
 * Any flags/env are forwarded to each test, so `npm run test-api -- --verbose` and API_BASE_URL=...
 * work as they do for an individual test.
 */

import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';
import { run_test_files } from './lib/file-runner.mjs';

const apiDir = join(dirname(fileURLToPath(import.meta.url)), 'api');
const passed = await run_test_files('api', apiDir, process.argv.slice(2));
process.exit(passed ? 0 : 1);
