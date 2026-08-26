/* Standalone sanity test for the users endpoint: GET /api/v1/users/me
 *
 * Returns the user attached to the current session.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/users-me.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { BYPASS_TEST_USER } from '../../mock/auth.mjs';

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const TEST_USER_ID = BYPASS_TEST_USER.id;
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

let failures = 0;
function ok(cond, msg) {
  console[cond ? 'log' : 'error'](`  ${cond ? '✓' : '✗'} ${msg}`);
  if (!cond) failures += 1;
}

// When verbose, echo the server's raw response (status line + body) so it can be eyeballed.
function showResponse(method, path, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- server response: ${method} ${path} -> ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

console.log(`# GET /api/v1/users/me  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const res = await fetch(`${BASE_URL}/api/v1/users/me`);
  const rawBody = await res.text();
  showResponse('GET', '/api/v1/users/me', res, rawBody);
  const user = rawBody ? JSON.parse(rawBody) : null;

  ok(res.status === 200, `responds 200 (got ${res.status})`);
  ok(user && user.id === TEST_USER_ID, 'returns the test user id');
  ok(user && user.email === BYPASS_TEST_USER.email, 'returns the test user email');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
