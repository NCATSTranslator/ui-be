/* Standalone API test: GET /api/v1/users/me/canvas
 *
 * Lists the canvases belonging to the current user.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs): auth checking is
 * bypassed and every request resolves to the fixed test user, so no session cookie is needed.
 *
 *   npm run dev                      # shell 1: starts the server with local-overrides (auth_check=false)
 *   node test/api/canvas-list.mjs    # shell 2
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

console.log(`# GET /api/v1/users/me/canvas  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const res = await fetch(`${BASE_URL}/api/v1/users/me/canvas`);
  const rawBody = await res.text();
  showResponse('GET', '/api/v1/users/me/canvas', res, rawBody);
  const canvases = rawBody ? JSON.parse(rawBody) : null;

  ok(res.status === 200, `responds 200 (got ${res.status})`);
  ok(Array.isArray(canvases), 'body is an array of canvases');
  if (Array.isArray(canvases)) {
    console.log(`    (${canvases.length} canvas(es) returned)`);
    ok(canvases.every((c) => c.user_id === TEST_USER_ID), 'every canvas belongs to the test user');
    ok(canvases.every((c) => c.id !== undefined && c.id !== null), 'every canvas has an id');
  }
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
