/* Standalone API test: POST /api/v1/users/me/canvas
 *
 * Creates a canvas for the current user, then confirms it shows up in the list.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs): auth checking is
 * bypassed and every request resolves to the fixed test user, so no session cookie is needed.
 *
 *   npm run dev                        # shell 1: starts the server with local-overrides (auth_check=false)
 *   node test/api/canvas-create.mjs    # shell 2
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

console.log(`# POST /api/v1/users/me/canvas  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas ${new Date().toISOString()}`;
  const layout = 'horizontal';

  const createRes = await fetch(`${BASE_URL}/api/v1/users/me/canvas`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ label, layout, data: { source: 'canvas-create.mjs' } }),
  });
  const createRaw = await createRes.text();
  showResponse('POST', '/api/v1/users/me/canvas', createRes, createRaw);
  const canvas = createRaw ? JSON.parse(createRaw) : null;

  ok(createRes.status === 200, `create responds 200 (got ${createRes.status})`);
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');
  ok(canvas && canvas.label === label, 'created canvas echoes the label');
  ok(canvas && canvas.layout === layout, 'created canvas echoes the layout');
  ok(canvas && canvas.user_id === TEST_USER_ID, 'created canvas belongs to the test user');

  // Round-trip: the new canvas should now appear in the user's canvas list.
  const listRes = await fetch(`${BASE_URL}/api/v1/users/me/canvas`);
  const listRaw = await listRes.text();
  showResponse('GET', '/api/v1/users/me/canvas', listRes, listRaw);
  const canvases = listRaw ? JSON.parse(listRaw) : null;
  ok(listRes.status === 200, `list responds 200 (got ${listRes.status})`);
  ok(Array.isArray(canvases) && canvas && canvases.some((c) => c.id === canvas.id),
    'created canvas appears in the canvas list');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
