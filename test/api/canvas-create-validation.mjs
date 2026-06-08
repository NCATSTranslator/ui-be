/* Standalone API test: POST /api/v1/users/me/canvas (validation)
 *
 * Confirms malformed canvas requests are rejected with 400. A canvas request requires a string
 * `label` and a `layout` of one of: horizontal, vertical, concentric, custom (see models/Canvas.mjs).
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs): the request is
 * authorized as the fixed test user and therefore reaches the canvas request validator.
 *
 *   npm run dev                                   # shell 1: starts the server (auth_check=false)
 *   node test/api/canvas-create-validation.mjs    # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print each request payload and raw server response.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

let failures = 0;
function ok(cond, msg) {
  console[cond ? 'log' : 'error'](`  ${cond ? '✓' : '✗'} ${msg}`);
  if (!cond) failures += 1;
}

// When verbose, echo what was sent and the server's raw response (status line + body).
function showResponse(reqBody, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- POST /api/v1/users/me/canvas  request: ${JSON.stringify(reqBody)}`);
  console.log(`      response: ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

async function postStatus(body) {
  const res = await fetch(`${BASE_URL}/api/v1/users/me/canvas`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const rawBody = await res.text();
  showResponse(body, res, rawBody);
  return res.status;
}

console.log(`# POST /api/v1/users/me/canvas validation  (target: ${BASE_URL})`);
try {
  ok((await postStatus({ layout: 'horizontal' })) === 400, 'missing label -> 400');
  ok((await postStatus({ label: 'no layout' })) === 400, 'missing layout -> 400');
  ok((await postStatus({ label: 'bad layout', layout: 'diagonal' })) === 400, 'invalid layout -> 400');
  ok((await postStatus({})) === 400, 'empty body -> 400');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
