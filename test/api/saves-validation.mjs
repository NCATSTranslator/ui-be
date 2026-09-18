/* Standalone sanity tests for error paths of the saves API:
 *
 *   GET    /api/v1/users/me/saves/:save_id   -> 404 when the save does not exist
 *   PUT    /api/v1/users/me/saves/:save_id   -> 404 when the save does not exist
 *   DELETE /api/v1/users/me/saves/:save_id   -> 404 when the save does not exist
 *
 * Confirms operations on a save id that does not belong to the user (or does not exist) are
 * rejected with 404 rather than silently succeeding.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/saves-validation.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print each request and raw server response.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

// A save id that should not exist for the test user.
const MISSING_ID = 2147483600;

let failures = 0;
function ok(cond, msg) {
  console[cond ? 'log' : 'error'](`  ${cond ? '✓' : '✗'} ${msg}`);
  if (!cond) failures += 1;
}

function showResponse(method, path, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- server response: ${method} ${path} -> ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

async function status(method, path, body) {
  const opts = { method };
  if (body !== undefined) {
    opts.headers = { 'Content-Type': 'application/json' };
    opts.body = JSON.stringify(body);
  }
  const res = await fetch(`${BASE_URL}${path}`, opts);
  const rawBody = await res.text();
  showResponse(method, path, res, rawBody);
  return res.status;
}

console.log(`# user saves by-id not-found handling  (target: ${BASE_URL})`);
try {
  ok((await status('GET', `/api/v1/users/me/saves/${MISSING_ID}`)) === 404,
    'GET missing save -> 404');
  ok((await status('PUT', `/api/v1/users/me/saves/${MISSING_ID}`, { id: MISSING_ID, label: 'x' })) === 404,
    'PUT missing save -> 404');
  ok((await status('DELETE', `/api/v1/users/me/saves/${MISSING_ID}`)) === 404,
    'DELETE missing save -> 404');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
