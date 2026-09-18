/* Standalone sanity test for error paths of the tags API.
 *
 * Confirms deleting a tag that does not exist (or does not belong to the user)
 * is rejected with 404.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/tags-validation.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print each request and raw server response.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

// A save id that should not exist for the test user.
const MISSING_ID = 2147483601;

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

async function status(method, path) {
  const res = await fetch(`${BASE_URL}${path}`, { method });
  const rawBody = await res.text();
  showResponse(method, path, res, rawBody);
  return res.status;
}

console.log(`# user tags delete not-found handling  (target: ${BASE_URL})`);
try {
  ok((await status('DELETE', `/api/v1/users/me/tags/${MISSING_ID}`)) === 404,
    'DELETE missing tag -> 404');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
