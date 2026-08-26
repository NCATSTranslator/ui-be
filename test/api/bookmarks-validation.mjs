/* Standalone sanity testing for error paths of the bookmarks API.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/bookmarks-validation.mjs
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

function showResponse(reqBody, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- POST /api/v1/users/me/bookmarks  request: ${JSON.stringify(reqBody)}`);
  console.log(`      response: ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

async function postStatus(body) {
  const res = await fetch(`${BASE_URL}/api/v1/users/me/bookmarks`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const rawBody = await res.text();
  showResponse(body, res, rawBody);
  return res.status;
}

console.log(`# POST /api/v1/users/me/bookmarks validation  (target: ${BASE_URL})`);
try {
  ok((await postStatus({ save_type: 'bookmark', label: 'no pkey' })) === 400,
    'bookmark without ars_pkey -> 400');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
