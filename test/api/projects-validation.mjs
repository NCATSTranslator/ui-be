/* Standalone sanity test for error paths of the projects API:
 *
 *   POST /api/v1/users/me/projects           requires "title" and "pks"
 *   PUT  /api/v1/users/me/projects           body must be a JSON array; every entry needs an id
 *   PUT  /api/v1/users/me/projects/trash     body must be a JSON array
 *   PUT  /api/v1/users/me/projects/restore   body must be a JSON array
 *
 * Confirms malformed project requests are rejected with 400.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/projects-validation.mjs
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

function showResponse(method, path, reqBody, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- ${method} ${path}  request: ${JSON.stringify(reqBody)}`);
  console.log(`      response: ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

async function status(method, path, body) {
  const res = await fetch(`${BASE_URL}${path}`, {
    method,
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const rawBody = await res.text();
  showResponse(method, path, body, res, rawBody);
  return res.status;
}

console.log(`# user projects validation  (target: ${BASE_URL})`);
try {
  // Create validation
  ok((await status('POST', '/api/v1/users/me/projects', { pks: ['pk-1'] })) === 400,
    'create missing title -> 400');
  ok((await status('POST', '/api/v1/users/me/projects', { title: 'no pks' })) === 400,
    'create missing pks -> 400');

  // Update validation
  ok((await status('PUT', '/api/v1/users/me/projects', { not: 'an array' })) === 400,
    'update with non-array body -> 400');
  ok((await status('PUT', '/api/v1/users/me/projects', [{ title: 'no id' }])) === 400,
    'update entry without an id -> 400');

  // Trash / restore validation
  ok((await status('PUT', '/api/v1/users/me/projects/trash', { not: 'an array' })) === 400,
    'trash with non-array body -> 400');
  ok((await status('PUT', '/api/v1/users/me/projects/restore', { not: 'an array' })) === 400,
    'restore with non-array body -> 400');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
