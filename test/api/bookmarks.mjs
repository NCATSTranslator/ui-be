/* Standalone sanity testing for the happy paths of bookmarks API:
 *
 *   POST /api/v1/users/me/bookmarks            create a bookmark (save_type "bookmark")
 *   GET  /api/v1/users/me/bookmarks            list the current user's bookmarks
 *   POST /api/v1/users/me/bookmarks/:save_id   update one bookmark (body must include its id)
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/bookmarks.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { randomUUID } from 'node:crypto';
import { BYPASS_TEST_USER } from '../../mock/auth.mjs';

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const TEST_USER_ID = BYPASS_TEST_USER.id;
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

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

async function req(method, path, body) {
  const opts = { method };
  if (body !== undefined) {
    opts.headers = { 'Content-Type': 'application/json' };
    opts.body = JSON.stringify(body);
  }
  const res = await fetch(`${BASE_URL}${path}`, opts);
  const rawBody = await res.text();
  showResponse(method, path, res, rawBody);
  let json = null;
  if (rawBody) { try { json = JSON.parse(rawBody); } catch { /* non-JSON */ } }
  return { res, json };
}

console.log(`# user bookmarks lifecycle  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test bookmark ${new Date().toISOString()}`;
  const arsPkey = randomUUID();

  // Create (retains arsPkey against the ARS, then saves)
  const create = await req('POST', '/api/v1/users/me/bookmarks',
    { save_type: 'bookmark', ars_pkey: arsPkey, label, object_ref: 'node:NCBIGene:283' });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const bookmark = create.json;
  ok(bookmark && bookmark.id !== undefined && bookmark.id !== null, 'created bookmark has an id');
  ok(bookmark && bookmark.user_id === TEST_USER_ID, 'created bookmark belongs to the test user');
  ok(bookmark && bookmark.save_type === 'bookmark', 'created bookmark has save_type "bookmark"');
  ok(bookmark && bookmark.ars_pkey === arsPkey, 'created bookmark echoes the ars_pkey');

  const bookmarkId = bookmark ? bookmark.id : null;

  // List
  const list = await req('GET', '/api/v1/users/me/bookmarks');
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  ok(Array.isArray(list.json) && bookmarkId !== null && list.json.some((b) => b.id === bookmarkId),
    'created bookmark appears in the bookmarks list');
  ok(Array.isArray(list.json) && list.json.every((b) => b.save_type === 'bookmark'),
    'bookmarks list only contains bookmark saves');

  // Update (POST to the by-id route; the partial update keys off the id in the body)
  const newLabel = `${label} (updated)`;
  const update = await req('POST', `/api/v1/users/me/bookmarks/${bookmarkId}`, { id: bookmarkId, label: newLabel });
  ok(update.res.status === 200, `update responds 200 (got ${update.res.status})`);
  ok(update.json && update.json.label === newLabel, 'update echoes the new label');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
