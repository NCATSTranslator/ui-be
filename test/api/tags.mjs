/* Standalone sanity test for happy paths of the tags API:
 *
 *   POST   /api/v1/users/me/tags            create a tag (save_type "tag")
 *   GET    /api/v1/users/me/tags            list the current user's tags
 *   POST   /api/v1/users/me/tags/:save_id   update one tag (body must include its id)
 *   DELETE /api/v1/users/me/tags/:save_id   soft-delete one tag
 *
 * Exercises the create -> list -> update -> delete lifecycle.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/tags.mjs
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

console.log(`# user tags lifecycle  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test tag ${new Date().toISOString()}`;

  // Create
  const create = await req('POST', '/api/v1/users/me/tags', { save_type: 'tag', label });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const tag = create.json;
  ok(tag && tag.id !== undefined && tag.id !== null, 'created tag has an id');
  ok(tag && tag.user_id === TEST_USER_ID, 'created tag belongs to the test user');
  ok(tag && tag.save_type === 'tag', 'created tag has save_type "tag"');

  const tagId = tag ? tag.id : null;

  // List
  const list = await req('GET', '/api/v1/users/me/tags');
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  ok(Array.isArray(list.json) && tagId !== null && list.json.some((t) => t.id === tagId),
    'created tag appears in the tags list');
  ok(Array.isArray(list.json) && list.json.every((t) => t.save_type === 'tag'),
    'tags list only contains tag saves');

  // Update (POST to the by-id route; the partial update keys off the id in the body)
  const newLabel = `${label} (updated)`;
  const update = await req('POST', `/api/v1/users/me/tags/${tagId}`, { id: tagId, label: newLabel });
  ok(update.res.status === 200, `update responds 200 (got ${update.res.status})`);
  ok(update.json && update.json.label === newLabel, 'update echoes the new label');

  // Delete (soft)
  const del = await req('DELETE', `/api/v1/users/me/tags/${tagId}`);
  ok(del.res.status === 204, `delete responds 204 (got ${del.res.status})`);

  // Confirm it no longer shows up (default list excludes deleted)
  const listAfter = await req('GET', '/api/v1/users/me/tags');
  ok(Array.isArray(listAfter.json) && !listAfter.json.some((t) => t.id === tagId),
    'deleted tag no longer appears in the tags list');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
