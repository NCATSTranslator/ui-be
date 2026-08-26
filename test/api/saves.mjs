/* Standalone sanity test for happy paths of the saves API:
 *
 *   POST   /api/v1/users/me/saves            create a save
 *   GET    /api/v1/users/me/saves            list the current user's saves
 *   GET    /api/v1/users/me/saves/:save_id   fetch one save
 *   PUT    /api/v1/users/me/saves/:save_id   update one save (body must include its id)
 *   DELETE /api/v1/users/me/saves/:save_id   soft-delete one save
 *
 * Exercises the full create -> list -> fetch -> update -> delete lifecycle.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/saves.mjs
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

console.log(`# user saves lifecycle  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test save ${new Date().toISOString()}`;

  // Create
  const create = await req('POST', '/api/v1/users/me/saves',
    { save_type: 'query', label, notes: 'created by saves.mjs', data: { source: 'saves.mjs' } });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const save = create.json;
  ok(save && save.id !== undefined && save.id !== null, 'created save has an id');
  ok(save && save.user_id === TEST_USER_ID, 'created save belongs to the test user');
  ok(save && save.save_type === 'query', 'created save echoes the save_type');
  ok(save && save.label === label, 'created save echoes the label');

  const saveId = save ? save.id : null;

  // List
  const list = await req('GET', '/api/v1/users/me/saves');
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  ok(Array.isArray(list.json) && saveId !== null && list.json.some((s) => s.id === saveId),
    'created save appears in the saves list');

  // Fetch one
  const get = await req('GET', `/api/v1/users/me/saves/${saveId}`);
  ok(get.res.status === 200, `fetch-by-id responds 200 (got ${get.res.status})`);
  ok(get.json && get.json.id === saveId, 'fetch-by-id returns the created save');

  // Update one (the partial update keys off the id in the body, not the URL)
  const newLabel = `${label} (updated)`;
  const update = await req('PUT', `/api/v1/users/me/saves/${saveId}`, { id: saveId, label: newLabel });
  ok(update.res.status === 200, `update responds 200 (got ${update.res.status})`);
  ok(update.json && update.json.label === newLabel, 'update echoes the new label');

  // Confirm the update persisted
  const getUpdated = await req('GET', `/api/v1/users/me/saves/${saveId}`);
  ok(getUpdated.json && getUpdated.json.label === newLabel, 'updated label persisted');

  // Delete (soft)
  const del = await req('DELETE', `/api/v1/users/me/saves/${saveId}`);
  ok(del.res.status === 204, `delete responds 204 (got ${del.res.status})`);

  // Confirm it no longer shows up (default list excludes deleted)
  const listAfter = await req('GET', '/api/v1/users/me/saves');
  ok(Array.isArray(listAfter.json) && !listAfter.json.some((s) => s.id === saveId),
    'deleted save no longer appears in the saves list');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
