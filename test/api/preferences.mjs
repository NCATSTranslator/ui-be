/* Standalone sanity testing for the happy paths of the preferences API:
 *
 *   POST /api/v1/users/me/preferences   upsert preferences, returns the full preference set
 *   GET  /api/v1/users/me/preferences   fetch the current user's preferences
 *
 * This test assumes that the `preferences` table has been pre-populated.
 * (see utilities/sql/populatePreferencesTable.sql / assets/db/pref_data.csv)
 * If the table is empty this test returns 400.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/preferences.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { BYPASS_TEST_USER } from '../../mock/auth.mjs';

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const TEST_USER_ID = BYPASS_TEST_USER.id;
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

// A preference name that exists in the seeded preferences table.
const PREF_NAME = 'result_sort';

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

console.log(`# user preferences  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const value = `date_high_${Date.now()}`;

  // Upsert one preference
  const update = await req('POST', '/api/v1/users/me/preferences', {
    user_id: TEST_USER_ID,
    preferences: { [PREF_NAME]: { pref_value: value, pref_data_type: 'string' } },
  });
  ok(update.res.status === 200, `update responds 200 (got ${update.res.status}; is the preferences table seeded?)`);
  ok(update.json && update.json.preferences && update.json.preferences[PREF_NAME],
    `update response includes "${PREF_NAME}"`);
  ok(update.json && update.json.preferences && update.json.preferences[PREF_NAME]
    && update.json.preferences[PREF_NAME].pref_value === value,
    'update response echoes the new value');

  // Read it back
  const get = await req('GET', '/api/v1/users/me/preferences');
  ok(get.res.status === 200, `get responds 200 (got ${get.res.status})`);
  ok(get.json && get.json.preferences && get.json.preferences[PREF_NAME]
    && get.json.preferences[PREF_NAME].pref_value === value,
    'persisted value is returned on read-back');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
