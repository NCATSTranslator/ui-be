/* Standalone test for the API key endpoints:
 *   GET    /api/v1/users/me/api-keys
 *   POST   /api/v1/users/me/api-keys
 *   DELETE /api/v1/users/me/api-keys/:key_id
 *
 * Also covers using a key as a credential on a normal API route, and the rule that keys
 * cannot manage keys.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/api-keys.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, postJson, deleteJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { API_KEY_PREFIX, generateApiKey } from '../../models/ApiKey.mjs';

const { ok, fail, finish } = createHarness();
const KEY_PATH = '/api/v1/users/me/api-keys';
const KEY_NAME = `api-keys test ${Date.now()}`;

console.log(`# API keys  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);

let created = null;
let rawKey = null;

try {
  const { res, json } = await postJson(KEY_PATH, { name: KEY_NAME });
  ok(res.status === 200, `POST ${KEY_PATH} responds 200 (got ${res.status})`);
  created = json && json.api_key;
  rawKey = json && json.key;
  ok(typeof rawKey === 'string' && rawKey.startsWith(API_KEY_PREFIX), 'returns a raw key with the expected prefix');
  ok(created && created.name === KEY_NAME, 'returns the key metadata under the requested name');
  ok(created && created.user_id === TEST_USER_ID, 'assigns the key to the session user');
  ok(created && created.key_hash === undefined, 'never returns the stored key hash');
  ok(created && created.time_revoked === null, 'new key starts unrevoked');
} catch (err) {
  fail(`create request failed: ${err.message} -- is the server running with auth_check=false?`);
}

try {
  const { res, json } = await postJson(KEY_PATH, {});
  ok(res.status === 400, `POST without a name responds 400 (got ${res.status})`);
} catch (err) {
  fail(`nameless create request failed: ${err.message}`);
}

try {
  const { res, json } = await getJson(KEY_PATH);
  ok(res.status === 200, `GET ${KEY_PATH} responds 200 (got ${res.status})`);
  const listed = Array.isArray(json) ? json.find((k) => created && k.id === created.id) : null;
  ok(!!listed, 'lists the key just created');
  ok(listed && listed.key_display && !listed.key_display.includes(rawKey.slice(API_KEY_PREFIX.length)),
    'lists only a truncated display form, not the key itself');
  ok(Array.isArray(json) && json.every((k) => k.key_hash === undefined), 'never lists stored key hashes');
} catch (err) {
  fail(`list request failed: ${err.message}`);
}

// The key should now work as a credential in its own right.
try {
  const { res, json } = await getJson('/api/v1/users/me', { Authorization: `Bearer ${rawKey}` });
  ok(res.status === 200, `GET /api/v1/users/me with a bearer key responds 200 (got ${res.status})`);
  ok(json && json.id === TEST_USER_ID, 'resolves the key to its owning user');
} catch (err) {
  fail(`bearer auth request failed: ${err.message}`);
}

try {
  const { res } = await getJson('/api/v1/users/me', { 'X-API-Key': rawKey });
  ok(res.status === 200, `GET /api/v1/users/me with an X-API-Key header responds 200 (got ${res.status})`);
} catch (err) {
  fail(`x-api-key auth request failed: ${err.message}`);
}

try {
  const { res } = await getJson('/api/v1/users/me', { Authorization: `Bearer ${generateApiKey()}` });
  ok(res.status === 401, `an unknown but well-formed key responds 401 (got ${res.status})`);
} catch (err) {
  fail(`unknown key request failed: ${err.message}`);
}

try {
  const { res } = await getJson('/api/v1/users/me', { Authorization: `Bearer ${API_KEY_PREFIX}nonsense` });
  ok(res.status === 401, `a malformed key responds 401 (got ${res.status})`);
} catch (err) {
  fail(`malformed key request failed: ${err.message}`);
}

// Keys must not be usable to manage keys.
try {
  const { res } = await getJson(KEY_PATH, { Authorization: `Bearer ${rawKey}` });
  ok(res.status === 403, `GET ${KEY_PATH} with a key responds 403 (got ${res.status})`);
} catch (err) {
  fail(`key-managing-keys request failed: ${err.message}`);
}

try {
  const { res } = await postJson(KEY_PATH, { name: 'minted by a key' }, { Authorization: `Bearer ${rawKey}` });
  ok(res.status === 403, `POST ${KEY_PATH} with a key responds 403 (got ${res.status})`);
} catch (err) {
  fail(`key-minting-keys request failed: ${err.message}`);
}

try {
  const { res } = await deleteJson(`${KEY_PATH}/not-a-uuid`);
  ok(res.status === 400, `DELETE with a malformed id responds 400 (got ${res.status})`);
} catch (err) {
  fail(`malformed revoke request failed: ${err.message}`);
}

try {
  const { res, json } = await deleteJson(`${KEY_PATH}/${created.id}`);
  ok(res.status === 200, `DELETE ${KEY_PATH}/:key_id responds 200 (got ${res.status})`);
  ok(json && json.time_revoked !== null, 'returns the key marked revoked');
} catch (err) {
  fail(`revoke request failed: ${err.message}`);
}

try {
  const { res } = await deleteJson(`${KEY_PATH}/${created.id}`);
  ok(res.status === 404, `revoking an already-revoked key responds 404 (got ${res.status})`);
} catch (err) {
  fail(`double revoke request failed: ${err.message}`);
}

try {
  const { res } = await getJson('/api/v1/users/me', { Authorization: `Bearer ${rawKey}` });
  ok(res.status === 401, `a revoked key responds 401 (got ${res.status})`);
} catch (err) {
  fail(`revoked key request failed: ${err.message}`);
}

try {
  const { json } = await getJson(KEY_PATH);
  ok(Array.isArray(json) && !json.some((k) => k.id === created.id), 'revoked key is omitted from the default listing');
  const { json: all } = await getJson(`${KEY_PATH}?include_revoked=true`);
  ok(Array.isArray(all) && all.some((k) => k.id === created.id), 'revoked key appears with include_revoked=true');
} catch (err) {
  fail(`post-revoke list request failed: ${err.message}`);
}

finish();
