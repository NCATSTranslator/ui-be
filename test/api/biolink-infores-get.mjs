/* Standalone API test: GET /api/v1/biolink/infores/:infores_id.
 *
 * Returns the infores catalog entry ({ name, url, wiki, knowledge_level }) for a single infores id -
 * the same entry a query result carries in its provenance map. The FE uses it to name and link
 * evidence sources for a Canvas Edge when no result set is loaded. An id that is not in the catalog
 * (including an inherited object key such as `constructor`) is a 404.
 *
 * The route is open and reads only the in-memory biolink catalog, so no database state is involved.
 * The expected entry comes from the catalog mock.json loads (infores-catalog-v1.1.8.json). Run it
 * against the mock-ars server:
 *
 *   npm run mock-ars                        # shell 1: start the server
 *   node test/api/biolink-infores-get.mjs   # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL } from '../lib/api-harness.mjs';

const { ok, fail, finish } = createHarness();

const INFORES_PATH = '/api/v1/biolink/infores';
const KNOWN_INFORES = 'infores:semmeddb';
const KNOWN_NAME = 'Semantic Medline Database';

console.log(`# GET ${INFORES_PATH}/:infores_id  (target: ${BASE_URL})`);
try {
  const encoded = await getJson(`${INFORES_PATH}/${encodeURIComponent(KNOWN_INFORES)}`);
  const entry = encoded.json;
  ok(encoded.res.status === 200, `known infores responds 200 (got ${encoded.res.status})`);
  ok(entry?.name === KNOWN_NAME, `returns the catalog name (got ${entry?.name})`);
  ok(typeof entry?.knowledge_level === 'string', 'returns a knowledge_level');
  ok(!!entry && 'url' in entry && 'wiki' in entry, 'returns url and wiki fields');

  const unencoded = await getJson(`${INFORES_PATH}/${KNOWN_INFORES}`);
  ok(unencoded.res.status === 200, `unencoded infores id responds 200 (got ${unencoded.res.status})`);
  ok(unencoded.json?.name === KNOWN_NAME, 'unencoded infores id resolves to the same entry');

  const unknown = await getJson(`${INFORES_PATH}/${encodeURIComponent('infores:api-test-not-a-source')}`);
  ok(unknown.res.status === 404, `unknown infores responds 404 (got ${unknown.res.status})`);

  const inherited = await getJson(`${INFORES_PATH}/constructor`);
  ok(inherited.res.status === 404, `inherited object key responds 404 (got ${inherited.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running?`);
}

finish();
