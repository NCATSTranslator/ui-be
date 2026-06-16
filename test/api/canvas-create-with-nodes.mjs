/* Standalone API test: POST /api/v1/users/me/canvas with graph nodes
 *
 * Creates a canvas whose request includes a graph with nodes, exercising the full node-persistence
 * path: request parse -> node-data upsert -> canvas_node placement, all in one transaction.
 *
 * There is no GET-graph endpoint yet, so node persistence is verified indirectly: node inserts run in
 * the same transaction as the canvas, so a 200 means they committed (a failure would roll back and
 * return 500). The test drives three shared-node upsert paths: creating with new nodes; a second
 * canvas reusing the same refs with identical data (ON CONFLICT, no-op write -- a plain insert would
 * fail here with a unique violation); and a canvas reusing the same refs with changed data
 * (ON CONFLICT, update branch). Asserting the node data actually changed will be possible once the
 * GET-graph endpoint exists; for now each path asserts 200.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs): auth checking is
 * bypassed and every request resolves to the fixed test user, so no session cookie is needed. This
 * hits a real Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                           # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-create-with-nodes.mjs # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { readFileSync } from 'node:fs';
import { BYPASS_TEST_USER } from '../../mock/auth.mjs';
import { SummaryNode } from '../../lib/summarization/SummaryNode.mjs';
import * as cmn from '../../lib/common.mjs';

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const TEST_USER_ID = BYPASS_TEST_USER.id;
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

// Nodes placed on a canvas must carry the HMAC signature the server mints when it generates a
// summary. We read the same secret the mock-ars server loads (configurations/secrets/secrets.json
// via mock.json's _load_secrets) and sign each node over its normalized form, exactly as the server
// will re-derive and verify it. If the server runs with a different secrets file this will mismatch.
const SIGNING_SECRET = JSON.parse(
  readFileSync(new URL('../../configurations/secrets/secrets.json', import.meta.url))).hmac.key;

// Stable node refs so re-runs (and the second canvas below) hit the shared-node upsert path.
const NODE_REF_1 = 'API_TEST:node-1';
const NODE_REF_2 = 'API_TEST:node-2';

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

// A graph node is a SummaryNode (flattened) plus canvas placement fields (x, y, optional hidden).
// The map key is authoritative for the node id, so the value need not repeat it.
function testNode(ref, name, type, x, y, extra = {}) {
  return {
    aras: ['api-test-ara'],
    descriptions: [`${name} description`],
    names: [name],
    types: [type],
    synonyms: [],
    curies: [ref],
    provenance: [],
    tags: {},
    x: x,
    y: y,
    ...extra,
  };
}

// Sign a node exactly as the server does: over SummaryNode.from_object(node).to_raw_obj() with the
// map key supplied as the authoritative id. Returns the node with its `signature` attached.
function signNode(ref, node) {
  const raw = SummaryNode.from_object({ ...node, id: ref }).to_raw_obj();
  return { ...node, signature: cmn.sign_entity_data(raw, SIGNING_SECRET) };
}

// variant alters the SummaryNode data (names/descriptions) for the same refs, so re-submitting it
// exercises the upsert's update branch (node.data IS DISTINCT FROM the stored data). Note x/y live on
// the canvas_node placement, not node.data, so changing them alone would NOT count as a data change.
function graphWithNodes(variant = '') {
  const suffix = variant ? ` ${variant}` : '';
  return {
    nodes: {
      // node 1 carries annotations to exercise lossless round-trip + signing of annotation data
      [NODE_REF_1]: signNode(NODE_REF_1, testNode(NODE_REF_1, `API Test Node One${suffix}`, 'biolink:Disease', 10, 20,
        { annotations: { disease: { mondo: ['MONDO:0005148'] } } })),
      [NODE_REF_2]: signNode(NODE_REF_2, testNode(NODE_REF_2, `API Test Node Two${suffix}`, 'biolink:ChemicalEntity', 30, 40, { hidden: true })),
    },
    edges: {},
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
}

async function postCanvas(body) {
  const res = await fetch(`${BASE_URL}/api/v1/users/me/canvas`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const raw = await res.text();
  showResponse('POST', '/api/v1/users/me/canvas', res, raw);
  let json = null;
  if (raw) { try { json = JSON.parse(raw); } catch { /* non-JSON */ } }
  return { res, json };
}

console.log(`# POST /api/v1/users/me/canvas with graph nodes  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas+nodes ${new Date().toISOString()}`;
  const layout = 'horizontal';

  // Create a canvas that includes a graph with nodes.
  const create = await postCanvas({ label, layout, graph: graphWithNodes() });
  ok(create.res.status === 200, `create-with-nodes responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');
  ok(canvas && canvas.label === label, 'created canvas echoes the label');
  ok(canvas && canvas.layout === layout, 'created canvas echoes the layout');
  ok(canvas && canvas.user_id === TEST_USER_ID, 'created canvas belongs to the test user');
  // The graph's source is stored on canvas.data (see make_user_canvas_from_req).
  ok(canvas && canvas.data && canvas.data.query_ref === 'API_TEST_QID', 'canvas data carries graph source query_ref');
  ok(canvas && canvas.data && canvas.data.result_ref === 'API_TEST_RID', 'canvas data carries graph source result_ref');

  // Round-trip: the new canvas appears in the user's canvas list.
  const listRes = await fetch(`${BASE_URL}/api/v1/users/me/canvas`);
  const listRaw = await listRes.text();
  showResponse('GET', '/api/v1/users/me/canvas', listRes, listRaw);
  const canvases = listRaw ? JSON.parse(listRaw) : null;
  ok(listRes.status === 200, `list responds 200 (got ${listRes.status})`);
  ok(Array.isArray(canvases) && canvas && canvases.some((c) => c.id === canvas.id),
    'created canvas appears in the canvas list');

  // Dedup: a second canvas reusing the SAME node refs must succeed. Node data is shared across
  // canvases and keyed by ref, so the store upserts (ON CONFLICT) instead of colliding.
  const create2 = await postCanvas({ label: `${label} (2)`, layout, graph: graphWithNodes() });
  ok(create2.res.status === 200, `second canvas reusing the same node refs responds 200 (got ${create2.res.status})`);
  ok(create2.json && create2.json.id !== undefined && create2.json.id !== (canvas && canvas.id),
    'second canvas is a distinct canvas sharing the same node data');

  // Update: reuse the SAME node refs but with CHANGED data, exercising the upsert's update branch
  // (node.data IS DISTINCT FROM the stored data -> the shared node rows are updated, not skipped).
  // Verifying the data actually changed needs the (not-yet-built) GET-graph endpoint; for now we
  // assert the path succeeds.
  const createUpdated = await postCanvas({ label: `${label} (updated nodes)`, layout, graph: graphWithNodes('UPDATED') });
  ok(createUpdated.res.status === 200, `canvas with updated node data responds 200 (got ${createUpdated.res.status})`);
  ok(createUpdated.json && createUpdated.json.id !== undefined && createUpdated.json.id !== null,
    'canvas with updated node data has an id');

  // Security: a node whose data was altered after signing must be rejected. Sign a node, then mutate
  // a signed field (names) -- the server re-derives a different hash than the stale signature.
  const tampered = graphWithNodes();
  tampered.nodes[NODE_REF_1].names = ['Tampered Name'];
  const tamper = await postCanvas({ label: `${label} (tampered)`, layout, graph: tampered });
  ok(tamper.res.status === 400, `node with tampered data is rejected with 400 (got ${tamper.res.status})`);

  // Security: a node with no signature at all must also be rejected.
  const unsigned = graphWithNodes();
  delete unsigned.nodes[NODE_REF_2].signature;
  const missing = await postCanvas({ label: `${label} (unsigned)`, layout, graph: unsigned });
  ok(missing.res.status === 400, `node with no signature is rejected with 400 (got ${missing.res.status})`);
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
