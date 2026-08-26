/* Standalone API test: POST /api/v1/users/me/canvas with graph nodes and edges (happy path)
 *
 * Creates a canvas whose request includes a graph with nodes and an edge, exercising the full
 * persistence path: request parse -> node-data upsert -> canvas_node placement -> edge-data upsert ->
 * canvas_edge placement (with subject/object resolved to the just-created node data ids), all in one
 * transaction. There is no GET-graph endpoint yet, so persistence is verified indirectly: the inserts
 * run in the same transaction as the canvas, so a 200 means they committed (a failure would roll back
 * and return 500).
 *
 * The shared-entity upsert lifecycle lives in canvas-graph-entity-upsert.mjs; signature and structural
 * rejections live in canvas-graph-validation.mjs.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                                # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-create-with-graph.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, graphWithNodesAndEdges, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# POST ${CANVAS_PATH} with graph nodes and edges  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas+graph ${new Date().toISOString()}`;
  const layout = 'horizontal';

  const create = await postCanvas({ label, layout, graph: graphWithNodesAndEdges() });
  ok(create.res.status === 200, `create-with-graph responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');
  ok(canvas && canvas.label === label, 'created canvas echoes the label');
  ok(canvas && canvas.layout === layout, 'created canvas echoes the layout');
  ok(canvas && canvas.user_id === TEST_USER_ID, 'created canvas belongs to the test user');
  // The graph's source is stored on canvas.data (see make_user_canvas_from_req).
  ok(canvas && canvas.data && canvas.data.query_ref === 'API_TEST_QID', 'canvas data carries graph source query_ref');
  ok(canvas && canvas.data && canvas.data.result_ref === 'API_TEST_RID', 'canvas data carries graph source result_ref');

  // Round-trip: the new canvas appears in the user's canvas list.
  const list = await getJson(CANVAS_PATH);
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  ok(Array.isArray(list.json) && canvas && list.json.some((c) => c.id === canvas.id),
    'created canvas appears in the canvas list');

  // A graph with nodes but no edges must also persist (the edge-creation branch is skipped).
  const nodesOnly = graphWithNodesAndEdges();
  delete nodesOnly.edges;
  const createNodesOnly = await postCanvas({ label: `${label} (nodes only)`, layout, graph: nodesOnly });
  ok(createNodesOnly.res.status === 200, `canvas with nodes but no edges responds 200 (got ${createNodesOnly.res.status})`);
  ok(createNodesOnly.json && createNodesOnly.json.id !== undefined && createNodesOnly.json.id !== null,
    'canvas with nodes but no edges has an id');
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
