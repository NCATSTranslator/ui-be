/* Standalone API test: PUT /api/v1/users/me/canvas/:save_id/graph/move.
 *
 * Updates the x/y positions of one or more Canvas Nodes in a single atomic operation. This is the
 * dedicated move endpoint that the element-update endpoint (label/hidden) defers position changes to.
 * Only active nodes on the canvas are moved; the endpoint returns the updated Canvas Node rows.
 *
 * Unique refs per run keep the test isolated from the shared entity pool. Assumes the server runs
 * with "auth_check": false (see mock/auth.mjs) against a real Postgres (the mock-ars server):
 *
 *   npm run mock-ars                         # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-move.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# PUT ${CANVAS_PATH}/:save_id/graph/move  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:move-A-${s}`;
  const refB = `API_TEST:move-B-${s}`;
  const eAB = `${refA}->${refB}`;

  // Create a canvas with two nodes (known starting positions) and one edge between them.
  const createGraph = {
    nodes: {
      [refA]: signNode(refA, testNode(refA, 'Move A', 'biolink:Disease', 10, 20)),
      [refB]: signNode(refB, testNode(refB, 'Move B', 'biolink:ChemicalEntity', 30, 40)),
    },
    edges: { [eAB]: signEdge(eAB, testEdge(refA, refB, 'biolink:treats')) },
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
  const create = await postCanvas({ label: `api-test graph-move ${s}`, layout: 'custom', graph: createGraph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  const initial = await getJson(`${CANVAS_PATH}/${id}/graph`);
  const nodeDataId = new Map((initial.json.nodes || []).map((n) => [n.ref, n.data_id]));
  const nodeAId = nodeDataId.get(refA);
  const nodeBId = nodeDataId.get(refB);
  ok(nodeAId != null && nodeBId != null, 'read back the node data ids');

  const movePath = `${CANVAS_PATH}/${id}/graph/move`;
  const byId = (rows) => new Map((rows || []).map((n) => [n.data_id, n]));

  // Move both nodes in one request; the response carries the updated positions.
  const moveBoth = await putJson(movePath, {
    nodes: [
      { data_id: nodeAId, x: 100.5, y: 200.5 },
      { data_id: nodeBId, x: 300, y: 400 },
    ],
  });
  ok(moveBoth.res.status === 200, `move both responds 200 (got ${moveBoth.res.status})`);
  ok(Array.isArray(moveBoth.json) && moveBoth.json.length === 2, 'move returns both updated nodes');
  const moved = byId(moveBoth.json);
  ok(moved.get(nodeAId) && moved.get(nodeAId).x === 100.5 && moved.get(nodeAId).y === 200.5,
    `node A is at its new position (got ${moved.get(nodeAId) && moved.get(nodeAId).x},${moved.get(nodeAId) && moved.get(nodeAId).y})`);
  ok(moved.get(nodeBId) && moved.get(nodeBId).x === 300 && moved.get(nodeBId).y === 400,
    `node B is at its new position (got ${moved.get(nodeBId) && moved.get(nodeBId).x},${moved.get(nodeBId) && moved.get(nodeBId).y})`);
  ok(moved.get(nodeAId) && moved.get(nodeAId).label === 'Move A' && moved.get(nodeAId).hidden === false,
    'a move leaves label and hidden untouched');

  // The new positions persist and are visible on a fresh graph read.
  const after = await getJson(`${CANVAS_PATH}/${id}/graph`);
  const afterById = new Map((after.json.nodes || []).map((n) => [n.data_id, n]));
  ok(afterById.get(nodeAId) && afterById.get(nodeAId).x === 100.5 && afterById.get(nodeAId).y === 200.5,
    'the move persists across a graph read');

  // A single-node move works too.
  const moveOne = await putJson(movePath, { nodes: [{ data_id: nodeBId, x: 1, y: 2 }] });
  ok(moveOne.res.status === 200, `single move responds 200 (got ${moveOne.res.status})`);
  ok(Array.isArray(moveOne.json) && moveOne.json.length === 1 && moveOne.json[0].x === 1 && moveOne.json[0].y === 2,
    'single move returns just the moved node at its new position');

  // An unknown data id on an existing canvas is a no-op for that id (no row updated), not an error.
  const moveUnknown = await putJson(movePath, { nodes: [{ data_id: 999999999, x: 5, y: 6 }] });
  ok(moveUnknown.res.status === 200, `move of an unknown node id responds 200 (got ${moveUnknown.res.status})`);
  ok(Array.isArray(moveUnknown.json) && moveUnknown.json.length === 0, 'an unknown node id moves nothing');

  // --- Validation ---

  const emptyNodes = await putJson(movePath, { nodes: [] });
  ok(emptyNodes.res.status === 400, `empty nodes array -> 400 (got ${emptyNodes.res.status})`);

  const noNodes = await putJson(movePath, {});
  ok(noNodes.res.status === 400, `missing nodes -> 400 (got ${noNodes.res.status})`);

  const missingCoords = await putJson(movePath, { nodes: [{ data_id: nodeAId }] });
  ok(missingCoords.res.status === 400, `missing x/y -> 400 (got ${missingCoords.res.status})`);

  const badCoords = await putJson(movePath, { nodes: [{ data_id: nodeAId, x: 'a', y: 2 }] });
  ok(badCoords.res.status === 400, `non-numeric x -> 400 (got ${badCoords.res.status})`);

  const badDataId = await putJson(movePath, { nodes: [{ data_id: 'x', x: 1, y: 2 }] });
  ok(badDataId.res.status === 400, `non-integer data_id -> 400 (got ${badDataId.res.status})`);

  // Unknown canvas id is a 404; non-numeric ids are 400s.
  const missingCanvas = await putJson(`${CANVAS_PATH}/999999999/graph/move`, { nodes: [{ data_id: nodeAId, x: 1, y: 2 }] });
  ok(missingCanvas.res.status === 404, `unknown canvas id -> 404 (got ${missingCanvas.res.status})`);

  const badCanvasId = await putJson(`${CANVAS_PATH}/not-a-number/graph/move`, { nodes: [{ data_id: nodeAId, x: 1, y: 2 }] });
  ok(badCanvasId.res.status === 400, `non-numeric canvas id -> 400 (got ${badCanvasId.res.status})`);

  // A soft-deleted node cannot be moved (the update is gated on time_deleted IS NULL).
  const trash = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: [nodeAId] });
  ok(trash.res.status === 200, `trash node responds 200 (got ${trash.res.status})`);
  const moveTrashed = await putJson(movePath, { nodes: [{ data_id: nodeAId, x: 7, y: 8 }] });
  ok(moveTrashed.res.status === 200, `moving a soft-deleted node responds 200 (got ${moveTrashed.res.status})`);
  ok(Array.isArray(moveTrashed.json) && moveTrashed.json.length === 0, 'a soft-deleted node is not moved');
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
