/* Standalone API test: PUT /api/v1/users/me/canvas/:save_id/graph/trash and /restore.
 *
 * These soft delete (and restore) the display-only Canvas Nodes and Edges, leaving the underlying
 * shared data pool untouched. The interesting rule: soft deleting a node also soft deletes every
 * edge attached to it (an edge cannot dangle), and restoring an edge only succeeds when both of its
 * endpoint nodes are active. Both endpoints return the resulting CanvasGraph (active entities only).
 *
 * Unique refs per run keep the test isolated from the shared entity pool. Assumes the server runs
 * with "auth_check": false (see mock/auth.mjs) against a real Postgres (the mock-ars server):
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-trash.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, postJson, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

const refsOf = (rows) => new Set((Array.isArray(rows) ? rows : []).map((r) => r.ref));

console.log(`# PUT ${CANVAS_PATH}/:save_id/graph/{trash,restore}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:trash-A-${s}`;
  const refB = `API_TEST:trash-B-${s}`;
  const refC = `API_TEST:trash-C-${s}`;
  const eAB = `${refA}->${refB}`;
  const eBC = `${refB}->${refC}`;

  // Create a canvas: A -> B -> C. B is the cut vertex; trashing it must take both edges with it.
  const createGraph = {
    nodes: {
      [refA]: signNode(refA, testNode(refA, 'Trash A', 'biolink:Disease', 10, 20)),
      [refB]: signNode(refB, testNode(refB, 'Trash B', 'biolink:ChemicalEntity', 30, 40)),
      [refC]: signNode(refC, testNode(refC, 'Trash C', 'biolink:Gene', 50, 60)),
    },
    edges: {
      [eAB]: signEdge(eAB, testEdge(refA, refB, 'biolink:treats')),
      [eBC]: signEdge(eBC, testEdge(refB, refC, 'biolink:treats')),
    },
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
  const create = await postCanvas({ label: `api-test trash ${s}`, layout: 'horizontal', graph: createGraph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  // The server assigns data ids (the shared-pool ids); read them back so we can address entities.
  const initial = await getJson(`${CANVAS_PATH}/${id}/graph`);
  ok(initial.res.status === 200, `initial graph fetch responds 200 (got ${initial.res.status})`);
  const nodeDataId = new Map((initial.json.nodes || []).map((n) => [n.ref, n.data_id]));
  const edgeDataId = new Map((initial.json.edges || []).map((e) => [e.ref, e.data_id]));
  ok(initial.json.nodes.length === 3 && initial.json.edges.length === 2, 'canvas starts with 3 nodes and 2 edges');

  // Trash node B: it should cascade to both edges (A->B and B->C), leaving A and C with no edges.
  const trashB = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: [nodeDataId.get(refB)] });
  ok(trashB.res.status === 200, `trash node responds 200 (got ${trashB.res.status})`);
  let refs = refsOf(trashB.json.nodes);
  ok(trashB.json.nodes.length === 2 && refs.has(refA) && refs.has(refC) && !refs.has(refB),
    'trashing B leaves only A and C active');
  ok(trashB.json.edges.length === 0, 'trashing B cascades to both attached edges');

  // Restore B and both edges: with B active again the edges satisfy the endpoint guard and come back.
  const restoreAll = await putJson(`${CANVAS_PATH}/${id}/graph/restore`,
    { nodes: [nodeDataId.get(refB)], edges: [edgeDataId.get(eAB), edgeDataId.get(eBC)] });
  ok(restoreAll.res.status === 200, `restore responds 200 (got ${restoreAll.res.status})`);
  ok(restoreAll.json.nodes.length === 3 && restoreAll.json.edges.length === 2,
    'restoring B and its edges brings the whole graph back');

  // Trash a single edge directly (no node cascade): only A->B goes, B->C and all nodes stay.
  const trashEdge = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { edges: [edgeDataId.get(eAB)] });
  ok(trashEdge.res.status === 200, `trash edge responds 200 (got ${trashEdge.res.status})`);
  ok(trashEdge.json.nodes.length === 3, 'trashing an edge leaves every node active');
  ok(trashEdge.json.edges.length === 1 && refsOf(trashEdge.json.edges).has(eBC),
    'trashing an edge removes only that edge');

  // Restore the edge: both endpoints are active, so it returns.
  const restoreEdge = await putJson(`${CANVAS_PATH}/${id}/graph/restore`, { edges: [edgeDataId.get(eAB)] });
  ok(restoreEdge.res.status === 200, `restore edge responds 200 (got ${restoreEdge.res.status})`);
  ok(restoreEdge.json.edges.length === 2, 'restoring the edge brings it back');

  // Restore guard: trash B (cascading its edges), then try to restore ONLY the edges. B is still
  // deleted, so neither edge may be restored into a dangling state.
  await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: [nodeDataId.get(refB)] });
  const restoreDangling = await putJson(`${CANVAS_PATH}/${id}/graph/restore`,
    { edges: [edgeDataId.get(eAB), edgeDataId.get(eBC)] });
  ok(restoreDangling.res.status === 200, `guarded restore responds 200 (got ${restoreDangling.res.status})`);
  ok(restoreDangling.json.edges.length === 0,
    'edges are not restored while an endpoint node is still deleted');

  // Restoring B first, then the edges, succeeds.
  await putJson(`${CANVAS_PATH}/${id}/graph/restore`, { nodes: [nodeDataId.get(refB)] });
  const restoreThenEdges = await putJson(`${CANVAS_PATH}/${id}/graph/restore`,
    { edges: [edgeDataId.get(eAB), edgeDataId.get(eBC)] });
  ok(restoreThenEdges.json.nodes.length === 3 && restoreThenEdges.json.edges.length === 2,
    'restoring the node first lets the edges follow');

  // An empty selection is a 400.
  const empty = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, {});
  ok(empty.res.status === 400, `empty selection -> 400 (got ${empty.res.status})`);

  // A non-array selection field is a 400.
  const notArray = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: 'nope' });
  ok(notArray.res.status === 400, `non-array selection -> 400 (got ${notArray.res.status})`);

  // A non-integer id is a 400.
  const notInt = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: [1.5] });
  ok(notInt.res.status === 400, `non-integer id -> 400 (got ${notInt.res.status})`);

  // Trashing in a canvas that does not exist is a 404.
  const missing = await putJson(`${CANVAS_PATH}/999999999/graph/trash`, { nodes: [1] });
  ok(missing.res.status === 404, `unknown canvas id -> 404 (got ${missing.res.status})`);

  // A non-numeric canvas id is a 400.
  const badId = await putJson(`${CANVAS_PATH}/not-a-number/graph/trash`, { nodes: [1] });
  ok(badId.res.status === 400, `non-numeric canvas id -> 400 (got ${badId.res.status})`);

  // The graph ended whole: the failed requests above left it intact.
  const finalRes = await getJson(`${CANVAS_PATH}/${id}/graph`);
  ok(finalRes.res.status === 200, `final graph fetch responds 200 (got ${finalRes.res.status})`);
  ok(finalRes.json.nodes.length === 3 && finalRes.json.edges.length === 2,
    'the canvas ends with all 3 nodes and 2 edges active');

  // A trashed canvas is gone: both graph/trash and graph/restore on it are 404. This canvas exists
  // but is soft-deleted, so it is distinct from the unknown-canvas 404 above.
  const trashCanvas = await putJson(`${CANVAS_PATH}/trash`, [id]);
  ok(trashCanvas.res.status === 200, `trash canvas responds 200 (got ${trashCanvas.res.status})`);
  const trashOnTrashed = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { nodes: [nodeDataId.get(refA)] });
  ok(trashOnTrashed.res.status === 404, `graph/trash on a trashed canvas -> 404 (got ${trashOnTrashed.res.status})`);
  const restoreOnTrashed = await putJson(`${CANVAS_PATH}/${id}/graph/restore`, { nodes: [nodeDataId.get(refA)] });
  ok(restoreOnTrashed.res.status === 404, `graph/restore on a trashed canvas -> 404 (got ${restoreOnTrashed.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
