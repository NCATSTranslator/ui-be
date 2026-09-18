/* Standalone API test: PUT /api/v1/users/me/canvas/:save_id/node/:data_id and .../edge/:data_id.
 *
 * These partially update the display-only properties (label, hidden) of a single Canvas Node or
 * Edge. They never touch node position (x/y) - that is handled by the dedicated move endpoint - so a
 * body that tries to move a node has its coordinates ignored. The endpoints return the updated row.
 *
 * Unique refs per run keep the test isolated from the shared entity pool. Assumes the server runs
 * with "auth_check": false (see mock/auth.mjs) against a real Postgres (the mock-ars server):
 *
 *   npm run mock-ars                            # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-element-update.mjs     # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# PUT ${CANVAS_PATH}/:save_id/{node,edge}/:data_id  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:elt-A-${s}`;
  const refB = `API_TEST:elt-B-${s}`;
  const eAB = `${refA}->${refB}`;

  // Create a canvas with two nodes and one edge so we have a node and an edge to update.
  const createGraph = {
    nodes: {
      [refA]: signNode(refA, testNode(refA, 'Element A', 'biolink:Disease', 10, 20)),
      [refB]: signNode(refB, testNode(refB, 'Element B', 'biolink:ChemicalEntity', 30, 40, { hidden: true })),
    },
    edges: { [eAB]: signEdge(eAB, testEdge(refA, refB, 'biolink:treats')) },
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
  const create = await postCanvas({ label: `api-test element-update ${s}`, layout: 'horizontal', graph: createGraph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  const initial = await getJson(`${CANVAS_PATH}/${id}/graph`);
  const nodeDataId = new Map((initial.json.nodes || []).map((n) => [n.ref, n.data_id]));
  const edgeDataId = new Map((initial.json.edges || []).map((e) => [e.ref, e.data_id]));
  const nodeAId = nodeDataId.get(refA);
  const edgeABId = edgeDataId.get(eAB);
  ok(nodeAId != null && edgeABId != null, 'read back the node and edge data ids');

  const nodePath = `${CANVAS_PATH}/${id}/node/${nodeAId}`;
  const edgePath = `${CANVAS_PATH}/${id}/edge/${edgeABId}`;

  // --- Node ---

  // Update the label only; the response carries the new label and the untouched fields.
  const relabel = await putJson(nodePath, { label: 'Node A renamed' });
  ok(relabel.res.status === 200, `node relabel responds 200 (got ${relabel.res.status})`);
  ok(relabel.json && relabel.json.label === 'Node A renamed', 'node label is updated');
  ok(relabel.json && relabel.json.hidden === false, 'node hidden is unchanged by a label-only update');

  // Update hidden only.
  const hide = await putJson(nodePath, { hidden: true });
  ok(hide.res.status === 200, `node hide responds 200 (got ${hide.res.status})`);
  ok(hide.json && hide.json.hidden === true, 'node hidden is updated');
  ok(hide.json && hide.json.label === 'Node A renamed', 'node label is unchanged by a hidden-only update');

  // Position is NOT updatable here: x/y in the body are ignored, the label still applies.
  const tryMove = await putJson(nodePath, { label: 'Node A again', x: 999, y: 888 });
  ok(tryMove.res.status === 200, `node update with stray x/y responds 200 (got ${tryMove.res.status})`);
  ok(tryMove.json && tryMove.json.label === 'Node A again', 'node label still updates when x/y are present');
  ok(tryMove.json && tryMove.json.x === 10 && tryMove.json.y === 20,
    `node position is left untouched (got ${tryMove.json && tryMove.json.x},${tryMove.json && tryMove.json.y})`);

  // An empty body is a 400.
  const emptyNode = await putJson(nodePath, {});
  ok(emptyNode.res.status === 400, `empty node update -> 400 (got ${emptyNode.res.status})`);

  // A body that only sets a non-updatable field (no label/hidden) is also a 400.
  const onlyMove = await putJson(nodePath, { x: 1, y: 2 });
  ok(onlyMove.res.status === 400, `position-only node update -> 400 (got ${onlyMove.res.status})`);

  // Wrong-typed fields are 400s.
  const badLabel = await putJson(nodePath, { label: 42 });
  ok(badLabel.res.status === 400, `non-string node label -> 400 (got ${badLabel.res.status})`);
  const badHidden = await putJson(nodePath, { hidden: 'yes' });
  ok(badHidden.res.status === 400, `non-boolean node hidden -> 400 (got ${badHidden.res.status})`);

  // Unknown node id is a 404.
  const missingNode = await putJson(`${CANVAS_PATH}/${id}/node/999999999`, { label: 'nope' });
  ok(missingNode.res.status === 404, `unknown node id -> 404 (got ${missingNode.res.status})`);

  // Non-numeric ids are 400s.
  const badCanvasId = await putJson(`${CANVAS_PATH}/not-a-number/node/${nodeAId}`, { label: 'nope' });
  ok(badCanvasId.res.status === 400, `non-numeric canvas id -> 400 (got ${badCanvasId.res.status})`);
  const badNodeId = await putJson(`${CANVAS_PATH}/${id}/node/not-a-number`, { label: 'nope' });
  ok(badNodeId.res.status === 400, `non-numeric node id -> 400 (got ${badNodeId.res.status})`);

  // --- Edge ---

  const relabelEdge = await putJson(edgePath, { label: 'treats (renamed)' });
  ok(relabelEdge.res.status === 200, `edge relabel responds 200 (got ${relabelEdge.res.status})`);
  ok(relabelEdge.json && relabelEdge.json.label === 'treats (renamed)', 'edge label is updated');
  ok(relabelEdge.json && relabelEdge.json.hidden === false, 'edge hidden is unchanged by a label-only update');

  const hideEdge = await putJson(edgePath, { label: 'treats again', hidden: true });
  ok(hideEdge.res.status === 200, `edge update responds 200 (got ${hideEdge.res.status})`);
  ok(hideEdge.json && hideEdge.json.hidden === true && hideEdge.json.label === 'treats again',
    'edge label and hidden update together');

  const emptyEdge = await putJson(edgePath, {});
  ok(emptyEdge.res.status === 400, `empty edge update -> 400 (got ${emptyEdge.res.status})`);

  const badEdgeHidden = await putJson(edgePath, { hidden: 1 });
  ok(badEdgeHidden.res.status === 400, `non-boolean edge hidden -> 400 (got ${badEdgeHidden.res.status})`);

  const missingEdge = await putJson(`${CANVAS_PATH}/${id}/edge/999999999`, { label: 'nope' });
  ok(missingEdge.res.status === 404, `unknown edge id -> 404 (got ${missingEdge.res.status})`);

  // A soft-deleted element cannot be updated (the update is gated on time_deleted IS NULL).
  const trash = await putJson(`${CANVAS_PATH}/${id}/graph/trash`, { edges: [edgeABId] });
  ok(trash.res.status === 200, `trash edge responds 200 (got ${trash.res.status})`);
  const updateTrashed = await putJson(edgePath, { label: 'should not apply' });
  ok(updateTrashed.res.status === 404, `updating a soft-deleted edge -> 404 (got ${updateTrashed.res.status})`);

  // A trashed canvas takes its elements with it: updating a node (still active in its own right) or
  // edge on a soft-deleted canvas is a 404.
  const trashCanvas = await putJson(`${CANVAS_PATH}/trash`, [id]);
  ok(trashCanvas.res.status === 200, `trash canvas responds 200 (got ${trashCanvas.res.status})`);
  const nodeAfterCanvasTrash = await putJson(nodePath, { label: 'after canvas trash' });
  ok(nodeAfterCanvasTrash.res.status === 404, `updating a node on a trashed canvas -> 404 (got ${nodeAfterCanvasTrash.res.status})`);
  const edgeAfterCanvasTrash = await putJson(edgePath, { label: 'after canvas trash' });
  ok(edgeAfterCanvasTrash.res.status === 404, `updating an edge on a trashed canvas -> 404 (got ${edgeAfterCanvasTrash.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
