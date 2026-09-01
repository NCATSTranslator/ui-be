/* Standalone API test: canvas.time_updated tracks changes to the canvas's child rows.
 *
 * Clients poll GET /api/v1/users/me/canvas and compare each canvas's time_updated against the
 * value they last loaded, refetching the graph when the two differ. That only works if writes to
 * a canvas's nodes, edges and annotations bump the parent canvas row, not just the child row --
 * otherwise adding, moving or deleting graph elements is invisible to a polling client.
 *
 * Every write path is exercised here: geometry, element display updates, annotation create/edit,
 * graph merge, trash and restore, plus the canvas metadata update that already bumped the row.
 * Writes that change nothing (a re-trash of an already-trashed annotation, a move of a data_id
 * that is not on the canvas) must leave the timestamp alone, so a no-op cannot spuriously tell
 * every other tab to refetch the graph.
 *
 * Assumes the server runs with "auth_check": false (see mock/auth.mjs) against a real Postgres:
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-sync-timestamp.mjs   # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, postJson, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# canvas.time_updated tracks child writes  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:sync-A-${s}`;
  const refB = `API_TEST:sync-B-${s}`;
  const refC = `API_TEST:sync-C-${s}`;
  const eAB = `${refA}->${refB}`;

  const createGraph = {
    nodes: {
      [refA]: signNode(refA, testNode(refA, 'Sync A', 'biolink:Disease', 10, 20)),
      [refB]: signNode(refB, testNode(refB, 'Sync B', 'biolink:ChemicalEntity', 30, 40)),
    },
    edges: { [eAB]: signEdge(eAB, testEdge(refA, refB, 'biolink:treats')) },
    tag_descriptions: {},
  };
  const create = await postCanvas({ label: `api-test sync-timestamp ${s}`, layout: 'custom', graph: createGraph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  const graphRes = await getJson(`${CANVAS_PATH}/${id}/graph`);
  const nodeDataId = new Map((graphRes.json.nodes || []).map((n) => [n.ref, n.data_id]));
  const edgeDataId = new Map((graphRes.json.edges || []).map((e) => [e.ref, e.data_id]));
  const nodeAId = nodeDataId.get(refA);
  const nodeBId = nodeDataId.get(refB);
  const edgeABId = edgeDataId.get(eAB);
  ok(nodeAId != null && nodeBId != null && edgeABId != null, 'read back the node and edge data ids');

  // The list endpoint is what a polling client actually reads, so assert against that.
  const listTimeUpdated = async () => {
    const list = await getJson(CANVAS_PATH);
    const row = (list.json || []).find((c) => c.id === id);
    return row ? row.time_updated : null;
  };

  let mark = await listTimeUpdated();
  ok(mark != null, 'the canvas appears in the list with a time_updated');

  // Runs a write and asserts the parent canvas timestamp moved forward because of it.
  async function bumps(label, fn) {
    const before = mark;
    const result = await fn();
    const after = await listTimeUpdated();
    ok(new Date(after).getTime() > new Date(before).getTime(),
      `${label} bumps canvas.time_updated (${before} -> ${after})`);
    mark = after;
    return result;
  }

  // Runs a write that changes no rows and asserts the parent canvas timestamp stood still.
  async function leavesAlone(label, fn) {
    const before = mark;
    const result = await fn();
    const after = await listTimeUpdated();
    ok(new Date(after).getTime() === new Date(before).getTime(),
      `${label} leaves canvas.time_updated alone (${before} -> ${after})`);
    mark = after;
    return result;
  }

  const geometryPath = `${CANVAS_PATH}/${id}/graph/geometry`;
  const annotationPath = `${CANVAS_PATH}/${id}/annotation`;
  const trashPath = `${CANVAS_PATH}/${id}/graph/trash`;
  const restorePath = `${CANVAS_PATH}/${id}/graph/restore`;

  await bumps('moving a node', async () => {
    const r = await putJson(geometryPath, { nodes: [{ data_id: nodeAId, x: 111, y: 222 }] });
    ok(r.res.status === 200, `  move responds 200 (got ${r.res.status})`);
  });

  await bumps('relabelling a node', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}/node/${nodeAId}`, { label: 'Sync A renamed' });
    ok(r.res.status === 200, `  node update responds 200 (got ${r.res.status})`);
  });

  await bumps('hiding an edge', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}/edge/${edgeABId}`, { hidden: true });
    ok(r.res.status === 200, `  edge update responds 200 (got ${r.res.status})`);
  });

  const noteId = await bumps('creating an annotation', async () => {
    const r = await postJson(annotationPath, { content: 'sync note', x: 10, y: 20, width: 100, height: 40 });
    ok(r.res.status === 200, `  annotation create responds 200 (got ${r.res.status})`);
    return r.json && r.json.id;
  });
  ok(noteId != null, 'created annotation has an id');

  await bumps('editing annotation content', async () => {
    const r = await putJson(`${annotationPath}/${noteId}`, { content: 'sync note edited' });
    ok(r.res.status === 200, `  annotation edit responds 200 (got ${r.res.status})`);
  });

  await bumps('moving an annotation', async () => {
    const r = await putJson(geometryPath, { annotations: [{ id: noteId, x: 55, y: 66 }] });
    ok(r.res.status === 200, `  annotation move responds 200 (got ${r.res.status})`);
  });

  await bumps('merging a new node into the graph', async () => {
    const r = await postJson(`${CANVAS_PATH}/${id}/graph`, {
      nodes: { [refC]: signNode(refC, testNode(refC, 'Sync C', 'biolink:Gene', 50, 60)) },
      edges: {},
    });
    ok(r.res.status === 200, `  merge responds 200 (got ${r.res.status})`);
  });

  await leavesAlone('re-merging a node that is already on the canvas', async () => {
    const r = await postJson(`${CANVAS_PATH}/${id}/graph`, {
      nodes: { [refC]: signNode(refC, testNode(refC, 'Sync C', 'biolink:Gene', 50, 60)) },
      edges: {},
    });
    ok(r.res.status === 200, `  repeat merge responds 200 (got ${r.res.status})`);
  });

  await bumps('trashing a node', async () => {
    const r = await putJson(trashPath, { nodes: [nodeBId] });
    ok(r.res.status === 200, `  trash responds 200 (got ${r.res.status})`);
  });

  await bumps('restoring a node', async () => {
    const r = await putJson(restorePath, { nodes: [nodeBId] });
    ok(r.res.status === 200, `  restore responds 200 (got ${r.res.status})`);
  });

  // The metadata path already bumped the row before child writes did; guard it against regression.
  await bumps('renaming the canvas', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}`, { label: `api-test sync-timestamp ${s} renamed` });
    ok(r.res.status === 200, `  rename responds 200 (got ${r.res.status})`);
  });

  // No-ops must not wake up every other tab.
  await leavesAlone('relabelling a node with its current label', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}/node/${nodeAId}`, { label: 'Sync A renamed' });
    ok(r.res.status === 200, `  idempotent node relabel responds 200 (got ${r.res.status})`);
  });

  await leavesAlone('hiding an edge that is already hidden', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}/edge/${edgeABId}`, { hidden: true });
    ok(r.res.status === 200, `  idempotent edge hide responds 200 (got ${r.res.status})`);
  });

  await leavesAlone('editing annotation content to its current text', async () => {
    const r = await putJson(`${annotationPath}/${noteId}`, { content: 'sync note edited' });
    ok(r.res.status === 200, `  idempotent annotation edit responds 200 (got ${r.res.status})`);
  });

  await leavesAlone('moving a data_id that is not on the canvas', async () => {
    const r = await putJson(geometryPath, { nodes: [{ data_id: 999999999, x: 1, y: 2 }] });
    ok(r.res.status === 200, `  no-op move responds 200 (got ${r.res.status})`);
  });

  await putJson(trashPath, { annotations: [noteId] });
  mark = await listTimeUpdated();
  await leavesAlone('re-trashing an already trashed annotation', async () => {
    const r = await putJson(trashPath, { annotations: [noteId] });
    ok(r.res.status === 200, `  repeat trash responds 200 (got ${r.res.status})`);
  });

  await leavesAlone('a failed update against a missing node', async () => {
    const r = await putJson(`${CANVAS_PATH}/${id}/node/999999999`, { label: 'nope' });
    ok(r.res.status === 404, `  missing node responds 404 (got ${r.res.status})`);
  });

  // Clean up after ourselves so repeated runs do not pile up canvases.
  const trashCanvas = await putJson(`${CANVAS_PATH}/trash`, [id]);
  ok(trashCanvas.res.status === 200, `trashing the test canvas responds 200 (got ${trashCanvas.res.status})`);
} catch (err) {
  fail(`unexpected error: ${err && err.stack ? err.stack : err}`);
}

finish();
