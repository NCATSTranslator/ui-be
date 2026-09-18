/* Standalone API test: the Canvas Annotation endpoints.
 *
 *   POST /api/v1/users/me/canvas/:save_id/annotation
 *   PUT  /api/v1/users/me/canvas/:save_id/annotation/:annotation_id
 *
 * Trash and restore are not annotation-specific endpoints: annotations ride along in the
 * GraphSelection that /graph/trash and /graph/restore already accept, so a mixed selection of
 * nodes, edges and annotations goes down in one atomic call. Those paths are exercised here for
 * the annotation collection.
 *
 * An annotation is a free-text box on a canvas. Unlike nodes and edges it has no signed data
 * entity behind it, so it is created directly rather than merged in as part of a graph. Content is
 * edited here; position and size move through the graph geometry endpoint, which this test also
 * covers for annotations (the node half lives in canvas-graph-geometry.mjs).
 *
 * Assumes the server runs with "auth_check": false (see mock/auth.mjs) against a real Postgres:
 *
 *   npm run mock-ars                      # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-annotation.mjs   # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, postJson, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# Canvas Annotations  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const create = await postCanvas({ label: `api-test annotation ${s}`, layout: 'custom' });
  ok(create.res.status === 200, `create canvas responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  const annotationPath = `${CANVAS_PATH}/${id}/annotation`;
  const graphPath = `${CANVAS_PATH}/${id}/graph`;
  const geometryPath = `${graphPath}/geometry`;
  const trashPath = `${graphPath}/trash`;
  const restorePath = `${graphPath}/restore`;

  // --- Create ---

  const made = await postJson(annotationPath, { content: 'first note', x: 10, y: 20, width: 100, height: 40 });
  ok(made.res.status === 200, `create annotation responds 200 (got ${made.res.status})`);
  const noteId = made.json && made.json.id;
  ok(noteId != null, 'created annotation has an id');
  ok(made.json && made.json.content === 'first note', 'the created annotation carries the submitted content');
  ok(made.json && made.json.x === 10 && made.json.y === 20 && made.json.width === 100 && made.json.height === 40,
    'the created annotation carries the submitted rectangle');
  ok(made.json && made.json.canvas_id === id, 'the created annotation belongs to the canvas');
  ok(made.json && made.json.time_deleted === null, 'a new annotation is not trashed');

  const graph = await getJson(graphPath);
  ok(graph.res.status === 200, `graph read responds 200 (got ${graph.res.status})`);
  ok(Array.isArray(graph.json && graph.json.annotations) && graph.json.annotations.length === 1,
    'the annotation shows up on the canvas graph');

  // --- Content update ---

  const edited = await putJson(`${annotationPath}/${noteId}`, { content: 'edited note' });
  ok(edited.res.status === 200, `content update responds 200 (got ${edited.res.status})`);
  ok(edited.json && edited.json.content === 'edited note', 'the content update took effect');
  ok(edited.json && edited.json.x === 10 && edited.json.width === 100,
    'a content update leaves the rectangle untouched');

  // --- Geometry: a move must not disturb the size ---

  const moved = await putJson(geometryPath, { annotations: [{ id: noteId, x: 55, y: 66 }] });
  ok(moved.res.status === 200, `annotation move responds 200 (got ${moved.res.status})`);
  const movedNote = moved.json && moved.json.annotations && moved.json.annotations[0];
  ok(movedNote && movedNote.x === 55 && movedNote.y === 66, 'the annotation moved to its new position');
  ok(movedNote && movedNote.width === 100 && movedNote.height === 40,
    'omitting size on a move leaves the stored size unchanged');
  ok(Array.isArray(moved.json && moved.json.nodes) && moved.json.nodes.length === 0,
    'an annotation-only geometry request returns an empty nodes array');

  const resized = await putJson(geometryPath, { annotations: [{ id: noteId, x: 55, y: 66, width: 7, height: 8 }] });
  ok(resized.res.status === 200, `annotation resize responds 200 (got ${resized.res.status})`);
  const resizedNote = resized.json && resized.json.annotations && resized.json.annotations[0];
  ok(resizedNote && resizedNote.width === 7 && resizedNote.height === 8, 'the annotation resized');

  // --- Trash and restore ---

  const trashed = await putJson(trashPath, { annotations: [noteId] });
  ok(trashed.res.status === 200, `trash responds 200 (got ${trashed.res.status})`);
  ok(Array.isArray(trashed.json && trashed.json.annotations) && trashed.json.annotations.length === 0,
    'trash returns the resulting graph, with the annotation gone from it');

  const afterTrash = await getJson(graphPath);
  ok((afterTrash.json.annotations || []).length === 0, 'a trashed annotation is off the graph');
  const withDeleted = await getJson(`${graphPath}?include_deleted=true`);
  ok((withDeleted.json.annotations || []).length === 1, 'include_deleted brings the trashed annotation back');

  const trashAgain = await putJson(trashPath, { annotations: [noteId] });
  ok(trashAgain.res.status === 200, `trashing an already-trashed annotation responds 200 (got ${trashAgain.res.status})`);

  const movedWhileTrashed = await putJson(geometryPath, { annotations: [{ id: noteId, x: 1, y: 1 }] });
  ok(movedWhileTrashed.res.status === 200, `moving a trashed annotation responds 200 (got ${movedWhileTrashed.res.status})`);
  ok((movedWhileTrashed.json.annotations || []).length === 0, 'a trashed annotation is not moved');

  const editWhileTrashed = await putJson(`${annotationPath}/${noteId}`, { content: 'nope' });
  ok(editWhileTrashed.res.status === 404, `editing a trashed annotation -> 404 (got ${editWhileTrashed.res.status})`);

  const restored = await putJson(restorePath, { annotations: [noteId] });
  ok(restored.res.status === 200, `restore responds 200 (got ${restored.res.status})`);
  ok((restored.json.annotations || []).length === 1, 'restore returns the graph with the annotation back on it');
  const afterRestore = await getJson(graphPath);
  ok((afterRestore.json.annotations || []).length === 1, 'a restored annotation is back on the graph');

  // --- Validation ---

  const noContent = await postJson(annotationPath, { x: 1, y: 2, width: 3, height: 4 });
  ok(noContent.res.status === 400, `create without content -> 400 (got ${noContent.res.status})`);

  const noX = await postJson(annotationPath, { content: 'x', y: 2, width: 3, height: 4 });
  ok(noX.res.status === 400, `create without x -> 400 (got ${noX.res.status})`);

  const nullWidth = await postJson(annotationPath, { content: 'x', x: 1, y: 2, width: null, height: 4 });
  ok(nullWidth.res.status === 400, `create with null width -> 400 (got ${nullWidth.res.status})`);

  const negativeWidth = await postJson(annotationPath, { content: 'x', x: 1, y: 2, width: -1, height: 4 });
  ok(negativeWidth.res.status === 400, `create with negative width -> 400 (got ${negativeWidth.res.status})`);

  const emptyUpdate = await putJson(`${annotationPath}/${noteId}`, {});
  ok(emptyUpdate.res.status === 400, `content update without content -> 400 (got ${emptyUpdate.res.status})`);

  const badTrashBody = await putJson(trashPath, { annotations: noteId });
  ok(badTrashBody.res.status === 400, `trash with a non-array annotations field -> 400 (got ${badTrashBody.res.status})`);

  const emptySelection = await putJson(trashPath, { nodes: [], edges: [], annotations: [] });
  ok(emptySelection.res.status === 400, `trash with an empty selection -> 400 (got ${emptySelection.res.status})`);

  const unknownNote = await putJson(`${annotationPath}/999999999`, { content: 'x' });
  ok(unknownNote.res.status === 404, `updating an unknown annotation -> 404 (got ${unknownNote.res.status})`);

  const badNoteId = await putJson(`${annotationPath}/not-a-number`, { content: 'x' });
  ok(badNoteId.res.status === 400, `non-numeric annotation id -> 400 (got ${badNoteId.res.status})`);

  const unknownCanvas = await postJson(`${CANVAS_PATH}/999999999/annotation`,
    { content: 'x', x: 1, y: 2, width: 3, height: 4 });
  ok(unknownCanvas.res.status === 404, `create on an unknown canvas -> 404 (got ${unknownCanvas.res.status})`);

  // A trashed canvas is gone: creating on it is a 404.
  const trashCanvas = await putJson(`${CANVAS_PATH}/trash`, [id]);
  ok(trashCanvas.res.status === 200, `trash canvas responds 200 (got ${trashCanvas.res.status})`);
  const createOnTrashed = await postJson(annotationPath, { content: 'x', x: 1, y: 2, width: 3, height: 4 });
  ok(createOnTrashed.res.status === 404, `create on a trashed canvas -> 404 (got ${createOnTrashed.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
