/* Standalone API test: PUT /api/v1/users/me/canvas/trash (soft delete canvases).
 *
 * Trash soft-deletes a list of the current user's canvases by id (matching the queries/projects
 * /trash convention). It is best-effort and scoped to the user: ids that are unknown, not owned, or
 * already trashed are silently skipped, so the request succeeds with 200. A trashed canvas drops out
 * of the default canvas list but is still returned with include_deleted=true (with time_deleted set).
 * A body that is not an array of integer ids is a 400.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                      # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-trash.mjs        # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, CANVAS_PATH } from '../lib/api-canvas.mjs';

const TRASH_PATH = `${CANVAS_PATH}/trash`;
const { ok, fail, finish } = createHarness();

const isActive = (list, id) => Array.isArray(list) && list.some((c) => c.id === id);
const findCanvas = (list, id) => (Array.isArray(list) ? list.find((c) => c.id === id) : null);

console.log(`# PUT ${TRASH_PATH}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const stamp = new Date().toISOString();
  const a = await postCanvas({ label: `api-test trash A ${stamp}`, layout: 'horizontal' });
  const b = await postCanvas({ label: `api-test trash B ${stamp}`, layout: 'horizontal' });
  ok(a.res.status === 200 && b.res.status === 200, 'created two canvases');
  const idA = a.json && a.json.id;
  const idB = b.json && b.json.id;
  ok(idA != null && idB != null && idA !== idB, 'both canvases have distinct ids');

  // Trash a single canvas (the array-of-one case).
  const trashA = await putJson(TRASH_PATH, [idA]);
  ok(trashA.res.status === 200, `trash [A] responds 200 (got ${trashA.res.status})`);

  // A trashed canvas drops out of the default list but B (untouched) remains.
  const active = await getJson(CANVAS_PATH);
  ok(active.res.status === 200, `list responds 200 (got ${active.res.status})`);
  ok(!isActive(active.json, idA), 'trashed canvas A is gone from the active list');
  ok(isActive(active.json, idB), 'untouched canvas B is still in the active list');

  // include_deleted surfaces A again, now carrying a time_deleted timestamp.
  const withDeleted = await getJson(`${CANVAS_PATH}?include_deleted=true`);
  ok(withDeleted.res.status === 200, `list?include_deleted=true responds 200 (got ${withDeleted.res.status})`);
  const deletedA = findCanvas(withDeleted.json, idA);
  ok(deletedA, 'trashed canvas A appears with include_deleted=true');
  ok(deletedA && deletedA.time_deleted, 'trashed canvas A has a time_deleted timestamp');

  // Re-trashing an already-trashed canvas is a no-op success (best-effort).
  const reTrashA = await putJson(TRASH_PATH, [idA]);
  ok(reTrashA.res.status === 200, `re-trash [A] still responds 200 (got ${reTrashA.res.status})`);

  // Trash multiple at once, including an unknown id (silently skipped).
  const trashRest = await putJson(TRASH_PATH, [idB, 999999999]);
  ok(trashRest.res.status === 200, `trash [B, unknown] responds 200 (got ${trashRest.res.status})`);
  const activeAfter = await getJson(CANVAS_PATH);
  ok(!isActive(activeAfter.json, idB), 'canvas B is gone from the active list after the batch trash');

  // An empty list is a no-op success.
  const empty = await putJson(TRASH_PATH, []);
  ok(empty.res.status === 200, `empty trash list responds 200 (got ${empty.res.status})`);

  // A non-array body is a 400.
  const notArray = await putJson(TRASH_PATH, { id: idA });
  ok(notArray.res.status === 400, `non-array body -> 400 (got ${notArray.res.status})`);

  // A non-integer id is a 400.
  const notInt = await putJson(TRASH_PATH, ['not-a-number']);
  ok(notInt.res.status === 400, `non-integer id -> 400 (got ${notInt.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
