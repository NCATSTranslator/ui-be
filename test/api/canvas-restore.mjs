/* Standalone API test: PUT /api/v1/users/me/canvas/restore (un-delete canvases).
 *
 * Restore clears time_deleted on a list of the current user's trashed canvases by id (the inverse of
 * /trash, matching the queries/projects convention). It is best-effort and scoped to the user: ids
 * that are unknown, not owned, or not currently trashed are silently skipped, so the request succeeds
 * with 200. A restored canvas reappears in the default canvas list with a null time_deleted. A body
 * that is not an array of integer ids is a 400.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                      # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-restore.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, CANVAS_PATH } from '../lib/api-canvas.mjs';

const TRASH_PATH = `${CANVAS_PATH}/trash`;
const RESTORE_PATH = `${CANVAS_PATH}/restore`;
const { ok, fail, finish } = createHarness();

const isActive = (list, id) => Array.isArray(list) && list.some((c) => c.id === id);
const findCanvas = (list, id) => (Array.isArray(list) ? list.find((c) => c.id === id) : null);

console.log(`# PUT ${RESTORE_PATH}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const stamp = new Date().toISOString();
  const create = await postCanvas({ label: `api-test restore ${stamp}`, layout: 'horizontal' });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  // Trash it, then confirm it dropped out of the active list.
  const trash = await putJson(TRASH_PATH, [id]);
  ok(trash.res.status === 200, `trash responds 200 (got ${trash.res.status})`);
  const afterTrash = await getJson(CANVAS_PATH);
  ok(!isActive(afterTrash.json, id), 'canvas is gone from the active list after trash');

  // Restore brings it back into the active list with a cleared time_deleted.
  const restore = await putJson(RESTORE_PATH, [id]);
  ok(restore.res.status === 200, `restore responds 200 (got ${restore.res.status})`);
  const afterRestore = await getJson(CANVAS_PATH);
  ok(isActive(afterRestore.json, id), 'restored canvas reappears in the active list');
  const restored = findCanvas(afterRestore.json, id);
  ok(restored && (restored.time_deleted === null || restored.time_deleted === undefined),
    'restored canvas has a cleared time_deleted');

  // Restoring a canvas that is not currently trashed is a no-op success (best-effort).
  const reRestore = await putJson(RESTORE_PATH, [id]);
  ok(reRestore.res.status === 200, `restoring a non-trashed canvas still responds 200 (got ${reRestore.res.status})`);

  // An unknown id and an empty list are both no-op successes.
  const unknown = await putJson(RESTORE_PATH, [999999999]);
  ok(unknown.res.status === 200, `restore [unknown] responds 200 (got ${unknown.res.status})`);
  const empty = await putJson(RESTORE_PATH, []);
  ok(empty.res.status === 200, `empty restore list responds 200 (got ${empty.res.status})`);

  // A non-array body is a 400.
  const notArray = await putJson(RESTORE_PATH, { id });
  ok(notArray.res.status === 400, `non-array body -> 400 (got ${notArray.res.status})`);

  // A non-integer id is a 400.
  const notInt = await putJson(RESTORE_PATH, ['not-a-number']);
  ok(notInt.res.status === 400, `non-integer id -> 400 (got ${notInt.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
