/* Standalone API test: PUT /api/v1/users/me/canvas/:save_id (update canvas properties).
 *
 * The update endpoint applies a partial change to a canvas's own properties (label, layout). Only
 * the fields present in the body are changed; the rest are left untouched. The update is scoped to a
 * canvas the current user owns, so a canvas that does not exist (or is not owned) is a 404, a body
 * with no updatable fields or an invalid layout is a 400, and a non-numeric id is a 400.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                      # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-update.mjs       # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, CANVAS_PATH } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# PUT ${CANVAS_PATH}/:save_id  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const stamp = new Date().toISOString();
  const create = await postCanvas({ label: `api-test canvas update ${stamp}`, layout: 'horizontal' });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');

  // Partial update of the label only: the layout is left untouched.
  const newLabel = `renamed ${stamp}`;
  const labelUpdate = await putJson(`${CANVAS_PATH}/${canvas.id}`, { label: newLabel });
  ok(labelUpdate.res.status === 200, `label update responds 200 (got ${labelUpdate.res.status})`);
  ok(labelUpdate.json && labelUpdate.json.label === newLabel, 'response reflects the new label');
  ok(labelUpdate.json && labelUpdate.json.layout === 'horizontal', 'layout is unchanged by a label-only update');

  // Partial update of the layout only: the label is left untouched.
  const layoutUpdate = await putJson(`${CANVAS_PATH}/${canvas.id}`, { layout: 'concentric' });
  ok(layoutUpdate.res.status === 200, `layout update responds 200 (got ${layoutUpdate.res.status})`);
  ok(layoutUpdate.json && layoutUpdate.json.layout === 'concentric', 'response reflects the new layout');
  ok(layoutUpdate.json && layoutUpdate.json.label === newLabel, 'label is unchanged by a layout-only update');

  // Both at once.
  const bothUpdate = await putJson(`${CANVAS_PATH}/${canvas.id}`, { label: `both ${stamp}`, layout: 'vertical' });
  ok(bothUpdate.res.status === 200, `combined update responds 200 (got ${bothUpdate.res.status})`);
  ok(bothUpdate.json && bothUpdate.json.label === `both ${stamp}` && bothUpdate.json.layout === 'vertical',
    'response reflects both updated fields');

  // The changes persist: the canvas list shows the latest label and layout.
  const list = await getJson(CANVAS_PATH);
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  const listed = Array.isArray(list.json) ? list.json.find((c) => c.id === canvas.id) : null;
  ok(listed && listed.label === `both ${stamp}` && listed.layout === 'vertical', 'updates persisted to the canvas');

  // A body with no updatable fields is a 400.
  const empty = await putJson(`${CANVAS_PATH}/${canvas.id}`, {});
  ok(empty.res.status === 400, `empty update body -> 400 (got ${empty.res.status})`);

  // An invalid layout is a 400.
  const badLayout = await putJson(`${CANVAS_PATH}/${canvas.id}`, { layout: 'diagonal' });
  ok(badLayout.res.status === 400, `invalid layout -> 400 (got ${badLayout.res.status})`);

  // A non-numeric id is a 400.
  const badId = await putJson(`${CANVAS_PATH}/not-a-number`, { label: 'x' });
  ok(badId.res.status === 400, `non-numeric canvas id -> 400 (got ${badId.res.status})`);

  // A canvas that does not exist (or is not owned) is a 404.
  const missing = await putJson(`${CANVAS_PATH}/999999999`, { label: 'x' });
  ok(missing.res.status === 404, `unknown canvas id -> 404 (got ${missing.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
