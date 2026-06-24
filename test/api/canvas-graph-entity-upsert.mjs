/* Standalone API test: shared node/edge entity-pool upsert across canvases.
 *
 * Node and edge data is shared across canvases and keyed by ref, so re-submitting the same refs
 * upserts the shared rows (ON CONFLICT) rather than colliding. This test drives that lifecycle in
 * order against the same refs:
 *   1. baseline  -- seed the shared node/edge rows
 *   2. dedup     -- a second canvas reusing the same refs with identical data (ON CONFLICT no-op; a
 *                   plain insert would fail here with a unique violation)
 *   3. update    -- the same refs with changed data AND a strictly newer source_time (the update
 *                   branch: data IS DISTINCT FROM stored AND incoming source_time is newer)
 *   4. stale     -- the same refs with an older source_time (the skip-not-overwrite branch)
 *
 * These cases are intentionally sequential and share the same node and edge refs: they must run in
 * this order. Asserting the stored data actually changed (or was preserved) needs the not-yet-built
 * GET-graph endpoint; for now each path asserts the request succeeds.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                                  # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-entity-upsert.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, graphWithNodesAndEdges, CANVAS_PATH, NEWER_SOURCE_TIME, STALE_SOURCE_TIME } from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# shared node/edge entity-pool upsert  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas upsert ${new Date().toISOString()}`;
  const layout = 'horizontal';

  // Baseline: seed the shared node/edge rows for the stable refs.
  const base = await postCanvas({ label: `${label} (base)`, layout, graph: graphWithNodesAndEdges() });
  ok(base.res.status === 200, `baseline create responds 200 (got ${base.res.status})`);
  const baseId = base.json && base.json.id;

  // Dedup: a second canvas reusing the SAME refs with identical data must succeed via ON CONFLICT.
  const dedup = await postCanvas({ label: `${label} (dedup)`, layout, graph: graphWithNodesAndEdges() });
  ok(dedup.res.status === 200, `second canvas reusing the same refs responds 200 (got ${dedup.res.status})`);
  ok(dedup.json && dedup.json.id !== undefined && dedup.json.id !== baseId,
    'second canvas is a distinct canvas sharing the same node and edge data');

  // Update: reuse the SAME refs but with CHANGED data and a strictly newer source_time, exercising
  // the upsert's update branch (data IS DISTINCT FROM stored AND incoming source_time is newer).
  const updated = await postCanvas({ label: `${label} (updated data)`, layout, graph: graphWithNodesAndEdges('UPDATED', NEWER_SOURCE_TIME) });
  ok(updated.res.status === 200, `canvas with updated node/edge data responds 200 (got ${updated.res.status})`);
  ok(updated.json && updated.json.id !== undefined && updated.json.id !== null,
    'canvas with updated node/edge data has an id');

  // Stale: reuse the SAME refs with an OLDER source_time, exercising the skip-not-overwrite branch.
  // The skip behavior itself is unverifiable until the GET-graph endpoint exists; for now assert 200.
  const stale = await postCanvas({ label: `${label} (stale data)`, layout, graph: graphWithNodesAndEdges('STALE', STALE_SOURCE_TIME) });
  ok(stale.res.status === 200, `canvas with an older source_time still creates successfully (got ${stale.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
