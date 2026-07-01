/* Standalone API test: shared node/edge entity-pool upsert across canvases (with outcome checks).
 *
 * Node and edge data is shared across canvases and keyed by ref, so re-submitting the same refs
 * upserts the shared rows (ON CONFLICT) rather than colliding. This drives that lifecycle in order
 * against the same refs and verifies the STORED data by reading it back through GET .../node/:data_id
 * after each step:
 *   1. baseline  -- seed the shared node/edge rows
 *   2. dedup     -- reuse the refs with identical data (ON CONFLICT no-op; stored data unchanged)
 *   3. update    -- reuse the refs with changed data AND a strictly newer source_time (the overwrite
 *                   branch: data IS DISTINCT FROM stored AND incoming source_time is newer)
 *   4. stale     -- reuse the refs with changed data but an OLDER source_time (the skip branch; the
 *                   stored data is preserved)
 *
 * These cases are sequential and share the same refs; they must run in this order. Unique refs per
 * run keep the shared-pool rows isolated from the rest of the suite so the read-backs are exact.
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

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH,
  SOURCE_TIME, NEWER_SOURCE_TIME, STALE_SOURCE_TIME,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# shared node/edge entity-pool upsert  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const nodeRef = `API_TEST:upsert-node-${s}`;
  const node2Ref = `API_TEST:upsert-node2-${s}`;
  const edgeRef = `API_TEST:upsert-edge-${s}`;
  const layout = 'horizontal';

  // The same two nodes + edge every step; only the primary node's name (part of its signed data) and
  // the source_time vary - which is exactly what the upsert's overwrite condition keys on. An edge is
  // present so the edge-pool upsert (the identical _batch_create_entity path) also runs each step.
  const stepGraph = (nodeName, sourceTime) => ({
    nodes: {
      [nodeRef]: signNode(nodeRef, testNode(nodeRef, nodeName, 'biolink:Disease', 10, 20, { source_time: sourceTime })),
      [node2Ref]: signNode(node2Ref, testNode(node2Ref, 'Upsert Node Two', 'biolink:ChemicalEntity', 30, 40, { source_time: sourceTime })),
    },
    edges: {
      [edgeRef]: signEdge(edgeRef, testEdge(nodeRef, node2Ref, 'biolink:treats', { source_time: sourceTime })),
    },
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  });

  // Read the primary node's stored data-pool entity through a canvas that carries it.
  const readNodeNames = async (canvasId, dataId) => {
    const res = await getJson(`${CANVAS_PATH}/${canvasId}/node/${dataId}`);
    return { status: res.res.status, names: (res.json && res.json.names) || [] };
  };

  // 1. Baseline: seed the shared rows, then discover the (stable) pooled node data_id.
  const base = await postCanvas({ label: `api-test upsert base ${s}`, layout, graph: stepGraph('Upsert Base', SOURCE_TIME) });
  ok(base.res.status === 200, `baseline create responds 200 (got ${base.res.status})`);
  const baseId = base.json && base.json.id;
  const baseGraph = await getJson(`${CANVAS_PATH}/${baseId}/graph`);
  const dataId = (baseGraph.json.nodes || []).find((n) => n.ref === nodeRef)?.data_id;
  ok(Number.isInteger(dataId), 'baseline graph exposes the pooled node data_id');
  const read0 = await readNodeNames(baseId, dataId);
  ok(read0.status === 200 && read0.names.includes('Upsert Base'),
    `baseline stored the node data (got ${JSON.stringify(read0.names)})`);

  // 2. Dedup: a second canvas with IDENTICAL data -> ON CONFLICT no-op; stored data unchanged.
  const dedup = await postCanvas({ label: `api-test upsert dedup ${s}`, layout, graph: stepGraph('Upsert Base', SOURCE_TIME) });
  ok(dedup.res.status === 200, `dedup create responds 200 (got ${dedup.res.status})`);
  ok(dedup.json && dedup.json.id !== baseId, 'dedup is a distinct canvas sharing the same pooled data');
  const read1 = await readNodeNames(dedup.json.id, dataId);
  ok(read1.names.includes('Upsert Base'), 'dedup left the stored data unchanged');

  // 3. Update: CHANGED data + strictly NEWER source_time -> overwrite.
  const upd = await postCanvas({ label: `api-test upsert update ${s}`, layout, graph: stepGraph('Upsert Updated', NEWER_SOURCE_TIME) });
  ok(upd.res.status === 200, `update create responds 200 (got ${upd.res.status})`);
  const read2 = await readNodeNames(upd.json.id, dataId);
  ok(read2.names.includes('Upsert Updated'), 'a newer source_time overwrote the stored data');
  ok(!read2.names.includes('Upsert Base'), 'the old data was replaced');

  // 4. Stale: CHANGED data + OLDER source_time -> skip; stored data preserved.
  const stale = await postCanvas({ label: `api-test upsert stale ${s}`, layout, graph: stepGraph('Upsert Stale', STALE_SOURCE_TIME) });
  ok(stale.res.status === 200, `stale create responds 200 (got ${stale.res.status})`);
  const read3 = await readNodeNames(stale.json.id, dataId);
  ok(read3.names.includes('Upsert Updated'), 'an older source_time was skipped (data preserved)');
  ok(!read3.names.includes('Upsert Stale'), 'the stale data was not written');
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
