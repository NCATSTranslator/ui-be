/* Standalone API test: GET /api/v1/users/me/canvas/:save_id/node/:data_id.
 *
 * The node-data endpoint returns the underlying data-pool entity (the signed SummaryNode) for a
 * Canvas Node, by its data_id. Unlike the graph endpoint - which returns the canvas_node row with a
 * { tag_id: null } id-set - this returns the full entity: its curies, names, annotations, and the
 * full tag objects. The data pool is shared and deduplicated by ref, so the endpoint reads it
 * directly by id without scoping to the canvas; a missing id is a 404 and a non-numeric id a 400.
 *
 * This test mints a unique ref per run so the pool row it reads back is exactly what it submitted -
 * the suite's stable-ref fixtures share pool rows whose newest-source_time version (or a legacy row
 * with no source_time at all) may differ from any single submission.
 *
 * The flow creates a canvas, reads its graph to discover the node's data_id, then fetches the node's
 * data. Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-node-data-get.mjs    # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, testNode, signNode, tagObject, CANVAS_PATH,
  NODE_TAG_DRUG, NODE_TAG_FDA,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# GET ${CANVAS_PATH}/:save_id/node/:data_id  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas node-data ${new Date().toISOString()}`;
  const layout = 'horizontal';

  const ref = `API_TEST:node-data-${Date.now()}`;
  const node = signNode(ref, testNode(ref, 'Node Data Test', 'biolink:Disease', 5, 6, {
    annotations: { disease: { mondo: ['MONDO:0005148'] } },
    tags: {
      [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug'),
      [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved'),
    },
  }));
  const graph = {
    nodes: { [ref]: node },
    edges: {},
    tag_descriptions: {
      [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug'),
      [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved'),
    },
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };

  const create = await postCanvas({ label, layout, graph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');

  const graphRes = await getJson(`${CANVAS_PATH}/${canvas.id}/graph`);
  ok(graphRes.res.status === 200, `get graph responds 200 (got ${graphRes.res.status})`);
  const canvasNode = (graphRes.json && graphRes.json.nodes || []).find((n) => n.ref === ref);
  ok(canvasNode && Number.isInteger(canvasNode.data_id), 'graph node exposes an integer data_id');

  const dataRes = await getJson(`${CANVAS_PATH}/${canvas.id}/node/${canvasNode.data_id}`);
  ok(dataRes.res.status === 200, `get node data responds 200 (got ${dataRes.res.status})`);
  const data = dataRes.json;

  // The response is the full data-pool entity, identified by its ref.
  ok(data && data.id === ref, 'node data id matches the node ref');
  ok(data && Array.isArray(data.curies) && data.curies.includes(ref), 'node data carries its curies');
  ok(data && Array.isArray(data.names) && data.names.length > 0, 'node data carries its names');
  ok(data && data.annotations && data.annotations.disease, 'node data carries its annotations');

  // Tags here are the FULL objects (not the canvas_node id-set): the data pool retains descriptions.
  ok(data && data.tags && data.tags[NODE_TAG_DRUG] && data.tags[NODE_TAG_DRUG].description.name === 'Drug',
    'node data carries the full drug tag object');
  ok(data && data.tags && data.tags[NODE_TAG_FDA] && data.tags[NODE_TAG_FDA].description.name === 'FDA Approved',
    'node data carries the full FDA tag object');

  // A data_id that does not exist in the pool is a 404.
  const missing = await getJson(`${CANVAS_PATH}/${canvas.id}/node/999999999`);
  ok(missing.res.status === 404, `unknown node id -> 404 (got ${missing.res.status})`);

  // A non-numeric node id is a bad request.
  const badId = await getJson(`${CANVAS_PATH}/${canvas.id}/node/not-a-number`);
  ok(badId.res.status === 400, `non-numeric node id -> 400 (got ${badId.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
