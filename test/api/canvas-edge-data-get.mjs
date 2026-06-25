/* Standalone API test: GET /api/v1/users/me/canvas/:save_id/edge/:data_id.
 *
 * The edge-data endpoint returns the underlying data-pool entity (the signed SummaryEdge) for a
 * Canvas Edge, by its data_id. Unlike the graph endpoint - which returns the canvas_edge row with a
 * { tag_id: null } id-set - this returns the full entity: its subject/object/predicate and the full
 * tag objects. The data pool is shared and deduplicated by ref, so the endpoint reads it directly by
 * id without scoping to the canvas; a missing id is a 404 and a non-numeric id a 400.
 *
 * This test mints unique refs per run so the pool row it reads back is exactly what it submitted -
 * the suite's stable-ref fixtures share pool rows whose newest-source_time version (or a legacy row
 * with no source_time at all) may differ from any single submission.
 *
 * The flow creates a canvas (two nodes + one edge), reads its graph to discover the edge's data_id,
 * then fetches the edge's data. Assumes the server is running with "auth_check": false (see
 * mock/auth.mjs). This hits a real Postgres, so run it against the mock-ars server (host=mock allows
 * the auth bypass):
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-edge-data-get.mjs    # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, testNode, signNode, testEdge, signEdge, tagObject, CANVAS_PATH,
  EDGE_TAG_CLINICAL,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# GET ${CANVAS_PATH}/:save_id/edge/:data_id  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas edge-data ${new Date().toISOString()}`;
  const layout = 'horizontal';

  const stamp = Date.now();
  const subjectRef = `API_TEST:edge-data-s-${stamp}`;
  const objectRef = `API_TEST:edge-data-o-${stamp}`;
  const edgeRef = `API_TEST:edge-data-${stamp}`;
  const graph = {
    nodes: {
      [subjectRef]: signNode(subjectRef, testNode(subjectRef, 'Edge Data Subject', 'biolink:Disease', 5, 6)),
      [objectRef]: signNode(objectRef, testNode(objectRef, 'Edge Data Object', 'biolink:ChemicalEntity', 7, 8)),
    },
    edges: {
      [edgeRef]: signEdge(edgeRef, testEdge(subjectRef, objectRef, 'biolink:treats', {
        description: 'edge data test',
        tags: { [EDGE_TAG_CLINICAL]: tagObject(EDGE_TAG_CLINICAL, 'Clinical Evidence') },
      })),
    },
    tag_descriptions: {
      [EDGE_TAG_CLINICAL]: tagObject(EDGE_TAG_CLINICAL, 'Clinical Evidence'),
    },
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };

  const create = await postCanvas({ label, layout, graph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');

  const graphRes = await getJson(`${CANVAS_PATH}/${canvas.id}/graph`);
  ok(graphRes.res.status === 200, `get graph responds 200 (got ${graphRes.res.status})`);
  const canvasEdge = (graphRes.json && graphRes.json.edges || []).find((e) => e.ref === edgeRef);
  ok(canvasEdge && Number.isInteger(canvasEdge.data_id), 'graph edge exposes an integer data_id');

  const dataRes = await getJson(`${CANVAS_PATH}/${canvas.id}/edge/${canvasEdge.data_id}`);
  ok(dataRes.res.status === 200, `get edge data responds 200 (got ${dataRes.res.status})`);
  const data = dataRes.json;

  // The response is the full data-pool entity, identified by its ref and connecting its endpoints.
  ok(data && data.id === edgeRef, 'edge data id matches the edge ref');
  ok(data && data.subject === subjectRef, 'edge data keeps its subject ref');
  ok(data && data.object === objectRef, 'edge data keeps its object ref');
  ok(data && data.predicate === 'biolink:treats', 'edge data keeps its predicate');

  // Tags here are the FULL objects (not the canvas_edge id-set): the data pool retains descriptions.
  ok(data && data.tags && data.tags[EDGE_TAG_CLINICAL] && data.tags[EDGE_TAG_CLINICAL].description.name === 'Clinical Evidence',
    'edge data carries the full clinical tag object');

  // A data_id that does not exist in the pool is a 404.
  const missing = await getJson(`${CANVAS_PATH}/${canvas.id}/edge/999999999`);
  ok(missing.res.status === 404, `unknown edge id -> 404 (got ${missing.res.status})`);

  // A non-numeric edge id is a bad request.
  const badId = await getJson(`${CANVAS_PATH}/${canvas.id}/edge/not-a-number`);
  ok(badId.res.status === 400, `non-numeric edge id -> 400 (got ${badId.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
