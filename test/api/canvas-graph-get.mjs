/* Standalone API test: GET /api/v1/users/me/canvas/:save_id/graph.
 *
 * The graph endpoint returns the Canvas's own nodes and edges (the canvas_node / canvas_edge rows)
 * plus the tag descriptions stored on the Canvas - but NOT the underlying data-pool entities. So the
 * returned nodes/edges carry placement and denormalized tag ids, never the signed SummaryNode /
 * SummaryEdge payload. This test creates a canvas with a tagged graph, reads it back, and asserts the
 * shape: nodes/edges round-trip with their tag id-sets, the tag descriptions resolve, the edge's
 * endpoints line up with the node data ids, and none of the data-pool fields leak through.
 *
 * Ownership is enforced by the endpoint, so a canvas the user does not own (or that does not exist)
 * reads as 404 and a non-numeric id is rejected as 400.
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-get.mjs        # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, graphWithNodesAndEdges, CANVAS_PATH,
  NODE_REF_1, NODE_REF_2, EDGE_REF_1,
  NODE_TAG_DRUG, NODE_TAG_FDA, EDGE_TAG_CLINICAL,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# GET ${CANVAS_PATH}/:save_id/graph  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas graph ${new Date().toISOString()}`;
  const layout = 'horizontal';

  const create = await postCanvas({ label, layout, graph: graphWithNodesAndEdges() });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');

  const graphRes = await getJson(`${CANVAS_PATH}/${canvas.id}/graph`);
  ok(graphRes.res.status === 200, `get graph responds 200 (got ${graphRes.res.status})`);
  const graph = graphRes.json;
  ok(graph && Array.isArray(graph.nodes), 'graph has a nodes array');
  ok(graph && Array.isArray(graph.edges), 'graph has an edges array');

  const nodes = (graph && graph.nodes) || [];
  const edges = (graph && graph.edges) || [];
  ok(nodes.length === 2, `graph returns both nodes (got ${nodes.length})`);
  ok(edges.length === 1, `graph returns the edge (got ${edges.length})`);

  const node1 = nodes.find((n) => n.ref === NODE_REF_1);
  const node2 = nodes.find((n) => n.ref === NODE_REF_2);
  const edge1 = edges.find((e) => e.ref === EDGE_REF_1);
  ok(node1, `node ${NODE_REF_1} is present`);
  ok(node2, `node ${NODE_REF_2} is present`);
  ok(edge1, `edge ${EDGE_REF_1} is present`);

  // Placement and display fields survive the round-trip.
  ok(node1 && node1.x === 10 && node1.y === 20, 'node 1 keeps its x/y placement');
  ok(node1 && typeof node1.label === 'string' && node1.label.length > 0, 'node 1 has a label');
  ok(node1 && node1.hidden === false, 'node 1 is not hidden');
  ok(node2 && node2.hidden === true, 'node 2 keeps its hidden flag');

  // Per-entity tags come back as a { tag_id: null } id-set, never the full descriptions.
  ok(node1 && node1.tags && node1.tags[NODE_TAG_DRUG] === null, 'node 1 carries the drug tag id (null-valued)');
  ok(node1 && node1.tags && node1.tags[NODE_TAG_FDA] === null, 'node 1 carries the FDA tag id (null-valued)');
  ok(node2 && node2.tags && Object.keys(node2.tags).length === 0, 'node 2 has no tags');
  ok(edge1 && edge1.tags && edge1.tags[EDGE_TAG_CLINICAL] === null, 'edge 1 carries the clinical tag id (null-valued)');

  // The descriptions for those ids resolve in the graph-level tags map.
  const tags = graph && graph.tags;
  ok(tags && tags[NODE_TAG_DRUG] && tags[NODE_TAG_DRUG].description.name === 'Drug', 'graph tags describe the drug tag');
  ok(tags && tags[NODE_TAG_FDA] && tags[NODE_TAG_FDA].description.name === 'FDA Approved', 'graph tags describe the FDA tag');
  ok(tags && tags[EDGE_TAG_CLINICAL] && tags[EDGE_TAG_CLINICAL].description.name === 'Clinical Evidence',
    'graph tags describe the clinical tag');

  // The edge's endpoints reference the node data ids within the same canvas.
  ok(edge1 && node1 && edge1.subject_id === node1.data_id, 'edge subject_id matches node 1 data_id');
  ok(edge1 && node2 && edge1.object_id === node2.data_id, 'edge object_id matches node 2 data_id');

  // No data-pool leakage: the canvas node/edge must not expose the signed SummaryNode/SummaryEdge data.
  ok(node1 && node1.data === undefined && node1.names === undefined && node1.curies === undefined,
    'node does not leak the data-pool entity payload');
  ok(edge1 && edge1.data === undefined && edge1.predicate === undefined && edge1.subject === undefined,
    'edge does not leak the data-pool entity payload');

  // A canvas that does not exist (or is not owned by this user) is a 404.
  const missing = await getJson(`${CANVAS_PATH}/999999999/graph`);
  ok(missing.res.status === 404, `unknown canvas id -> 404 (got ${missing.res.status})`);

  // A non-numeric id is a bad request.
  const badId = await getJson(`${CANVAS_PATH}/not-a-number/graph`);
  ok(badId.res.status === 400, `non-numeric canvas id -> 400 (got ${badId.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
