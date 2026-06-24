/* Standalone API test: POST /api/v1/users/me/canvas graph rejections.
 *
 * Confirms a canvas request whose graph fails signature or structural checks is rejected with 400
 * (rather than committing bad data or hitting a 500). Covers:
 *   - a node whose signed data was altered after signing (server re-derives a different hash)
 *   - a node with no signature at all
 *   - a node with non-numeric placement coordinates
 *   - an edge whose signed data was altered after signing
 *   - an edge with no signature at all
 *   - an edge whose endpoint is not among the submitted nodes (would otherwise hit a foreign-key
 *     violation -> 500; the edge is signed correctly so it specifically exercises the endpoint check)
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                               # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-validation.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, graphWithNodesAndEdges, signEdge, testEdge,
  CANVAS_PATH, NODE_REF_1, NODE_REF_2, EDGE_REF_1,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# POST ${CANVAS_PATH} graph rejections  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const layout = 'horizontal';
  const label = `api-test canvas graph rejection ${new Date().toISOString()}`;

  // A node whose data was altered after signing must be rejected (mutate a signed field).
  const tampered = graphWithNodesAndEdges();
  tampered.nodes[NODE_REF_1].names = ['Tampered Name'];
  const tamper = await postCanvas({ label: `${label} (tampered node)`, layout, graph: tampered });
  ok(tamper.res.status === 400, `node with tampered data is rejected with 400 (got ${tamper.res.status})`);

  // A node with no signature at all must also be rejected.
  const unsigned = graphWithNodesAndEdges();
  delete unsigned.nodes[NODE_REF_2].signature;
  const missing = await postCanvas({ label: `${label} (unsigned node)`, layout, graph: unsigned });
  ok(missing.res.status === 400, `node with no signature is rejected with 400 (got ${missing.res.status})`);

  // An edge whose data was altered after signing must be rejected (mutate a signed field).
  const tamperedEdge = graphWithNodesAndEdges();
  tamperedEdge.edges[EDGE_REF_1].predicate = 'biolink:tampered';
  const edgeTamper = await postCanvas({ label: `${label} (tampered edge)`, layout, graph: tamperedEdge });
  ok(edgeTamper.res.status === 400, `edge with tampered data is rejected with 400 (got ${edgeTamper.res.status})`);

  // An edge with no signature at all must also be rejected (parity with the unsigned-node case).
  const unsignedEdge = graphWithNodesAndEdges();
  delete unsignedEdge.edges[EDGE_REF_1].signature;
  const missingEdge = await postCanvas({ label: `${label} (unsigned edge)`, layout, graph: unsignedEdge });
  ok(missingEdge.res.status === 400, `edge with no signature is rejected with 400 (got ${missingEdge.res.status})`);

  // A node with non-numeric placement coordinates must be rejected. x/y are checked before the
  // signature and are not part of the signed data, so a validly-signed node still fails this check.
  const badCoords = graphWithNodesAndEdges();
  badCoords.nodes[NODE_REF_1].x = 'not-a-number';
  const badCoordsRes = await postCanvas({ label: `${label} (non-numeric x)`, layout, graph: badCoords });
  ok(badCoordsRes.res.status === 400, `node with non-numeric x/y is rejected with 400 (got ${badCoordsRes.res.status})`);

  // An edge whose endpoint is not among the submitted nodes must be rejected with a 400.
  const dangling = graphWithNodesAndEdges();
  dangling.edges[EDGE_REF_1] = signEdge(EDGE_REF_1, testEdge(NODE_REF_1, 'API_TEST:not-in-graph', 'biolink:treats'));
  const danglingRes = await postCanvas({ label: `${label} (dangling edge)`, layout, graph: dangling });
  ok(danglingRes.res.status === 400, `edge referencing a node not in the graph is rejected with 400 (got ${danglingRes.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
