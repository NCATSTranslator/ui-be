/* Standalone API test: POST /api/v1/users/me/canvas tag handling.
 *
 * A canvas request carries tags at two levels: the graph's tag_descriptions (the full tag objects,
 * stored once on canvas.data.tags) and each node/edge's own tags (carried inside the signed entity
 * data). On create the server denormalizes each entity's tag ids onto canvas_node.tags /
 * canvas_edge.tags as a { tag_id: null } set, while the descriptions live at the canvas level.
 *
 * The canvas-level descriptions round-trip in the create response (canvas.data.tags), so those are
 * asserted directly. The per-node/edge tag ids are not yet readable (no GET-graph endpoint), so
 * their persistence is verified indirectly: the canvas_node/canvas_edge inserts run in the same
 * transaction as the canvas, so a 200 means they committed (a failure would roll back -> 500).
 *
 * Assumes the server is running with "auth_check": false (see mock/auth.mjs). This hits a real
 * Postgres, so run it against the mock-ars server (host=mock allows the auth bypass):
 *
 *   npm run mock-ars                              # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-create-with-tags.mjs     # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, graphWithNodesAndEdges, tagObject, CANVAS_PATH,
  NODE_TAG_DRUG, NODE_TAG_FDA, EDGE_TAG_CLINICAL,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

console.log(`# POST ${CANVAS_PATH} tag handling  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const label = `api-test canvas+tags ${new Date().toISOString()}`;
  const layout = 'horizontal';

  // A graph whose nodes and edges carry tags must persist (the tagged canvas_node/canvas_edge inserts
  // commit in the same transaction as the canvas).
  const create = await postCanvas({ label, layout, graph: graphWithNodesAndEdges() });
  ok(create.res.status === 200, `create-with-tags responds 200 (got ${create.res.status})`);
  const canvas = create.json;
  ok(canvas && canvas.id !== undefined && canvas.id !== null, 'created canvas has an id');

  // The graph's tag_descriptions are stored once on canvas.data.tags and round-trip in the response.
  const tags = canvas && canvas.data && canvas.data.tags;
  ok(tags && typeof tags === 'object', 'canvas data carries the tag descriptions');
  ok(tags && tags[NODE_TAG_DRUG] && tags[NODE_TAG_DRUG].description.name === 'Drug',
    'canvas data describes the node drug tag');
  ok(tags && tags[NODE_TAG_FDA] && tags[NODE_TAG_FDA].description.name === 'FDA Approved',
    'canvas data describes the node FDA tag');
  ok(tags && tags[EDGE_TAG_CLINICAL] && tags[EDGE_TAG_CLINICAL].description.name === 'Clinical Evidence',
    'canvas data describes the edge clinical tag');

  // Round-trip: the new canvas appears in the user's canvas list with its tag descriptions intact.
  const list = await getJson(CANVAS_PATH);
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  const listed = Array.isArray(list.json) && canvas ? list.json.find((c) => c.id === canvas.id) : null;
  ok(listed, 'created canvas appears in the canvas list');
  ok(listed && listed.data && listed.data.tags && listed.data.tags[NODE_TAG_DRUG],
    'listed canvas retains its tag descriptions');

  // A graph with no tags anywhere must still create cleanly (entities produce an empty {} tag set).
  const untagged = graphWithNodesAndEdges();
  untagged.tag_descriptions = {};
  const createUntagged = await postCanvas({ label: `${label} (untagged)`, layout, graph: untagged });
  ok(createUntagged.res.status === 200, `tagged-entities + empty tag_descriptions responds 200 (got ${createUntagged.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
