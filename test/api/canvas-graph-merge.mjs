/* Standalone API test: POST /api/v1/users/me/canvas/:save_id/graph (merge a graph into a canvas).
 *
 * Merge folds a submitted graph into an existing canvas as a union: new nodes/edges are added, the
 * underlying shared data is refreshed, and (not exercisable via the API yet) soft-deleted entities
 * are undeleted. Entities already on the canvas keep their display fields (x/y/hidden/label/tags) -
 * merge never moves what is already there; positioning is handled elsewhere. Crucially, a submitted
 * edge may connect to a node already on the canvas (not just nodes in the same submission). The
 * endpoint returns the resulting CanvasGraph.
 *
 * Unique refs per run keep the test isolated from the shared entity pool. Assumes the server runs
 * with "auth_check": false (see mock/auth.mjs) against a real Postgres (the mock-ars server):
 *
 *   npm run mock-ars                          # shell 1: start the server (auth_check=false)
 *   node test/api/canvas-graph-merge.mjs      # shell 2
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { createHarness, postJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import {
  postCanvas, testNode, signNode, testEdge, signEdge, tagObject, CANVAS_PATH,
  NODE_TAG_DRUG, NODE_TAG_FDA,
} from '../lib/api-canvas.mjs';

const { ok, fail, finish } = createHarness();

const byRef = (rows, ref) => (Array.isArray(rows) ? rows.find((r) => r.ref === ref) : null);
const source = { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' };

// Two tag id-sets are equal iff they carry the same ids (the values are always the null marker).
const sameTagIds = (actual, expected) => {
  const a = Object.keys(actual || {}).sort();
  const b = Object.keys(expected).sort();
  return a.length === b.length && a.every((k, i) => k === b[i]);
};

// Assert the canvas graph the server returns is exactly the expected end state. Nodes/edges are
// matched by ref (server-assigned data ids are not predictable); edge endpoints are checked by
// mapping each subject_id/object_id back to the ref of the node that carries that data_id.
function verifyFinalGraph(actual, expected) {
  const nodes = (actual && actual.nodes) || [];
  const edges = (actual && actual.edges) || [];
  const expectedNodeRefs = Object.keys(expected.nodes);
  const expectedEdgeRefs = Object.keys(expected.edges);
  const expectedTagIds = Object.keys(expected.tags);

  ok(nodes.length === expectedNodeRefs.length, `final graph has ${expectedNodeRefs.length} nodes (got ${nodes.length})`);
  ok(edges.length === expectedEdgeRefs.length, `final graph has ${expectedEdgeRefs.length} edges (got ${edges.length})`);

  const refByDataId = new Map(nodes.map((n) => [n.data_id, n.ref]));
  for (const ref of expectedNodeRefs) {
    const want = expected.nodes[ref];
    const got = byRef(nodes, ref);
    ok(got, `final graph has node ${ref}`);
    if (!got) continue;
    ok(got.label === want.label && got.type === want.type
      && got.x === want.x && got.y === want.y && got.hidden === want.hidden,
      `node ${ref} matches expected display (got label=${got.label}, type=${got.type}, x=${got.x}, y=${got.y}, hidden=${got.hidden})`);
    ok(sameTagIds(got.tags, want.tags), `node ${ref} has the expected tag id-set`);
  }

  for (const ref of expectedEdgeRefs) {
    const want = expected.edges[ref];
    const got = byRef(edges, ref);
    ok(got, `final graph has edge ${ref}`);
    if (!got) continue;
    ok(got.label === want.label && got.hidden === want.hidden,
      `edge ${ref} matches expected display (got label=${got.label}, hidden=${got.hidden})`);
    ok(refByDataId.get(got.subject_id) === want.subject && refByDataId.get(got.object_id) === want.object,
      `edge ${ref} connects ${want.subject} -> ${want.object}`);
    ok(sameTagIds(got.tags, want.tags), `edge ${ref} has the expected tag id-set`);
  }

  const tags = (actual && actual.tags) || {};
  ok(Object.keys(tags).length === expectedTagIds.length,
    `final graph has ${expectedTagIds.length} tag descriptions (got ${Object.keys(tags).length})`);
  for (const tagId of expectedTagIds) {
    ok(tags[tagId] && tags[tagId].description && tags[tagId].description.name === expected.tags[tagId],
      `final graph tag ${tagId} describes "${expected.tags[tagId]}"`);
  }
}

console.log(`# POST ${CANVAS_PATH}/:save_id/graph  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:merge-A-${s}`;
  const refB = `API_TEST:merge-B-${s}`;
  const refC = `API_TEST:merge-C-${s}`;
  const refD = `API_TEST:merge-D-${s}`;
  const refF = `API_TEST:merge-F-${s}`;
  const eAB = `${refA}->${refB}`;
  const eBC = `${refB}->${refC}`;
  const eCA = `${refC}->${refA}`;

  // The end state the canvas must be in after every mutating request below. The no-op re-merges and
  // the 400/404 cases must leave this untouched; we GET the graph at the very end and assert it
  // matches. Node type is the sanitized biolink item (see get_specific_type); edge label is the raw
  // predicate. Tag id-sets list the ids present; the values are the null marker.
  const expectedGraph = {
    nodes: {
      [refA]: { label: 'Merge A', type: 'Disease', x: 10, y: 20, hidden: false, tags: { [NODE_TAG_DRUG]: null } },
      [refB]: { label: 'Merge B', type: 'ChemicalEntity', x: 30, y: 40, hidden: false, tags: {} },
      [refC]: { label: 'Merge C', type: 'Gene', x: 50, y: 60, hidden: false, tags: { [NODE_TAG_FDA]: null } },
      [refF]: { label: 'Merge F', type: 'Gene', x: 70, y: 80, hidden: false, tags: {} },
    },
    edges: {
      [eAB]: { subject: refA, object: refB, label: 'biolink:treats', hidden: false, tags: {} },
      [eBC]: { subject: refB, object: refC, label: 'biolink:treats', hidden: false, tags: {} },
      [eCA]: { subject: refC, object: refA, label: 'biolink:treats', hidden: false, tags: {} },
    },
    tags: { [NODE_TAG_DRUG]: 'Drug', [NODE_TAG_FDA]: 'FDA Approved' },
  };

  // Create a canvas with nodes A, B and edge A->B, plus the drug tag description.
  const createGraph = {
    nodes: {
      [refA]: signNode(refA, testNode(refA, 'Merge A', 'biolink:Disease', 10, 20,
        { tags: { [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug') } })),
      [refB]: signNode(refB, testNode(refB, 'Merge B', 'biolink:ChemicalEntity', 30, 40)),
    },
    edges: { [eAB]: signEdge(eAB, testEdge(refA, refB, 'biolink:treats')) },
    tag_descriptions: { [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug') },
    source,
  };
  const create = await postCanvas({ label: `api-test merge ${s}`, layout: 'horizontal', graph: createGraph });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const id = create.json && create.json.id;
  ok(id != null, 'created canvas has an id');

  // Merge: new node C (FDA tag), edge B->C (B already on canvas), edge C->A (A already on canvas).
  const mergeGraph = {
    nodes: {
      [refC]: signNode(refC, testNode(refC, 'Merge C', 'biolink:Gene', 50, 60,
        { tags: { [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved') } })),
    },
    edges: {
      [eBC]: signEdge(eBC, testEdge(refB, refC, 'biolink:treats')),
      [eCA]: signEdge(eCA, testEdge(refC, refA, 'biolink:treats')),
    },
    tag_descriptions: { [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved') },
    source,
  };
  const merge = await postJson(`${CANVAS_PATH}/${id}/graph`, mergeGraph);
  ok(merge.res.status === 200, `merge responds 200 (got ${merge.res.status})`);
  const g = merge.json;
  ok(g && Array.isArray(g.nodes) && g.nodes.length === 3, `merged graph has all 3 nodes (got ${g && g.nodes && g.nodes.length})`);
  ok(g && Array.isArray(g.edges) && g.edges.length === 3, `merged graph has all 3 edges (got ${g && g.edges && g.edges.length})`);

  const nodeA = byRef(g.nodes, refA);
  const nodeB = byRef(g.nodes, refB);
  const nodeC = byRef(g.nodes, refC);
  ok(nodeC, 'new node C was added by the merge');
  ok(nodeC && nodeC.tags && nodeC.tags[NODE_TAG_FDA] === null, 'new node C carries its tag id-set');

  // Tag descriptions are the union of the canvas's and the merge's.
  ok(g.tags && g.tags[NODE_TAG_DRUG] && g.tags[NODE_TAG_DRUG].description.name === 'Drug', 'canvas keeps the drug tag description');
  ok(g.tags && g.tags[NODE_TAG_FDA] && g.tags[NODE_TAG_FDA].description.name === 'FDA Approved', 'merge added the FDA tag description');

  // Edges connect to the right canvas nodes, including endpoints that already existed on the canvas.
  const edgeBC = byRef(g.edges, eBC);
  const edgeCA = byRef(g.edges, eCA);
  ok(edgeBC && nodeB && nodeC && edgeBC.subject_id === nodeB.data_id && edgeBC.object_id === nodeC.data_id,
    'edge B->C resolves to the existing B and the new C');
  ok(edgeCA && nodeC && nodeA && edgeCA.subject_id === nodeC.data_id && edgeCA.object_id === nodeA.data_id,
    'edge C->A resolves to the new C and the existing A');

  // Merging an existing node with new coordinates must NOT move it (display fields preserved).
  const moveA = {
    nodes: { [refA]: signNode(refA, testNode(refA, 'Merge A moved', 'biolink:Disease', 999, 888,
      { tags: { [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug') } })) },
    edges: {}, tag_descriptions: {}, source,
  };
  const remerge = await postJson(`${CANVAS_PATH}/${id}/graph`, moveA);
  ok(remerge.res.status === 200, `re-merge of existing node responds 200 (got ${remerge.res.status})`);
  const keptA = byRef(remerge.json && remerge.json.nodes, refA);
  ok(keptA && keptA.x === 10 && keptA.y === 20, `existing node A keeps its placement (got ${keptA && keptA.x},${keptA && keptA.y})`);

  // Merge is idempotent: re-submitting the same graph adds nothing.
  const again = await postJson(`${CANVAS_PATH}/${id}/graph`, mergeGraph);
  ok(again.res.status === 200, `re-merge responds 200 (got ${again.res.status})`);
  ok(again.json && again.json.nodes.length === 3 && again.json.edges.length === 3, 're-merging the same graph adds no duplicates');

  // Merging a single node with no edges adds just that node.
  const singleNode = {
    nodes: { [refF]: signNode(refF, testNode(refF, 'Merge F', 'biolink:Gene', 70, 80)) },
    edges: {}, tag_descriptions: {}, source,
  };
  const single = await postJson(`${CANVAS_PATH}/${id}/graph`, singleNode);
  ok(single.res.status === 200, `single-node merge responds 200 (got ${single.res.status})`);
  const nodeF = byRef(single.json && single.json.nodes, refF);
  ok(nodeF, 'single-node merge added node F');
  ok(single.json && single.json.nodes.length === 4 && single.json.edges.length === 3,
    `single-node merge adds only the node (got ${single.json && single.json.nodes && single.json.nodes.length} nodes, ${single.json && single.json.edges && single.json.edges.length} edges)`);

  // An edge to a node neither submitted nor on the canvas is a 400.
  const dangling = {
    nodes: {}, edges: { [`${refA}->${refD}`]: signEdge(`${refA}->${refD}`, testEdge(refA, refD, 'biolink:treats')) },
    tag_descriptions: {}, source,
  };
  const danglingRes = await postJson(`${CANVAS_PATH}/${id}/graph`, dangling);
  ok(danglingRes.res.status === 400, `edge to an absent node -> 400 (got ${danglingRes.res.status})`);

  // A tampered signature is a 400.
  const refE = `API_TEST:merge-E-${s}`;
  const tampered = { ...signNode(refE, testNode(refE, 'Merge E', 'biolink:Disease', 1, 2)), signature: 'deadbeef' };
  const badSig = await postJson(`${CANVAS_PATH}/${id}/graph`, { nodes: { [refE]: tampered }, edges: {}, tag_descriptions: {}, source });
  ok(badSig.res.status === 400, `bad signature -> 400 (got ${badSig.res.status})`);

  // A missing/empty body is a 400.
  const emptyBody = await postJson(`${CANVAS_PATH}/${id}/graph`, {});
  ok(emptyBody.res.status === 400, `empty merge body -> 400 (got ${emptyBody.res.status})`);

  // Merging into a canvas that does not exist is a 404.
  const missing = await postJson(`${CANVAS_PATH}/999999999/graph`, { nodes: {}, edges: {}, tag_descriptions: {}, source });
  ok(missing.res.status === 404, `unknown canvas id -> 404 (got ${missing.res.status})`);

  // A non-numeric id is a 400.
  const badId = await postJson(`${CANVAS_PATH}/not-a-number/graph`, { nodes: {}, edges: {}, tag_descriptions: {}, source });
  ok(badId.res.status === 400, `non-numeric canvas id -> 400 (got ${badId.res.status})`);

  // The end state matches what we expected up front; the failed requests above left it intact.
  const finalRes = await getJson(`${CANVAS_PATH}/${id}/graph`);
  ok(finalRes.res.status === 200, `final graph fetch responds 200 (got ${finalRes.res.status})`);
  verifyFinalGraph(finalRes.json, expectedGraph);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
