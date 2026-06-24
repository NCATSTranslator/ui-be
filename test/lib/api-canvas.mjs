/* Canvas graph fixtures for the API tests in test/api/.
 *
 * Builds the signed node/edge graph the POST /api/v1/users/me/canvas endpoint expects. Nodes and
 * edges placed on a canvas must carry the HMAC signature the server mints when it generates a
 * summary; we read the same secret the mock-ars server loads (configurations/secrets/secrets.json
 * via mock.json's _load_secrets) and sign each entity over its normalized form, exactly as the
 * server re-derives and verifies it. If the server runs with a different secrets file these mismatch.
 */

import { readFileSync } from 'node:fs';
import { SummaryNode } from '../../lib/summarization/SummaryNode.mjs';
import { SummaryEdge } from '../../lib/summarization/SummaryEdge.mjs';
import * as cmn from '../../lib/common.mjs';
import { postJson } from './api-harness.mjs';

export const CANVAS_PATH = '/api/v1/users/me/canvas';

const SIGNING_SECRET = JSON.parse(
  readFileSync(new URL('../../configurations/secrets/secrets.json', import.meta.url))).hmac.key;

// Stable refs so re-runs (and a second canvas reusing them) hit the shared-entity upsert path.
export const NODE_REF_1 = 'API_TEST:node-1';
export const NODE_REF_2 = 'API_TEST:node-2';
export const EDGE_REF_1 = 'API_TEST:edge-1';
export const SOURCE_TIME = '2026-06-23T12:00:00.000Z';
export const NEWER_SOURCE_TIME = '2026-06-24T12:00:00.000Z';
export const STALE_SOURCE_TIME = '2026-06-01T12:00:00.000Z';

export async function postCanvas(body) {
  return postJson(CANVAS_PATH, body);
}

// A graph node is a SummaryNode (flattened) plus canvas placement fields (x, y, optional hidden).
// The map key is authoritative for the node id, so the value need not repeat it.
export function testNode(ref, name, type, x, y, extra = {}) {
  return {
    aras: ['api-test-ara'],
    descriptions: [`${name} description`],
    names: [name],
    types: [type],
    synonyms: [],
    curies: [ref],
    provenance: [],
    tags: {},
    source_time: SOURCE_TIME,
    x: x,
    y: y,
    ...extra,
  };
}

// Sign a node exactly as the server does: over SummaryNode.from_object(node).to_raw_obj() with the
// map key supplied as the authoritative id. Returns the node with its `signature` attached.
export function signNode(ref, node) {
  const raw = SummaryNode.from_object({ ...node, id: ref }).to_raw_obj();
  return { ...node, signature: cmn.sign_entity_data(raw, SIGNING_SECRET) };
}

// A graph edge is a SummaryEdge (flattened) plus optional canvas fields (hidden, label). subject and
// object are node ids and must reference nodes present in the same graph. The map key is the edge id.
export function testEdge(subject, object, predicate, extra = {}) {
  return {
    aras: ['api-test-ara'],
    support: [],
    is_root: true,
    knowledge_level: 'trusted',
    description: null,
    type: 'direct',
    subject: subject,
    object: object,
    predicate: predicate,
    predicate_url: null,
    provenance: [],
    publications: {},
    metadata: null,
    trials: [],
    tags: {},
    source_time: SOURCE_TIME,
    ...extra,
  };
}

// Sign an edge exactly as the server does: over SummaryEdge.from_object(edge).to_raw_obj() with the
// map key supplied as the authoritative id. Returns the edge with its `signature` attached.
export function signEdge(ref, edge) {
  const raw = SummaryEdge.from_object({ ...edge, id: ref }).to_raw_obj();
  return { ...edge, signature: cmn.sign_entity_data(raw, SIGNING_SECRET) };
}

// variant alters the SummaryNode/SummaryEdge data for the same refs; combined with a strictly newer
// sourceTime, re-submitting it exercises the upsert's update branch (data IS DISTINCT FROM the stored
// data AND the incoming source_time is newer). Note node x/y live on the canvas_node placement, not
// node.data, so changing them alone would NOT count as a data change.
export function graphWithNodesAndEdges(variant = '', sourceTime = SOURCE_TIME) {
  const suffix = variant ? ` ${variant}` : '';
  return {
    nodes: {
      // node 1 carries annotations to exercise lossless round-trip + signing of annotation data
      [NODE_REF_1]: signNode(NODE_REF_1, testNode(NODE_REF_1, `API Test Node One${suffix}`, 'biolink:Disease', 10, 20,
        { annotations: { disease: { mondo: ['MONDO:0005148'] } }, source_time: sourceTime })),
      [NODE_REF_2]: signNode(NODE_REF_2, testNode(NODE_REF_2, `API Test Node Two${suffix}`, 'biolink:ChemicalEntity', 30, 40,
        { hidden: true, source_time: sourceTime })),
    },
    edges: {
      // edge 1 connects node 1 -> node 2; its endpoints resolve to the canvas node data ids. The
      // description varies with the variant so the update branch sees data IS DISTINCT FROM stored.
      [EDGE_REF_1]: signEdge(EDGE_REF_1, testEdge(NODE_REF_1, NODE_REF_2, 'biolink:treats',
        { description: `API test edge${suffix}`, source_time: sourceTime })),
    },
    tag_descriptions: {},
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
}
