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

// Tag ids carried by the fixture entities. The full tag objects live in the entity data (and are
// signed); the server denormalizes the ids onto canvas_node/canvas_edge.tags and the descriptions
// onto canvas.data.tags via the graph's tag_descriptions.
export const NODE_TAG_DRUG = 'r/cc/drug';
export const NODE_TAG_FDA = 'r/fda/approved';
export const EDGE_TAG_CLINICAL = 'p/ev/clinical';

// A TagObject as the summary schema defines it: an id plus a user-facing description.
export function tagObject(id, name, description = '') {
  return { id, description: { name, description } };
}

export async function postCanvas(body) {
  return postJson(CANVAS_PATH, body);
}

// A graph node is built here as a SummaryNode (flattened) plus canvas placement fields (x, y, optional
// hidden/label/user_data_id); signNode nests the SummaryNode fields under `data`, the Translator slot.
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

// Sign a node exactly as the server does: over SummaryNode.from_object(data).to_raw_obj() with ref as
// its id. Returns the placement fields with the signed data (carrying its `id` and `signature`) in the
// `data` slot.
export function signNode(ref, node) {
  const { placement, data } = splitGraphEntry(node, NODE_PLACEMENT_FIELDS);
  const signed = { ...data, id: ref };
  const raw = SummaryNode.from_object(signed).to_raw_obj();
  return { ...placement, data: { ...signed, signature: cmn.sign_entity_data(raw, SIGNING_SECRET) } };
}

// A graph edge is built here as a SummaryEdge (flattened) plus optional canvas fields (hidden, label,
// user_data_id); signEdge nests the SummaryEdge fields under `data`. subject and object are node ids
// and must reference nodes present in the same graph.
export function testEdge(subject, object, predicate, extra = {}) {
  return {
    aras: ['api-test-ara'],
    is_root: true,
    knowledge_level: 'trusted',
    description: null,
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

// Sign an edge exactly as the server does: over SummaryEdge.from_object(data).to_raw_obj() with ref as
// its id. Returns the canvas fields with the signed data (carrying its `id` and `signature`) in the
// `data` slot.
export function signEdge(ref, edge) {
  const { placement, data } = splitGraphEntry(edge, EDGE_PLACEMENT_FIELDS);
  const signed = { ...data, id: ref };
  const raw = SummaryEdge.from_object(signed).to_raw_obj();
  return { ...placement, data: { ...signed, signature: cmn.sign_entity_data(raw, SIGNING_SECRET) } };
}

export function graphEntry(entries, ref) {
  return entries.find((entry) => entry.data?.id === ref);
}

const NODE_PLACEMENT_FIELDS = Object.freeze(['x', 'y', 'hidden', 'label', 'user_data_id']);
const EDGE_PLACEMENT_FIELDS = Object.freeze(['hidden', 'label', 'user_data_id']);

function splitGraphEntry(entry, placementFields) {
  const placement = {};
  const data = {};
  for (const [field, value] of Object.entries(entry)) {
    if (placementFields.includes(field)) placement[field] = value;
    else data[field] = value;
  }
  return { placement, data };
}

// variant alters the SummaryNode/SummaryEdge data for the same refs; combined with a strictly newer
// sourceTime, re-submitting it exercises the upsert's update branch (data IS DISTINCT FROM the stored
// data AND the incoming source_time is newer). Note node x/y live on the canvas_node placement, not
// node.data, so changing them alone would NOT count as a data change.
export function graphWithNodesAndEdges(variant = '', sourceTime = SOURCE_TIME) {
  const suffix = variant ? ` ${variant}` : '';
  return {
    nodes: [
      // node 1 carries annotations to exercise lossless round-trip + signing of annotation data, and
      // tags to exercise denormalizing the entity's tag ids onto canvas_node.tags
      signNode(NODE_REF_1, testNode(NODE_REF_1, `API Test Node One${suffix}`, 'biolink:Disease', 10, 20,
        {
          annotations: { disease: { mondo: ['MONDO:0005148'] } },
          tags: {
            [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug'),
            [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved'),
          },
          source_time: sourceTime,
        })),
      signNode(NODE_REF_2, testNode(NODE_REF_2, `API Test Node Two${suffix}`, 'biolink:ChemicalEntity', 30, 40,
        { hidden: true, source_time: sourceTime })),
    ],
    edges: [
      // edge 1 connects node 1 -> node 2; its endpoints resolve to the canvas node data ids. The
      // description varies with the variant so the update branch sees data IS DISTINCT FROM stored.
      signEdge(EDGE_REF_1, testEdge(NODE_REF_1, NODE_REF_2, 'biolink:treats',
        {
          description: `API test edge${suffix}`,
          tags: { [EDGE_TAG_CLINICAL]: tagObject(EDGE_TAG_CLINICAL, 'Clinical Evidence') },
          source_time: sourceTime,
        })),
    ],
    tag_descriptions: {
      [NODE_TAG_DRUG]: tagObject(NODE_TAG_DRUG, 'Drug'),
      [NODE_TAG_FDA]: tagObject(NODE_TAG_FDA, 'FDA Approved'),
      [EDGE_TAG_CLINICAL]: tagObject(EDGE_TAG_CLINICAL, 'Clinical Evidence'),
    },
    source: { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' },
  };
}
