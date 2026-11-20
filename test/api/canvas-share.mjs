import {
  createHarness, postJson, putJson, getJson, deleteJson, BASE_URL, TEST_USER_ID
} from '../lib/api-harness.mjs';
import { postCanvas, signNode, signEdge, testNode, testEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const SHARE_PATH = '/api/v1/canvas/share';
const UNKNOWN_SHARE_ID = '00000000-0000-4000-8000-000000000000';

const { ok, fail, finish } = createHarness();

function node_fields(nodes) {
  return nodes.map((node) => ({
    ref: node.ref, label: node.label, type: node.type, x: node.x, y: node.y, hidden: node.hidden
  }));
}

function annotation_fields(annotations) {
  return annotations.map((annotation) => ({
    content: annotation.content, x: annotation.x, y: annotation.y, width: annotation.width, height: annotation.height
  }));
}

function link_fields(links) {
  return links.map((link) => ({ id: link.id, canvas_id: link.canvas_id, user_id: link.user_id }));
}

function same(actual, expected) {
  return JSON.stringify(actual) === JSON.stringify(expected);
}

async function canvas_time_updated(canvas_id) {
  const list = await getJson(`${CANVAS_PATH}?include_deleted=true`);
  return list.json.find((canvas) => canvas.id === canvas_id).time_updated;
}

console.log(`# canvas share links  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const stamp = Date.now();
  const ref_a = `API_TEST:share-A-${stamp}`;
  const ref_b = `API_TEST:share-B-${stamp}`;
  const ref_c = `API_TEST:share-C-${stamp}`;
  const ref_d = `API_TEST:share-D-${stamp}`;
  const ref_ab = `${ref_a}->${ref_b}`;
  const ref_bc = `${ref_b}->${ref_c}`;

  const create_first = await postCanvas({
    label: `api-test share first ${stamp}`,
    layout: 'vertical',
    graph: {
      nodes: [
        signNode(ref_a, testNode(ref_a, 'Share A', 'biolink:Disease', 10, 20, { label: 'Alpha' })),
        signNode(ref_b, testNode(ref_b, 'Share B', 'biolink:Gene', 30, 40, { label: 'Beta', hidden: true })),
        signNode(ref_c, testNode(ref_c, 'Share C', 'biolink:Disease', 50, 60, { label: 'Gamma' })),
      ],
      edges: [
        signEdge(ref_ab, testEdge(ref_a, ref_b, 'biolink:treats', { label: 'alpha to beta' })),
        signEdge(ref_bc, testEdge(ref_b, ref_c, 'biolink:treats', { label: 'beta to gamma' })),
      ],
      tag_descriptions: {},
      source: { query_ref: 'API_TEST_SHARE_QID', result_ref: 'API_TEST_SHARE_RID' },
    },
  });
  ok(create_first.res.status === 200, `create first canvas responds 200 (got ${create_first.res.status})`);
  const create_second = await postCanvas({
    label: `api-test share second ${stamp}`,
    layout: 'horizontal',
    graph: {
      nodes: [signNode(ref_d, testNode(ref_d, 'Share D', 'biolink:Disease', 70, 80, { label: 'Delta' }))],
      edges: [],
      tag_descriptions: {},
      source: { query_ref: 'API_TEST_SHARE_QID', result_ref: 'API_TEST_SHARE_RID' },
    },
  });
  ok(create_second.res.status === 200, `create second canvas responds 200 (got ${create_second.res.status})`);
  const first_id = create_first.json.id;
  const second_id = create_second.json.id;
  const first_path = `${CANVAS_PATH}/${first_id}`;
  const second_path = `${CANVAS_PATH}/${second_id}`;

  const kept_note = await postJson(`${first_path}/annotation`,
    { content: 'kept note', x: 1, y: 2, width: 30, height: 40 });
  ok(kept_note.res.status === 200, `create kept annotation responds 200 (got ${kept_note.res.status})`);
  const trashed_note = await postJson(`${first_path}/annotation`,
    { content: 'trashed note', x: 5, y: 6, width: 70, height: 80 });
  ok(trashed_note.res.status === 200, `create trashed annotation responds 200 (got ${trashed_note.res.status})`);
  const first_before = await getJson(`${first_path}/graph`);
  const node_id = new Map(first_before.json.nodes.map((node) => [node.ref, node.id]));
  const edge_id = new Map(first_before.json.edges.map((edge) => [edge.ref, edge.id]));
  const trash = await putJson(`${first_path}/graph/trash`,
    { nodes: [node_id.get(ref_c)], annotations: [trashed_note.json.id] });
  ok(trash.res.status === 200, `trash node C and one annotation responds 200 (got ${trash.res.status})`);
  const second_graph = await getJson(`${second_path}/graph`);
  const node_d_id = second_graph.json.nodes[0].id;

  const no_links = await getJson(`${first_path}/share`);
  ok(no_links.res.status === 200 && same(no_links.json, []),
    `a canvas starts with no share links (got ${no_links.res.status})`);

  const updated_before_links = await canvas_time_updated(first_id);
  const link_1 = await postJson(`${first_path}/share`);
  ok(link_1.res.status === 200, `create a link responds 200 (got ${link_1.res.status})`);
  const link_2 = await postJson(`${first_path}/share`);
  ok(link_2.res.status === 200, `create a second link on the same canvas responds 200 (got ${link_2.res.status})`);
  const link_other = await postJson(`${second_path}/share`);
  ok(link_other.res.status === 200, `create a link on the second canvas responds 200 (got ${link_other.res.status})`);
  const share_1 = link_1.json.id;
  const share_2 = link_2.json.id;
  const share_other = link_other.json.id;
  ok(new Set([share_1, share_2, share_other]).size === 3, 'each link has its own id');
  ok(await canvas_time_updated(first_id) === updated_before_links,
    'creating links leaves the canvas update time alone');

  const first_links = await getJson(`${first_path}/share`);
  ok(first_links.res.status === 200 && same(link_fields(first_links.json), [
    { id: share_1, canvas_id: first_id, user_id: TEST_USER_ID },
    { id: share_2, canvas_id: first_id, user_id: TEST_USER_ID },
  ]), "listing a canvas's links returns both of its links, oldest first");
  const second_links = await getJson(`${second_path}/share`);
  ok(second_links.res.status === 200 && same(link_fields(second_links.json), [
    { id: share_other, canvas_id: second_id, user_id: TEST_USER_ID },
  ]), "listing the second canvas's links returns only its own link");

  const owner_first = await getJson(`${first_path}/graph`);
  const shared_first = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(shared_first.res.status === 200, `reading a graph through a link responds 200 (got ${shared_first.res.status})`);
  ok(same(node_fields(shared_first.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 10, y: 20, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), "a link shows the canvas's active nodes and not the trashed one");
  ok(shared_first.json.edges.length === 1 && shared_first.json.edges[0].ref === ref_ab
    && shared_first.json.edges[0].label === 'alpha to beta'
    && shared_first.json.edges[0].subject_id === node_id.get(ref_a)
    && shared_first.json.edges[0].object_id === node_id.get(ref_b),
    "a link shows the canvas's active edge and not the one lost with the trashed node");
  ok(same(annotation_fields(shared_first.json.annotations),
    [{ content: 'kept note', x: 1, y: 2, width: 30, height: 40 }]),
    "a link shows the canvas's active annotation and not the trashed one");
  ok(same(shared_first.json, owner_first.json), 'the shared graph is the graph the owner reads');
  const shared_first_again = await getJson(`${SHARE_PATH}/${share_2}/graph`);
  ok(shared_first_again.res.status === 200 && same(shared_first_again.json, shared_first.json),
    'two links to one canvas show the same graph');
  const shared_second = await getJson(`${SHARE_PATH}/${share_other}/graph`);
  ok(shared_second.res.status === 200 && same(node_fields(shared_second.json.nodes), [
    { ref: ref_d, label: 'Delta', type: 'Disease', x: 70, y: 80, hidden: false },
  ]) && same(shared_second.json.edges, []) && same(shared_second.json.annotations, []),
    'a link to the second canvas shows the second canvas');
  const shared_with_deleted = await getJson(`${SHARE_PATH}/${share_1}/graph?include_deleted=true`);
  ok(shared_with_deleted.res.status === 200 && same(shared_with_deleted.json, shared_first.json),
    'include_deleted does not reveal trashed elements through a link');

  const shared_node = await getJson(`${SHARE_PATH}/${share_1}/node/${node_id.get(ref_a)}`);
  ok(shared_node.res.status === 200 && shared_node.json.id === ref_a && same(shared_node.json.names, ['Share A']),
    `a link reads a node's Translator data (got ${shared_node.res.status})`);
  const shared_edge = await getJson(`${SHARE_PATH}/${share_1}/edge/${edge_id.get(ref_ab)}`);
  ok(shared_edge.res.status === 200 && shared_edge.json.id === ref_ab && shared_edge.json.subject === ref_a
    && shared_edge.json.object === ref_b && shared_edge.json.predicate === 'biolink:treats',
    `a link reads an edge's Translator data (got ${shared_edge.res.status})`);
  const shared_node_d = await getJson(`${SHARE_PATH}/${share_other}/node/${node_d_id}`);
  ok(shared_node_d.res.status === 200 && shared_node_d.json.id === ref_d && same(shared_node_d.json.names, ['Share D']),
    `the second canvas's link reads its own node (got ${shared_node_d.res.status})`);
  const cross_node = await getJson(`${SHARE_PATH}/${share_other}/node/${node_id.get(ref_a)}`);
  ok(cross_node.res.status === 404, `a node is not readable through a link to another canvas (got ${cross_node.res.status})`);
  const cross_edge = await getJson(`${SHARE_PATH}/${share_other}/edge/${edge_id.get(ref_ab)}`);
  ok(cross_edge.res.status === 404, `an edge is not readable through a link to another canvas (got ${cross_edge.res.status})`);
  const unknown_node = await getJson(`${SHARE_PATH}/${share_1}/node/999999999`);
  ok(unknown_node.res.status === 404, `unknown node id through a link -> 404 (got ${unknown_node.res.status})`);
  const unknown_edge = await getJson(`${SHARE_PATH}/${share_1}/edge/999999999`);
  ok(unknown_edge.res.status === 404, `unknown edge id through a link -> 404 (got ${unknown_edge.res.status})`);
  const bad_node = await getJson(`${SHARE_PATH}/${share_1}/node/not-a-number`);
  ok(bad_node.res.status === 400, `non-numeric node id through a link -> 400 (got ${bad_node.res.status})`);
  const bad_edge = await getJson(`${SHARE_PATH}/${share_1}/edge/not-a-number`);
  ok(bad_edge.res.status === 400, `non-numeric edge id through a link -> 400 (got ${bad_edge.res.status})`);

  for (const read of ['graph', `node/${node_id.get(ref_a)}`, `edge/${edge_id.get(ref_ab)}`]) {
    const unknown_link = await getJson(`${SHARE_PATH}/${UNKNOWN_SHARE_ID}/${read}`);
    ok(unknown_link.res.status === 404, `unknown link on ${read} -> 404 (got ${unknown_link.res.status})`);
    const bad_link = await getJson(`${SHARE_PATH}/not-a-uuid/${read}`);
    ok(bad_link.res.status === 400, `non-UUID link on ${read} -> 400 (got ${bad_link.res.status})`);
  }

  const move = await putJson(`${first_path}/graph/geometry`, { nodes: [{ id: node_id.get(ref_a), x: 111, y: 222 }] });
  ok(move.res.status === 200, `the owner moving a node responds 200 (got ${move.res.status})`);
  const shared_moved = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(same(node_fields(shared_moved.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 111, y: 222, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), "the owner's later edit shows through the link");

  const shared_write = await putJson(`${SHARE_PATH}/${share_1}/graph/geometry`,
    { nodes: [{ id: node_id.get(ref_a), x: 0, y: 0 }] });
  ok(shared_write.res.status === 403, `writing through a link -> 403 (got ${shared_write.res.status})`);
  const after_shared_write = await getJson(`${first_path}/graph`);
  ok(same(node_fields(after_shared_write.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 111, y: 222, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'the refused write changed nothing on the canvas');

  const plain = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`, {});
  ok(plain.res.status === 200, `copy through a link with an empty body responds 200 (got ${plain.res.status})`);
  const annotated = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`,
    { label: `api-test share renamed ${stamp}`, include_annotations: true });
  ok(annotated.res.status === 200, `copy through a link with a label and annotations responds 200 (got ${annotated.res.status})`);
  const second_copy = await postJson(`${CANVAS_PATH}/share/${share_other}/copy`, {});
  ok(second_copy.res.status === 200, `copy through the second canvas's link responds 200 (got ${second_copy.res.status})`);
  const plain_id = plain.json.id;
  const annotated_id = annotated.json.id;
  const second_copy_id = second_copy.json.id;
  ok(new Set([first_id, second_id, plain_id, annotated_id, second_copy_id]).size === 5,
    'the sources and the three copies have distinct ids');

  const list = await getJson(CANVAS_PATH);
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  const listed_plain = list.json.find((canvas) => canvas.id === plain_id);
  const listed_annotated = list.json.find((canvas) => canvas.id === annotated_id);
  const listed_second_copy = list.json.find((canvas) => canvas.id === second_copy_id);
  ok(listed_plain && listed_plain.label === `api-test share first ${stamp}` && listed_plain.layout === 'vertical',
    'a copy made through a link without a label keeps the source label and layout');
  ok(listed_annotated && listed_annotated.label === `api-test share renamed ${stamp}`,
    'a copy made through a link with a label takes that label');
  ok(listed_second_copy && listed_second_copy.label === `api-test share second ${stamp}`
    && listed_second_copy.layout === 'horizontal',
    "a copy made through the second canvas's link is a copy of the second canvas");
  ok(listed_plain && listed_plain.user_id === TEST_USER_ID && listed_plain.time_deleted === null,
    'the copy is an active canvas of the copying user');
  ok(listed_plain && listed_plain.data.query_ref === 'API_TEST_SHARE_QID'
    && listed_plain.data.result_ref === 'API_TEST_SHARE_RID',
    'the copy keeps the source query and result refs');

  const plain_graph = await getJson(`${CANVAS_PATH}/${plain_id}/graph?include_deleted=true`);
  const annotated_graph = await getJson(`${CANVAS_PATH}/${annotated_id}/graph?include_deleted=true`);
  const second_copy_graph = await getJson(`${CANVAS_PATH}/${second_copy_id}/graph?include_deleted=true`);
  ok(plain_graph.res.status === 200, `plain copy graph responds 200 (got ${plain_graph.res.status})`);
  ok(annotated_graph.res.status === 200, `annotated copy graph responds 200 (got ${annotated_graph.res.status})`);
  ok(second_copy_graph.res.status === 200, `second copy graph responds 200 (got ${second_copy_graph.res.status})`);
  for (const [name, graph] of [['plain', plain_graph.json], ['annotated', annotated_graph.json]]) {
    ok(same(node_fields(graph.nodes), [
      { ref: ref_a, label: 'Alpha', type: 'Disease', x: 111, y: 222, hidden: false },
      { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
    ]), `the ${name} copy has the active source nodes as they are now and not the trashed one`);
    const copy_node_id = new Map(graph.nodes.map((node) => [node.ref, node.id]));
    ok(graph.edges.length === 1 && graph.edges[0].ref === ref_ab && graph.edges[0].label === 'alpha to beta'
      && graph.edges[0].subject_id === copy_node_id.get(ref_a) && graph.edges[0].object_id === copy_node_id.get(ref_b),
      `the ${name} copy has the active source edge, connecting the copy's own nodes`);
    ok(copy_node_id.get(ref_a) !== node_id.get(ref_a) && copy_node_id.get(ref_b) !== node_id.get(ref_b),
      `the ${name} copy's nodes are new rows, not the source's`);
  }
  ok(same(plain_graph.json.annotations, []), 'a copy made through a link without include_annotations has no annotations');
  ok(same(annotation_fields(annotated_graph.json.annotations),
    [{ content: 'kept note', x: 1, y: 2, width: 30, height: 40 }]),
    'a copy made through a link with include_annotations has the active source annotation and not the trashed one');
  ok(same(node_fields(second_copy_graph.json.nodes), [
    { ref: ref_d, label: 'Delta', type: 'Disease', x: 70, y: 80, hidden: false },
  ]), "the second canvas's copy has the second canvas's node");

  const plain_node_a = plain_graph.json.nodes.find((node) => node.ref === ref_a);
  const copy_node_data = await getJson(`${CANVAS_PATH}/${plain_id}/node/${plain_node_a.id}`);
  ok(copy_node_data.res.status === 200 && copy_node_data.json.id === ref_a && same(copy_node_data.json.names, ['Share A']),
    "the copy's node resolves to the same Translator data");
  const copy_links = await getJson(`${CANVAS_PATH}/${plain_id}/share`);
  ok(copy_links.res.status === 200 && same(copy_links.json, []), "a copy does not inherit the source's links");
  const source_links = await getJson(`${first_path}/share`);
  ok(same(link_fields(source_links.json), [
    { id: share_1, canvas_id: first_id, user_id: TEST_USER_ID },
    { id: share_2, canvas_id: first_id, user_id: TEST_USER_ID },
  ]), "copying through a link leaves the source's links in place");

  const move_copy = await putJson(`${CANVAS_PATH}/${plain_id}/graph/geometry`,
    { nodes: [{ id: plain_node_a.id, x: 333, y: 444 }] });
  ok(move_copy.res.status === 200, `moving a node on the copy responds 200 (got ${move_copy.res.status})`);
  const plain_after = await getJson(`${CANVAS_PATH}/${plain_id}/graph`);
  ok(same(node_fields(plain_after.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 333, y: 444, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'the move took effect on the copy');
  const shared_after_copy_move = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(same(node_fields(shared_after_copy_move.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 111, y: 222, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'editing the copy leaves the shared source untouched');

  const count_before = (await getJson(`${CANVAS_PATH}?include_deleted=true`)).json.length;
  const bad_label = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`, { label: 42 });
  ok(bad_label.res.status === 400, `non-string label -> 400 (got ${bad_label.res.status})`);
  const bad_flag = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`, { include_annotations: 'yes' });
  ok(bad_flag.res.status === 400, `non-boolean include_annotations -> 400 (got ${bad_flag.res.status})`);
  const no_body = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`);
  ok(no_body.res.status === 400, `copy through a link without a body -> 400 (got ${no_body.res.status})`);
  const copy_bad_link = await postJson(`${CANVAS_PATH}/share/not-a-uuid/copy`, {});
  ok(copy_bad_link.res.status === 400, `copy through a non-UUID link -> 400 (got ${copy_bad_link.res.status})`);
  const copy_unknown_link = await postJson(`${CANVAS_PATH}/share/${UNKNOWN_SHARE_ID}/copy`, {});
  ok(copy_unknown_link.res.status === 404, `copy through an unknown link -> 404 (got ${copy_unknown_link.res.status})`);

  const list_bad_canvas = await getJson(`${CANVAS_PATH}/not-a-number/share`);
  ok(list_bad_canvas.res.status === 400, `list links with a non-numeric canvas id -> 400 (got ${list_bad_canvas.res.status})`);
  const list_unknown_canvas = await getJson(`${CANVAS_PATH}/999999999/share`);
  ok(list_unknown_canvas.res.status === 404, `list links of an unknown canvas -> 404 (got ${list_unknown_canvas.res.status})`);
  const create_bad_canvas = await postJson(`${CANVAS_PATH}/not-a-number/share`);
  ok(create_bad_canvas.res.status === 400, `create a link with a non-numeric canvas id -> 400 (got ${create_bad_canvas.res.status})`);
  const create_unknown_canvas = await postJson(`${CANVAS_PATH}/999999999/share`);
  ok(create_unknown_canvas.res.status === 404, `create a link on an unknown canvas -> 404 (got ${create_unknown_canvas.res.status})`);

  const delete_wrong_canvas = await deleteJson(`${first_path}/share/${share_other}`);
  ok(delete_wrong_canvas.res.status === 404,
    `deleting a link through another canvas -> 404 (got ${delete_wrong_canvas.res.status})`);
  const delete_unknown_canvas = await deleteJson(`${CANVAS_PATH}/999999999/share/${share_other}`);
  ok(delete_unknown_canvas.res.status === 404,
    `deleting a link through an unknown canvas -> 404 (got ${delete_unknown_canvas.res.status})`);
  const delete_unknown_link = await deleteJson(`${first_path}/share/${UNKNOWN_SHARE_ID}`);
  ok(delete_unknown_link.res.status === 404, `deleting an unknown link -> 404 (got ${delete_unknown_link.res.status})`);
  const delete_bad_link = await deleteJson(`${first_path}/share/not-a-uuid`);
  ok(delete_bad_link.res.status === 400, `deleting a non-UUID link -> 400 (got ${delete_bad_link.res.status})`);
  const delete_bad_canvas = await deleteJson(`${CANVAS_PATH}/not-a-number/share/${share_1}`);
  ok(delete_bad_canvas.res.status === 400,
    `deleting a link with a non-numeric canvas id -> 400 (got ${delete_bad_canvas.res.status})`);
  const still_other = await getJson(`${SHARE_PATH}/${share_other}/graph`);
  ok(still_other.res.status === 200, `a link survives the refused deletes (got ${still_other.res.status})`);

  const updated_before_delete = await canvas_time_updated(first_id);
  const delete_2 = await deleteJson(`${first_path}/share/${share_2}`);
  ok(delete_2.res.status === 200, `deleting a link responds 200 (got ${delete_2.res.status})`);
  ok(await canvas_time_updated(first_id) === updated_before_delete,
    'deleting a link leaves the canvas update time alone');
  const links_after_delete = await getJson(`${first_path}/share`);
  ok(same(link_fields(links_after_delete.json), [
    { id: share_1, canvas_id: first_id, user_id: TEST_USER_ID },
  ]), 'the deleted link is gone from the list and the other link stays');
  const deleted_graph = await getJson(`${SHARE_PATH}/${share_2}/graph`);
  ok(deleted_graph.res.status === 404, `a deleted link no longer reads the graph (got ${deleted_graph.res.status})`);
  const deleted_node = await getJson(`${SHARE_PATH}/${share_2}/node/${node_id.get(ref_a)}`);
  ok(deleted_node.res.status === 404, `a deleted link no longer reads node data (got ${deleted_node.res.status})`);
  const deleted_edge = await getJson(`${SHARE_PATH}/${share_2}/edge/${edge_id.get(ref_ab)}`);
  ok(deleted_edge.res.status === 404, `a deleted link no longer reads edge data (got ${deleted_edge.res.status})`);
  const deleted_copy = await postJson(`${CANVAS_PATH}/share/${share_2}/copy`, {});
  ok(deleted_copy.res.status === 404, `a deleted link no longer copies (got ${deleted_copy.res.status})`);
  const surviving_graph = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(surviving_graph.res.status === 200, `the other link to the same canvas still works (got ${surviving_graph.res.status})`);
  const delete_again = await deleteJson(`${first_path}/share/${share_2}`);
  ok(delete_again.res.status === 404, `deleting a link twice -> 404 (got ${delete_again.res.status})`);

  const trash_first = await putJson(`${CANVAS_PATH}/trash`, [first_id]);
  ok(trash_first.res.status === 200, `trash the first canvas responds 200 (got ${trash_first.res.status})`);
  const trashed_graph = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(trashed_graph.res.status === 404, `a link to a trashed canvas does not read the graph (got ${trashed_graph.res.status})`);
  const trashed_node = await getJson(`${SHARE_PATH}/${share_1}/node/${node_id.get(ref_a)}`);
  ok(trashed_node.res.status === 404, `a link to a trashed canvas does not read node data (got ${trashed_node.res.status})`);
  const trashed_edge = await getJson(`${SHARE_PATH}/${share_1}/edge/${edge_id.get(ref_ab)}`);
  ok(trashed_edge.res.status === 404, `a link to a trashed canvas does not read edge data (got ${trashed_edge.res.status})`);
  const trashed_copy = await postJson(`${CANVAS_PATH}/share/${share_1}/copy`, {});
  ok(trashed_copy.res.status === 404, `a link to a trashed canvas does not copy (got ${trashed_copy.res.status})`);
  const trashed_list = await getJson(`${first_path}/share`);
  ok(trashed_list.res.status === 404, `listing the links of a trashed canvas -> 404 (got ${trashed_list.res.status})`);
  const trashed_create = await postJson(`${first_path}/share`);
  ok(trashed_create.res.status === 404, `creating a link on a trashed canvas -> 404 (got ${trashed_create.res.status})`);
  const count_after = (await getJson(`${CANVAS_PATH}?include_deleted=true`)).json.length;
  ok(count_after === count_before, 'the rejected copies created no canvas');

  const restore_first = await putJson(`${CANVAS_PATH}/restore`, [first_id]);
  ok(restore_first.res.status === 200, `restore the first canvas responds 200 (got ${restore_first.res.status})`);
  const restored_graph = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(restored_graph.res.status === 200 && same(restored_graph.json, shared_after_copy_move.json),
    'a link works again once its canvas is restored');
  const restored_links = await getJson(`${first_path}/share`);
  ok(same(link_fields(restored_links.json), [
    { id: share_1, canvas_id: first_id, user_id: TEST_USER_ID },
  ]), 'the restored canvas lists the link it had and not the one refused while trashed');

  await putJson(`${CANVAS_PATH}/trash`, [first_id]);
  const delete_on_trashed = await deleteJson(`${first_path}/share/${share_1}`);
  ok(delete_on_trashed.res.status === 200, `deleting a link on a trashed canvas responds 200 (got ${delete_on_trashed.res.status})`);
  await putJson(`${CANVAS_PATH}/restore`, [first_id]);
  const links_at_end = await getJson(`${first_path}/share`);
  ok(links_at_end.res.status === 200 && same(links_at_end.json, []),
    'the link deleted while the canvas was trashed is gone after restore');
  const graph_at_end = await getJson(`${SHARE_PATH}/${share_1}/graph`);
  ok(graph_at_end.res.status === 404, `the link deleted while the canvas was trashed stays dead (got ${graph_at_end.res.status})`);
  const plain_survives = await getJson(`${CANVAS_PATH}/${plain_id}/graph`);
  ok(plain_survives.res.status === 200 && plain_survives.json.nodes.length === 2,
    'trashing the source and deleting its links leaves the copy intact');
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
