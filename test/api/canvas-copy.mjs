import { createHarness, postJson, putJson, getJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, signNode, signEdge, testNode, testEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const NODES_PATH = '/api/v1/users/me/nodes';
const EDGES_PATH = '/api/v1/users/me/edges';

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

function same(actual, expected) {
  return JSON.stringify(actual) === JSON.stringify(expected);
}

console.log(`# POST ${CANVAS_PATH}/:canvas_id/copy  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const stamp = Date.now();
  const ref_a = `API_TEST:copy-A-${stamp}`;
  const ref_b = `API_TEST:copy-B-${stamp}`;
  const ref_c = `API_TEST:copy-C-${stamp}`;
  const ref_ab = `${ref_a}->${ref_b}`;
  const ref_bc = `${ref_b}->${ref_c}`;

  const create = await postCanvas({
    label: `api-test copy source ${stamp}`,
    layout: 'vertical',
    graph: {
      nodes: [
        signNode(ref_a, testNode(ref_a, 'Copy A', 'biolink:Disease', 10, 20, { label: 'Alpha' })),
        signNode(ref_b, testNode(ref_b, 'Copy B', 'biolink:Gene', 30, 40, { label: 'Beta', hidden: true })),
        signNode(ref_c, testNode(ref_c, 'Copy C', 'biolink:Disease', 50, 60, { label: 'Gamma' })),
      ],
      edges: [
        signEdge(ref_ab, testEdge(ref_a, ref_b, 'biolink:treats', { label: 'alpha to beta' })),
        signEdge(ref_bc, testEdge(ref_b, ref_c, 'biolink:treats', { label: 'beta to gamma' })),
      ],
      tag_descriptions: {},
      source: { query_ref: 'API_TEST_COPY_QID', result_ref: 'API_TEST_COPY_RID' },
    },
  });
  ok(create.res.status === 200, `create source canvas responds 200 (got ${create.res.status})`);
  const source_id = create.json.id;
  const source_path = `${CANVAS_PATH}/${source_id}`;

  const kept_note = await postJson(`${source_path}/annotation`,
    { content: 'kept note', x: 1, y: 2, width: 30, height: 40 });
  ok(kept_note.res.status === 200, `create kept annotation responds 200 (got ${kept_note.res.status})`);
  const trashed_note = await postJson(`${source_path}/annotation`,
    { content: 'trashed note', x: 5, y: 6, width: 70, height: 80 });
  ok(trashed_note.res.status === 200, `create trashed annotation responds 200 (got ${trashed_note.res.status})`);

  const source_before = await getJson(`${source_path}/graph`);
  const source_node_id = new Map(source_before.json.nodes.map((node) => [node.ref, node.id]));
  const trash = await putJson(`${source_path}/graph/trash`,
    { nodes: [source_node_id.get(ref_c)], annotations: [trashed_note.json.id] });
  ok(trash.res.status === 200, `trash node C and one annotation on the source responds 200 (got ${trash.res.status})`);

  const plain = await postJson(`${source_path}/copy`, {});
  ok(plain.res.status === 200, `copy with an empty body responds 200 (got ${plain.res.status})`);
  const annotated = await postJson(`${source_path}/copy`,
    { label: `api-test copy renamed ${stamp}`, include_annotations: true });
  ok(annotated.res.status === 200, `copy with a label and annotations responds 200 (got ${annotated.res.status})`);
  const plain_id = plain.json.id;
  const annotated_id = annotated.json.id;
  ok(new Set([source_id, plain_id, annotated_id]).size === 3, 'the source and both copies have distinct ids');

  const list = await getJson(CANVAS_PATH);
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  const listed_plain = list.json.find((canvas) => canvas.id === plain_id);
  const listed_annotated = list.json.find((canvas) => canvas.id === annotated_id);
  ok(listed_plain && listed_plain.label === `api-test copy source ${stamp}`,
    'a copy made without a label keeps the source label');
  ok(listed_annotated && listed_annotated.label === `api-test copy renamed ${stamp}`,
    'a copy made with a label takes that label');
  ok(listed_plain && listed_plain.layout === 'vertical' && listed_annotated && listed_annotated.layout === 'vertical',
    'both copies keep the source layout');
  ok(listed_plain && listed_plain.data.query_ref === 'API_TEST_COPY_QID'
    && listed_plain.data.result_ref === 'API_TEST_COPY_RID',
    'the copy keeps the source query and result refs');
  ok(listed_plain && listed_plain.user_id === TEST_USER_ID && listed_plain.time_deleted === null,
    'the copy is an active canvas of the same user');

  const plain_graph = await getJson(`${CANVAS_PATH}/${plain_id}/graph?include_deleted=true`);
  const annotated_graph = await getJson(`${CANVAS_PATH}/${annotated_id}/graph?include_deleted=true`);
  ok(plain_graph.res.status === 200, `plain copy graph responds 200 (got ${plain_graph.res.status})`);
  ok(annotated_graph.res.status === 200, `annotated copy graph responds 200 (got ${annotated_graph.res.status})`);

  for (const [name, graph] of [['plain', plain_graph.json], ['annotated', annotated_graph.json]]) {
    ok(same(node_fields(graph.nodes), [
      { ref: ref_a, label: 'Alpha', type: 'Disease', x: 10, y: 20, hidden: false },
      { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
    ]), `the ${name} copy has the active source nodes and not the trashed one`);
    const node_id = new Map(graph.nodes.map((node) => [node.ref, node.id]));
    ok(graph.edges.length === 1 && graph.edges[0].ref === ref_ab && graph.edges[0].label === 'alpha to beta',
      `the ${name} copy has the active source edge and not the trashed one`);
    ok(graph.edges.length === 1
      && graph.edges[0].subject_id === node_id.get(ref_a) && graph.edges[0].object_id === node_id.get(ref_b),
      `the ${name} copy's edge connects the copy's own nodes`);
    ok(node_id.get(ref_a) !== source_node_id.get(ref_a) && node_id.get(ref_b) !== source_node_id.get(ref_b),
      `the ${name} copy's nodes are new rows, not the source's`);
  }

  ok(same(plain_graph.json.annotations, []), 'a copy made without include_annotations has no annotations');
  ok(same(annotation_fields(annotated_graph.json.annotations),
    [{ content: 'kept note', x: 1, y: 2, width: 30, height: 40 }]),
    'a copy made with include_annotations has the active source annotation and not the trashed one');
  ok(annotated_graph.json.annotations.length === 1 && annotated_graph.json.annotations[0].id !== kept_note.json.id,
    "the copy's annotation is a new row, not the source's");

  const plain_node_a = plain_graph.json.nodes.find((node) => node.ref === ref_a);
  const node_data = await getJson(`${CANVAS_PATH}/${plain_id}/node/${plain_node_a.id}`);
  ok(node_data.res.status === 200 && node_data.json.id === ref_a && same(node_data.json.names, ['Copy A']),
    "the copy's node resolves to the same Translator data");

  const move = await putJson(`${CANVAS_PATH}/${plain_id}/graph/geometry`,
    { nodes: [{ id: plain_node_a.id, x: 111, y: 222 }] });
  ok(move.res.status === 200, `moving a node on the plain copy responds 200 (got ${move.res.status})`);
  const edit = await putJson(
    `${CANVAS_PATH}/${annotated_id}/annotation/${annotated_graph.json.annotations[0].id}`, { content: 'copy note' });
  ok(edit.res.status === 200, `editing the annotated copy's annotation responds 200 (got ${edit.res.status})`);

  const source_after = await getJson(`${source_path}/graph`);
  const annotated_after = await getJson(`${CANVAS_PATH}/${annotated_id}/graph`);
  const plain_after = await getJson(`${CANVAS_PATH}/${plain_id}/graph`);
  ok(same(node_fields(plain_after.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 111, y: 222, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'the move took effect on the plain copy');
  ok(same(node_fields(source_after.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 10, y: 20, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'moving a node on a copy leaves the source untouched');
  ok(same(node_fields(annotated_after.json.nodes), [
    { ref: ref_a, label: 'Alpha', type: 'Disease', x: 10, y: 20, hidden: false },
    { ref: ref_b, label: 'Beta', type: 'Gene', x: 30, y: 40, hidden: true },
  ]), 'moving a node on one copy leaves the other copy untouched');
  ok(same(annotation_fields(annotated_after.json.annotations),
    [{ content: 'copy note', x: 1, y: 2, width: 30, height: 40 }]),
    'the annotation edit took effect on the annotated copy');
  ok(same(annotation_fields(source_after.json.annotations),
    [{ content: 'kept note', x: 1, y: 2, width: 30, height: 40 }]),
    "editing a copy's annotation leaves the source annotation untouched");

  const count_before = (await getJson(`${CANVAS_PATH}?include_deleted=true`)).json.length;
  const bad_label = await postJson(`${source_path}/copy`, { label: 42 });
  ok(bad_label.res.status === 400, `non-string label -> 400 (got ${bad_label.res.status})`);
  const bad_flag = await postJson(`${source_path}/copy`, { include_annotations: 'yes' });
  ok(bad_flag.res.status === 400, `non-boolean include_annotations -> 400 (got ${bad_flag.res.status})`);
  const no_body = await postJson(`${source_path}/copy`);
  ok(no_body.res.status === 400, `copy without a body -> 400 (got ${no_body.res.status})`);
  const bad_id = await postJson(`${CANVAS_PATH}/not-a-number/copy`, {});
  ok(bad_id.res.status === 400, `non-numeric canvas id -> 400 (got ${bad_id.res.status})`);
  const unknown = await postJson(`${CANVAS_PATH}/999999999/copy`, {});
  ok(unknown.res.status === 404, `unknown canvas id -> 404 (got ${unknown.res.status})`);

  const trash_source = await putJson(`${CANVAS_PATH}/trash`, [source_id]);
  ok(trash_source.res.status === 200, `trash source canvas responds 200 (got ${trash_source.res.status})`);
  const copy_trashed = await postJson(`${source_path}/copy`, {});
  ok(copy_trashed.res.status === 404, `copying a trashed canvas -> 404 (got ${copy_trashed.res.status})`);
  const count_after = (await getJson(`${CANVAS_PATH}?include_deleted=true`)).json.length;
  ok(count_after === count_before, 'the rejected copies created no canvas');
  const plain_survives = await getJson(`${CANVAS_PATH}/${plain_id}/graph`);
  ok(plain_survives.res.status === 200 && plain_survives.json.nodes.length === 2,
    'trashing the source leaves its copy intact');

  const ref_d = `API_TEST:copy-D-${stamp}`;
  const ref_e = `API_TEST:copy-E-${stamp}`;
  const ref_de = `${ref_d}->${ref_e}`;
  const ref_ed = `${ref_e}->${ref_d}`;
  const note = await postJson(NODES_PATH, { label: `api-test copy note ${stamp}`, data: { text: 'note' } });
  const attached = await postJson(NODES_PATH, { label: `api-test copy attached ${stamp}`, data: { text: 'attached' } });
  const note_link = await postJson(EDGES_PATH,
    { label: `api-test copy note link ${stamp}`, data: {}, subject_user_node_id: note.json.id, object_ref: ref_d });
  const attached_link = await postJson(EDGES_PATH,
    { label: `api-test copy attached link ${stamp}`, data: {}, subject_ref: ref_d, object_ref: ref_e });
  ok([note, attached, note_link, attached_link].every((created) => created.res.status === 200),
    'create the user nodes and user edges for the user data source responds 200');
  const user_source = await postCanvas({
    label: `api-test copy user data ${stamp}`,
    layout: 'custom',
    graph: {
      nodes: [
        {
          ...signNode(ref_d, testNode(ref_d, 'Copy D', 'biolink:Disease', 1, 2,
            { tags: { 'r/cc/drug': { id: 'r/cc/drug', description: { name: 'Drug', description: '' } } } })),
          user_data_id: attached.json.id,
        },
        signNode(ref_e, testNode(ref_e, 'Copy E', 'biolink:Gene', 3, 4)),
        { x: 5, y: 6, user_data_id: note.json.id },
      ],
      edges: [
        {
          ...signEdge(ref_de, testEdge(ref_d, ref_e, 'biolink:treats', { hidden: true })),
          user_data_id: attached_link.json.id,
        },
        signEdge(ref_ed, testEdge(ref_e, ref_d, 'biolink:treats')),
        { user_data_id: note_link.json.id },
      ],
      tag_descriptions: { 'r/cc/drug': { id: 'r/cc/drug', description: { name: 'Drug', description: '' } } },
      source: { query_ref: 'API_TEST_COPY_QID', result_ref: 'API_TEST_COPY_RID' },
    },
  });
  ok(user_source.res.status === 200, `create a source canvas with user data responds 200 (got ${user_source.res.status})`);
  const user_source_path = `${CANVAS_PATH}/${user_source.json.id}`;
  const user_source_graph = await getJson(`${user_source_path}/graph`);
  const user_source_d = user_source_graph.json.nodes.find((node) => node.ref === ref_d);
  const user_source_note = user_source_graph.json.nodes.find((node) => node.user_data_id === note.json.id);
  const trash_edge = await putJson(`${user_source_path}/graph/trash`,
    { edges: [user_source_graph.json.edges.find((edge) => edge.ref === ref_ed).id] });
  ok(trash_edge.res.status === 200, `trash one edge between two active nodes responds 200 (got ${trash_edge.res.status})`);

  const user_nodes_before = (await getJson(`${NODES_PATH}?include_deleted=true`)).json.length;
  const user_edges_before = (await getJson(`${EDGES_PATH}?include_deleted=true`)).json.length;
  const user_copy = await postJson(`${user_source_path}/copy`, {});
  ok(user_copy.res.status === 200, `copy a canvas with user data responds 200 (got ${user_copy.res.status})`);
  const user_nodes_after = (await getJson(`${NODES_PATH}?include_deleted=true`)).json.length;
  const user_edges_after = (await getJson(`${EDGES_PATH}?include_deleted=true`)).json.length;
  ok(user_nodes_after === user_nodes_before && user_edges_after === user_edges_before,
    'copying your own canvas creates no user nodes or user edges');

  const user_copy_graph = await getJson(`${CANVAS_PATH}/${user_copy.json.id}/graph?include_deleted=true`);
  ok(user_copy_graph.res.status === 200, `user data copy graph responds 200 (got ${user_copy_graph.res.status})`);
  const copy_d = user_copy_graph.json.nodes.find((node) => node.ref === ref_d);
  const copy_e = user_copy_graph.json.nodes.find((node) => node.ref === ref_e);
  const copy_note = user_copy_graph.json.nodes.find((node) => node.user_data_id === note.json.id);
  ok(user_copy_graph.json.nodes.length === 3 && copy_d && copy_e && copy_note, 'the copy has all three source nodes');
  ok(copy_d && copy_d.user_data_id === attached.json.id && copy_d.data_id === user_source_d.data_id
    && copy_d.id !== user_source_d.id,
    'a copied Translator node keeps its Translator data and its attached user data as a new row');
  ok(copy_d && same(copy_d.tags, { 'r/cc/drug': null }), 'a copied node keeps its tags');
  ok(copy_e && copy_e.user_data_id === null, 'a copied node without user data stays without user data');
  ok(copy_note && copy_note.ref === null && copy_note.data_id === null
    && copy_note.label === `api-test copy note ${stamp}` && copy_note.type === 'Other'
    && copy_note.x === 5 && copy_note.y === 6 && copy_note.id !== user_source_note.id,
    'a user-only node is copied as a new row with its user data, label, type and placement');
  ok(same(user_copy_graph.json.tags, { 'r/cc/drug': { id: 'r/cc/drug', description: { name: 'Drug', description: '' } } }),
    'the copy keeps the source tag descriptions');
  const copy_de = user_copy_graph.json.edges.find((edge) => edge.ref === ref_de);
  const copy_note_link = user_copy_graph.json.edges.find((edge) => edge.user_data_id === note_link.json.id);
  ok(user_copy_graph.json.edges.length === 2 && !user_copy_graph.json.edges.find((edge) => edge.ref === ref_ed),
    'an edge trashed on its own is not copied');
  ok(copy_de && copy_de.hidden === true && copy_de.user_data_id === attached_link.json.id
    && copy_de.subject_id === copy_d.id && copy_de.object_id === copy_e.id,
    "a copied Translator edge keeps its hidden flag and user data and connects the copy's own nodes");
  ok(copy_note_link && copy_note_link.ref === null && copy_note_link.data_id === null
    && copy_note_link.label === `api-test copy note link ${stamp}`
    && copy_note_link.subject_id === copy_note.id && copy_note_link.object_id === copy_d.id,
    "a user-only edge is copied and connects the copy's own user-only node to the copy's own Translator node");
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
