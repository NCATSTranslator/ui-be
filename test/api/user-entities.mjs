import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';

const NODES_PATH = '/api/v1/users/me/nodes';
const EDGES_PATH = '/api/v1/users/me/edges';

const { ok, fail, finish } = createHarness();

const listIds = async (path, query = '') => {
  const res = await getJson(`${path}${query}`);
  return new Set((res.json || []).map((entity) => entity.id));
};

console.log(`# ${NODES_PATH} and ${EDGES_PATH}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();

  const created = await postJson(NODES_PATH, {
    label: `api-test user node ${s}`,
    data: { note: 'first', nested: { n: 1 } },
  });
  ok(created.res.status === 200, `create user node responds 200 (got ${created.res.status})`);
  const node = created.json;
  ok(node && Number.isInteger(node.id), 'created user node has an integer id');
  ok(node && node.user_id === TEST_USER_ID, 'created user node is owned by the current user');
  ok(node && node.type === 'Other', `user node type defaults to Other (got ${node && node.type})`);
  ok(node && node.data && node.data.note === 'first' && node.data.nested.n === 1, 'user node data round-trips');
  ok(node && node.time_deleted === null, 'created user node is active');

  const typed = await postJson(NODES_PATH, { label: `api-test typed node ${s}`, type: 'biolink:Gene', data: {} });
  ok(typed.res.status === 200 && typed.json.type === 'biolink:Gene', 'an explicit user node type is kept');

  const fetched = await getJson(`${NODES_PATH}/${node.id}`);
  ok(fetched.res.status === 200 && fetched.json.label === node.label, 'get user node returns it');
  ok((await listIds(NODES_PATH)).has(node.id), 'list user nodes includes it');

  const updated = await putJson(`${NODES_PATH}/${node.id}`, { label: 'renamed', data: { note: 'second' } });
  ok(updated.res.status === 200, `update user node responds 200 (got ${updated.res.status})`);
  ok(updated.json && updated.json.label === 'renamed', 'update changes the label');
  ok(updated.json && updated.json.data.note === 'second' && updated.json.data.nested === undefined,
    'update replaces the data object');
  ok(updated.json && updated.json.type === 'Other', 'update leaves an omitted type unchanged');

  const retyped = await putJson(`${NODES_PATH}/${node.id}`, { type: 'biolink:Disease' });
  ok(retyped.res.status === 200 && retyped.json.type === 'biolink:Disease', 'update can change the type');

  const emptyUpdate = await putJson(`${NODES_PATH}/${node.id}`, {});
  ok(emptyUpdate.res.status === 400, `empty update -> 400 (got ${emptyUpdate.res.status})`);
  const arrayData = await putJson(`${NODES_PATH}/${node.id}`, { data: [1, 2] });
  ok(arrayData.res.status === 400, `array data -> 400 (got ${arrayData.res.status})`);
  const unknownUpdate = await putJson(`${NODES_PATH}/999999999`, { label: 'x' });
  ok(unknownUpdate.res.status === 404, `update of an unknown user node -> 404 (got ${unknownUpdate.res.status})`);
  const badId = await getJson(`${NODES_PATH}/not-a-number`);
  ok(badId.res.status === 400, `non-numeric user node id -> 400 (got ${badId.res.status})`);
  const unknownGet = await getJson(`${NODES_PATH}/999999999`);
  ok(unknownGet.res.status === 404, `get of an unknown user node -> 404 (got ${unknownGet.res.status})`);

  const noLabel = await postJson(NODES_PATH, { data: {} });
  ok(noLabel.res.status === 400, `create without a label -> 400 (got ${noLabel.res.status})`);
  const noData = await postJson(NODES_PATH, { label: 'x' });
  ok(noData.res.status === 400, `create without data -> 400 (got ${noData.res.status})`);
  const emptyType = await postJson(NODES_PATH, { label: 'x', type: '', data: {} });
  ok(emptyType.res.status === 400, `create with an empty type -> 400 (got ${emptyType.res.status})`);

  const trash = await putJson(`${NODES_PATH}/trash`, [node.id]);
  ok(trash.res.status === 200, `trash user node responds 200 (got ${trash.res.status})`);
  const trashedGet = await getJson(`${NODES_PATH}/${node.id}`);
  ok(trashedGet.res.status === 404, `a trashed user node is not found by default (got ${trashedGet.res.status})`);
  const trashedGetAll = await getJson(`${NODES_PATH}/${node.id}?include_deleted=true`);
  ok(trashedGetAll.res.status === 200 && trashedGetAll.json.time_deleted !== null,
    'a trashed user node is returned with include_deleted and carries time_deleted');
  ok(!(await listIds(NODES_PATH)).has(node.id), 'list excludes a trashed user node');
  ok((await listIds(NODES_PATH, '?include_deleted=true')).has(node.id), 'list with include_deleted includes it');
  const trashedUpdate = await putJson(`${NODES_PATH}/${node.id}`, { label: 'x' });
  ok(trashedUpdate.res.status === 404, `a trashed user node cannot be updated (got ${trashedUpdate.res.status})`);

  const restore = await putJson(`${NODES_PATH}/restore`, [node.id]);
  ok(restore.res.status === 200, `restore user node responds 200 (got ${restore.res.status})`);
  const restoredGet = await getJson(`${NODES_PATH}/${node.id}`);
  ok(restoredGet.res.status === 200 && restoredGet.json.time_deleted === null, 'a restored user node is active again');

  const badTrash = await putJson(`${NODES_PATH}/trash`, { ids: [node.id] });
  ok(badTrash.res.status === 400, `trash with a non-array body -> 400 (got ${badTrash.res.status})`);

  const edgeWithType = await postJson(EDGES_PATH,
    { label: 'x', type: 'biolink:treats', data: {}, subject_ref: 'A:1', object_ref: 'B:2' });
  ok(edgeWithType.res.status === 400, `a user edge with a type -> 400 (got ${edgeWithType.res.status})`);
  const unknownNodeField = await postJson(NODES_PATH, { label: 'x', data: {}, user_id: 'someone-else' });
  ok(unknownNodeField.res.status === 400, `a user node with an unknown field -> 400 (got ${unknownNodeField.res.status})`);
  const unknownUpdateField = await putJson(`${NODES_PATH}/${node.id}`, { label: 'x', time_deleted: null });
  ok(unknownUpdateField.res.status === 400, `an update with an unknown field -> 400 (got ${unknownUpdateField.res.status})`);

  const edgeCreated = await postJson(EDGES_PATH, {
    label: `api-test user edge ${s}`,
    data: { weight: 2 },
    subject_user_node_id: node.id,
    object_ref: 'MONDO:0005148',
  });
  ok(edgeCreated.res.status === 200, `create user edge responds 200 (got ${edgeCreated.res.status})`);
  const edge = edgeCreated.json;
  ok(edge && Number.isInteger(edge.id) && edge.user_id === TEST_USER_ID, 'created user edge has an id and owner');
  ok(edge && !('type' in edge), 'user edges have no type');
  ok(edge && edge.data.weight === 2, 'user edge data round-trips');
  ok(edge && edge.subject_user_node_id === node.id && edge.subject_ref === null
    && edge.object_ref === 'MONDO:0005148' && edge.object_user_node_id === null,
    'user edge endpoints round-trip as a user node and a Translator ref');

  const refEdge = await postJson(EDGES_PATH, { label: 'refs', data: {}, subject_ref: 'MONDO:1', object_ref: 'CHEBI:2' });
  ok(refEdge.res.status === 200 && refEdge.json.subject_ref === 'MONDO:1' && refEdge.json.object_ref === 'CHEBI:2',
    'a user edge can connect two Translator refs');
  const noEndpoints = await postJson(EDGES_PATH, { label: 'x', data: {} });
  ok(noEndpoints.res.status === 400, `create a user edge without endpoints -> 400 (got ${noEndpoints.res.status})`);
  const bothForms = await postJson(EDGES_PATH,
    { label: 'x', data: {}, subject_ref: 'A:1', subject_user_node_id: node.id, object_ref: 'B:2' });
  ok(bothForms.res.status === 400, `an endpoint given as both a ref and a user node -> 400 (got ${bothForms.res.status})`);
  const unknownEndpoint = await postJson(EDGES_PATH,
    { label: 'x', data: {}, subject_user_node_id: 999999999, object_ref: 'B:2' });
  ok(unknownEndpoint.res.status === 400, `an unknown user node endpoint -> 400 (got ${unknownEndpoint.res.status})`);
  const trashedEndpointNode = (await postJson(NODES_PATH, { label: `api-test trashed endpoint ${s}`, data: {} })).json;
  await putJson(`${NODES_PATH}/trash`, [trashedEndpointNode.id]);
  const trashedEndpoint = await postJson(EDGES_PATH,
    { label: 'x', data: {}, subject_ref: 'A:1', object_user_node_id: trashedEndpointNode.id });
  ok(trashedEndpoint.res.status === 400, `a trashed user node endpoint -> 400 (got ${trashedEndpoint.res.status})`);

  const edgeUpdated = await putJson(`${EDGES_PATH}/${edge.id}`, { label: 'relabeled' });
  ok(edgeUpdated.res.status === 200 && edgeUpdated.json.label === 'relabeled'
    && edgeUpdated.json.subject_user_node_id === node.id, 'update user edge changes the label and keeps its endpoints');
  const edgeTypeOnly = await putJson(`${EDGES_PATH}/${edge.id}`, { type: 'x' });
  ok(edgeTypeOnly.res.status === 400, `a type-only user edge update -> 400 (got ${edgeTypeOnly.res.status})`);
  const endpointUpdate = await putJson(`${EDGES_PATH}/${edge.id}`, { object_ref: 'MONDO:2' });
  ok(endpointUpdate.res.status === 400, `changing a user edge endpoint -> 400 (got ${endpointUpdate.res.status})`);
  ok((await listIds(EDGES_PATH)).has(edge.id), 'list user edges includes it');

  await putJson(`${EDGES_PATH}/trash`, [edge.id]);
  ok((await getJson(`${EDGES_PATH}/${edge.id}`)).res.status === 404, 'a trashed user edge is not found');
  await putJson(`${EDGES_PATH}/restore`, [edge.id]);
  ok((await getJson(`${EDGES_PATH}/${edge.id}`)).res.status === 200, 'a restored user edge is found');
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
