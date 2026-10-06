import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { postCanvas, testNode, signNode, testEdge, signEdge, CANVAS_PATH } from '../lib/api-canvas.mjs';

const NODES_PATH = '/api/v1/users/me/nodes';
const EDGES_PATH = '/api/v1/users/me/edges';
const SOURCE = { query_ref: 'API_TEST_QID', result_ref: 'API_TEST_RID' };

const { ok, fail, finish } = createHarness();

const byId = (rows, id) => (rows || []).find((row) => row.id === id);
const byUserData = (rows, userDataId) => (rows || []).find((row) => row.user_data_id === userDataId);

console.log(`# user data on canvas entities  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const s = Date.now();
  const refA = `API_TEST:user-data-A-${s}`;
  const refB = `API_TEST:user-data-B-${s}`;
  const refC = `API_TEST:user-data-C-${s}`;
  const eAB = `${refA}->${refB}`;
  const eBC = `${refB}->${refC}`;
  const entryA = signNode(refA, testNode(refA, 'User Data A', 'biolink:Disease', 10, 20));
  const create = await postCanvas({
    label: `api-test user data ${s}`,
    layout: 'custom',
    graph: {
      nodes: [entryA, signNode(refB, testNode(refB, 'User Data B', 'biolink:ChemicalEntity', 30, 40))],
      edges: [signEdge(eAB, testEdge(refA, refB, 'biolink:treats'))],
      tag_descriptions: {},
      source: SOURCE,
    },
  });
  ok(create.res.status === 200, `create canvas responds 200 (got ${create.res.status})`);
  const canvasId = create.json && create.json.id;
  const graphPath = `${CANVAS_PATH}/${canvasId}/graph`;
  const initial = await getJson(graphPath);
  const nodeA = initial.json.nodes.find((n) => n.ref === refA);
  const nodeB = initial.json.nodes.find((n) => n.ref === refB);
  const edgeAB = initial.json.edges.find((e) => e.ref === eAB);
  ok(nodeA && nodeB && edgeAB, 'read back the Translator node and edge ids');

  const makeUserNode = async (label, extra = {}) =>
    (await postJson(NODES_PATH, { label, data: { source: 'api-test' }, ...extra })).json;
  const makeUserEdge = async (label, endpoints) =>
    (await postJson(EDGES_PATH, { label, data: { source: 'api-test' }, ...endpoints })).json;
  const userNodes = {};
  for (const name of ['u1', 'u2', 'u3', 'u4', 'u5', 'u6', 'u7']) {
    userNodes[name] = await makeUserNode(`api-test ${name} ${s}`, name === 'u1' ? { type: 'biolink:Gene' } : {});
  }
  const { u1, u2, u3, u4, u5, u6, u7 } = userNodes;
  const ueAB = await makeUserEdge(`api-test ueAB ${s}`, { subject_ref: refA, object_ref: refB });
  const ueAB2 = await makeUserEdge(`api-test ueAB2 ${s}`, { subject_ref: refA, object_ref: refB });
  const ueBA = await makeUserEdge(`api-test ueBA ${s}`, { subject_ref: refB, object_ref: refA });
  const ueU3B = await makeUserEdge(`api-test ueU3B ${s}`, { subject_user_node_id: u3.id, object_ref: refB });
  const ue7A = await makeUserEdge(`api-test ue7A ${s}`, { subject_user_node_id: u7.id, object_ref: refA });
  const ue12 = await makeUserEdge(`api-test ue12 ${s}`, { subject_user_node_id: u1.id, object_user_node_id: u2.id });
  const ueCA = await makeUserEdge(`api-test ueCA ${s}`, { subject_user_node_id: u5.id, object_ref: refA });
  const ue6A = await makeUserEdge(`api-test ue6A ${s}`, { subject_user_node_id: u6.id, object_ref: refA });
  const ueMissing = await makeUserEdge(`api-test ueMissing ${s}`, { subject_ref: `API_TEST:absent-${s}`, object_ref: refA });
  ok(Object.values(userNodes).every((u) => u && Number.isInteger(u.id))
    && [ueAB, ueAB2, ueBA, ueU3B, ue7A, ue12, ueCA, ue6A, ueMissing].every((ue) => ue && Number.isInteger(ue.id)),
    'created user nodes and edges');

  const merge = (graph) => postJson(graphPath, { nodes: [], edges: [], tag_descriptions: {}, source: SOURCE, ...graph });
  const nodePath = (id) => `${CANVAS_PATH}/${canvasId}/node/${id}`;
  const edgePath = (id) => `${CANVAS_PATH}/${canvasId}/edge/${id}`;

  const attach = await putJson(nodePath(nodeA.id), { user_data_id: u3.id });
  ok(attach.res.status === 200 && attach.json.user_data_id === u3.id && attach.json.data_id === nodeA.data_id,
    'PUT attaches user data to a Translator node and keeps its Translator data');
  ok(attach.json && attach.json.label === nodeA.label && attach.json.type === nodeA.type,
    'attaching user data leaves the label and type alone');
  const attachTwice = await putJson(nodePath(nodeB.id), { user_data_id: u3.id });
  ok(attachTwice.res.status === 409, `the same user data cannot be on two entities (got ${attachTwice.res.status})`);
  const attachUnknown = await putJson(nodePath(nodeB.id), { user_data_id: 999999999 });
  ok(attachUnknown.res.status === 404, `attaching unknown user data -> 404 (got ${attachUnknown.res.status})`);
  const attachUnknownNode = await putJson(nodePath(999999999), { user_data_id: u2.id });
  ok(attachUnknownNode.res.status === 404, `attaching to an unknown canvas node -> 404 (got ${attachUnknownNode.res.status})`);

  const attachEdge = await putJson(edgePath(edgeAB.id), { user_data_id: ueAB.id });
  ok(attachEdge.res.status === 200 && attachEdge.json.user_data_id === ueAB.id,
    'PUT attaches a user edge with matching refs to a Translator edge');
  const detachEdge = await putJson(edgePath(edgeAB.id), { user_data_id: null });
  ok(detachEdge.res.status === 200 && detachEdge.json.user_data_id === null, 'PUT detaches user data from a Translator edge');
  const reversedEdge = await putJson(edgePath(edgeAB.id), { user_data_id: ueBA.id });
  ok(reversedEdge.res.status === 404, `a user edge with reversed endpoints cannot attach (got ${reversedEdge.res.status})`);
  const userNodeEndpoint = await putJson(edgePath(edgeAB.id), { user_data_id: ueU3B.id });
  ok(userNodeEndpoint.res.status === 404,
    `a Translator edge only accepts user edges whose endpoints are its refs (got ${userNodeEndpoint.res.status})`);

  const detach = await putJson(nodePath(nodeA.id), { user_data_id: null });
  ok(detach.res.status === 200 && detach.json.user_data_id === null, 'PUT detaches user data from a Translator node');

  const addUserNodes = await merge({
    nodes: [
      { x: 50, y: 60, user_data_id: u1.id },
      { x: 70, y: 80, user_data_id: u2.id, label: 'custom label', hidden: true },
    ],
  });
  ok(addUserNodes.res.status === 200, `merge user-only nodes responds 200 (got ${addUserNodes.res.status})`);
  const userNode1 = byUserData(addUserNodes.json.nodes, u1.id);
  const userNode2 = byUserData(addUserNodes.json.nodes, u2.id);
  ok(userNode1 && Number.isInteger(userNode1.id) && userNode1.data_id === null && userNode1.ref === null,
    'a user-only node has its own id and no Translator data');
  ok(userNode1 && userNode1.label === u1.label && userNode1.type === 'biolink:Gene'
    && userNode1.x === 50 && userNode1.y === 60 && userNode1.hidden === false,
    'a user-only node takes its label and type from the user data and its placement from the entry');
  ok(userNode2 && userNode2.label === 'custom label' && userNode2.type === 'Other' && userNode2.hidden === true,
    'an entry label and hidden flag override the defaults');

  const remerge = await merge({ nodes: [{ x: 1, y: 1, user_data_id: u1.id }] });
  const remerged = remerge.json && byUserData(remerge.json.nodes, u1.id);
  ok(remerge.res.status === 200 && remerged && remerged.id === userNode1.id && remerged.x === 50,
    'merging an active user-only node again leaves it untouched');

  const userNodeData = await getJson(nodePath(userNode1.id));
  ok(userNodeData.res.status === 404, `a user-only node has no Translator data to read (got ${userNodeData.res.status})`);
  const detachOnly = await putJson(nodePath(userNode1.id), { user_data_id: null });
  ok(detachOnly.res.status === 409, `detaching the only source of a node -> 409 (got ${detachOnly.res.status})`);
  const move = await putJson(`${graphPath}/geometry`, { nodes: [{ id: userNode1.id, x: 5, y: 6 }] });
  ok(move.res.status === 200 && move.json.nodes.length === 1 && move.json.nodes[0].x === 5,
    'a user-only node can be moved');

  const bothSlots = await merge({
    nodes: [{ ...signNode(refC, testNode(refC, 'User Data C', 'biolink:Gene', 90, 90)), user_data_id: u5.id }],
  });
  ok(bothSlots.res.status === 200, `merge a node with both slots responds 200 (got ${bothSlots.res.status})`);
  const nodeC = bothSlots.json && bothSlots.json.nodes.find((n) => n.ref === refC);
  ok(nodeC && Number.isInteger(nodeC.data_id) && nodeC.user_data_id === u5.id && nodeC.label === 'User Data C',
    'a node with both slots carries Translator data, user data, and the Translator label');

  const attachByMerge = await merge({ nodes: [{ ...entryA, user_data_id: u3.id }] });
  ok(attachByMerge.res.status === 200 && byId(attachByMerge.json.nodes, nodeA.id).user_data_id === u3.id,
    'merging user data onto an active entity with an empty slot attaches it');
  const sameByMerge = await merge({ nodes: [{ ...entryA, user_data_id: u3.id }] });
  ok(sameByMerge.res.status === 200, `merging the same user data again is a no-op (got ${sameByMerge.res.status})`);
  const otherByMerge = await merge({ nodes: [{ ...entryA, user_data_id: u6.id }] });
  ok(otherByMerge.res.status === 409, `merging different user data onto a filled slot -> 409 (got ${otherByMerge.res.status})`);
  const takenByMerge = await merge({
    nodes: [{ ...signNode(refB, testNode(refB, 'User Data B', 'biolink:ChemicalEntity', 30, 40)), user_data_id: u1.id }],
  });
  ok(takenByMerge.res.status === 409, `merging user data already on another entity -> 409 (got ${takenByMerge.res.status})`);
  const severalFailures = await merge({
    nodes: [
      { x: 1, y: 2, user_data_id: 999999998 },
      { x: 3, y: 4, user_data_id: u3.id },
    ],
  });
  ok(severalFailures.res.status === 400,
    `a merge failing for several reasons is a 400 (got ${severalFailures.res.status})`);
  const afterConflict = await getJson(graphPath);
  ok(byId(afterConflict.json.nodes, nodeA.id).user_data_id === u3.id && byId(afterConflict.json.nodes, nodeB.id).user_data_id === null,
    'a conflicting merge changes nothing');
  const plainRemerge = await merge({ nodes: [entryA] });
  ok(plainRemerge.res.status === 200 && byId(plainRemerge.json.nodes, nodeA.id).user_data_id === u3.id,
    'merging a Translator node without user data leaves its user data attached');

  const addEdges = await merge({
    nodes: [{ x: 15, y: 25, user_data_id: u7.id }],
    edges: [
      { user_data_id: ue7A.id },
      { user_data_id: ue12.id, label: 'custom edge' },
      { user_data_id: ueCA.id },
      { ...signEdge(eAB, testEdge(refA, refB, 'biolink:treats')), user_data_id: ueAB2.id },
    ],
  });
  ok(addEdges.res.status === 200, `merge user-only and both-slot edges responds 200 (got ${addEdges.res.status})`);
  const userNode7 = byUserData(addEdges.json.nodes, u7.id);
  const userEdge7A = byUserData(addEdges.json.edges, ue7A.id);
  const userEdge12 = byUserData(addEdges.json.edges, ue12.id);
  const userEdgeCA = byUserData(addEdges.json.edges, ueCA.id);
  ok(userEdge7A && userEdge7A.data_id === null && userEdge7A.ref === null && userEdge7A.label === ue7A.label,
    'a user-only edge has no Translator data and takes its label from the user data');
  ok(userEdge7A && userNode7 && userEdge7A.subject_id === userNode7.id && userEdge7A.object_id === nodeA.id,
    'a user-only edge finds a user node endpoint added in the same merge and a Translator ref endpoint');
  ok(userEdge12 && userEdge12.subject_id === userNode1.id && userEdge12.object_id === userNode2.id
    && userEdge12.label === 'custom edge', 'a user-only edge connects two user-only nodes from its endpoints');
  ok(userEdgeCA && nodeC && userEdgeCA.subject_id === nodeC.id && userEdgeCA.object_id === nodeA.id,
    'a user node endpoint matches a Translator node carrying that user data');
  ok(byId(addEdges.json.edges, edgeAB.id).user_data_id === ueAB2.id && byId(addEdges.json.edges, edgeAB.id).data_id === edgeAB.data_id,
    'merging a user edge with matching refs onto an existing Translator edge attaches it');

  const missingEndpoint = await merge({ edges: [{ user_data_id: ueMissing.id }] });
  ok(missingEndpoint.res.status === 400, `a user-only edge whose endpoint is not on the canvas -> 400 (got ${missingEndpoint.res.status})`);
  const mismatchedBoth = await merge({
    nodes: [signNode(refC, testNode(refC, 'User Data C', 'biolink:Gene', 90, 90))],
    edges: [{ ...signEdge(eBC, testEdge(refB, refC, 'biolink:treats')), user_data_id: ueBA.id }],
  });
  ok(mismatchedBoth.res.status === 409,
    `a user edge whose endpoints differ from its Translator edge -> 409 (got ${mismatchedBoth.res.status})`);
  ok(!(await getJson(graphPath)).json.edges.find((e) => e.ref === eBC), 'the mismatched edge was not added');
  const explicitEndpoints = await merge({ edges: [{ user_data_id: ue7A.id, subject: nodeA.id }] });
  ok(explicitEndpoints.res.status === 400, `explicit endpoints on a submitted edge -> 400 (got ${explicitEndpoints.res.status})`);
  const noSlots = await merge({ nodes: [{ x: 1, y: 2 }] });
  ok(noSlots.res.status === 400, `an entry with neither slot -> 400 (got ${noSlots.res.status})`);
  const unknownUserData = await merge({ nodes: [{ x: 1, y: 2, user_data_id: 999999999 }] });
  ok(unknownUserData.res.status === 400, `unknown user data in a merge -> 400 (got ${unknownUserData.res.status})`);
  const duplicate = await merge({
    nodes: [{ x: 1, y: 2, user_data_id: u4.id }, { x: 3, y: 4, user_data_id: u4.id }],
  });
  ok(duplicate.res.status === 400, `the same user data twice in one submission -> 400 (got ${duplicate.res.status})`);
  ok(!byUserData((await getJson(graphPath)).json.nodes, u4.id), 'rejected merges write nothing');

  const detachDependedOn = await putJson(nodePath(nodeC.id), { user_data_id: null });
  ok(detachDependedOn.res.status === 404,
    `detaching user data an active user edge connects through -> 404 (got ${detachDependedOn.res.status})`);
  const replaceDependedOn = await putJson(nodePath(nodeC.id), { user_data_id: u4.id });
  ok(replaceDependedOn.res.status === 404,
    `replacing user data an active user edge connects through -> 404 (got ${replaceDependedOn.res.status})`);
  const sameDependedOn = await putJson(nodePath(nodeC.id), { user_data_id: u5.id });
  ok(sameDependedOn.res.status === 200 && sameDependedOn.json.user_data_id === u5.id,
    `re-sending the same user data is allowed while an edge connects through it (got ${sameDependedOn.res.status})`);
  await putJson(`${graphPath}/trash`, { edges: [userEdgeCA.id] });
  const detachAfterTrash = await putJson(nodePath(nodeC.id), { user_data_id: null });
  ok(detachAfterTrash.res.status === 200, `detaching is allowed once the dependent edge is trashed (got ${detachAfterTrash.res.status})`);
  const staleRestore = await putJson(`${graphPath}/restore`, { edges: [userEdgeCA.id] });
  ok(staleRestore.res.status === 200 && !byId(staleRestore.json.edges, userEdgeCA.id),
    'an edge whose user edge no longer matches its endpoints is not restored');
  await putJson(nodePath(nodeC.id), { user_data_id: u5.id });
  const matchingRestore = await putJson(`${graphPath}/restore`, { edges: [userEdgeCA.id] });
  ok(matchingRestore.res.status === 200 && byId(matchingRestore.json.edges, userEdgeCA.id),
    'the edge is restored once its endpoints match its user edge again');

  const trashUserNode = await putJson(`${graphPath}/trash`, { nodes: [userNode1.id] });
  ok(trashUserNode.res.status === 200 && !byId(trashUserNode.json.nodes, userNode1.id)
    && !byId(trashUserNode.json.edges, userEdge12.id), 'trashing a user-only node cascades to its edges');
  const revive = await merge({
    nodes: [{ x: 2, y: 3, user_data_id: u1.id }],
    edges: [{ user_data_id: ue12.id }],
  });
  ok(revive.res.status === 200, `merging trashed user-only entities again responds 200 (got ${revive.res.status})`);
  const revivedNode = byUserData(revive.json.nodes, u1.id);
  const revivedEdge = byUserData(revive.json.edges, ue12.id);
  ok(revivedNode && revivedNode.id === userNode1.id && revivedNode.x === 2,
    'a trashed user-only node is revived in place with the new placement');
  ok(revivedEdge && revivedEdge.id === userEdge12.id, 'a trashed user-only edge is revived in place');

  await putJson(`${NODES_PATH}/trash`, [u3.id]);
  ok(byId((await getJson(graphPath)).json.nodes, nodeA.id).user_data_id === u3.id,
    'trashing user data keeps the link on the canvas entity');
  ok((await getJson(`${NODES_PATH}/${u3.id}`)).res.status === 404, 'trashed user data reads as missing');
  const attachTrashed = await putJson(nodePath(nodeB.id), { user_data_id: u3.id });
  ok(attachTrashed.res.status === 404, `trashed user data cannot be attached (got ${attachTrashed.res.status})`);
  await putJson(`${NODES_PATH}/trash`, [u4.id]);
  const mergeTrashed = await merge({ nodes: [{ x: 0, y: 0, user_data_id: u4.id }] });
  ok(mergeTrashed.res.status === 400, `merging trashed user data -> 400 (got ${mergeTrashed.res.status})`);
  await putJson(`${NODES_PATH}/restore`, [u3.id, u4.id]);
  ok((await getJson(`${NODES_PATH}/${u3.id}`)).res.status === 200, 'restoring user data brings it back');

  const created = await postCanvas({
    label: `api-test user data create ${s}`,
    layout: 'custom',
    graph: {
      nodes: [
        { ...entryA, user_data_id: u4.id },
        { x: 5, y: 5, user_data_id: u6.id },
      ],
      edges: [{ user_data_id: ue6A.id }],
      tag_descriptions: {},
      source: SOURCE,
    },
  });
  ok(created.res.status === 200, `create a canvas with user data slots responds 200 (got ${created.res.status})`);
  const createdGraph = await getJson(`${CANVAS_PATH}/${created.json.id}/graph`);
  const createdA = createdGraph.json.nodes.find((n) => n.ref === refA);
  const createdNote = byUserData(createdGraph.json.nodes, u6.id);
  const createdLink = byUserData(createdGraph.json.edges, ue6A.id);
  ok(createdA && createdA.user_data_id === u4.id, 'canvas create fills the user data slot of a Translator node');
  ok(createdNote && createdNote.data_id === null, 'canvas create adds a user-only node');
  ok(createdLink && createdNote && createdA && createdLink.subject_id === createdNote.id && createdLink.object_id === createdA.id,
    'canvas create connects a user-only edge from its user edge endpoints');

  const unknownCanvas = await postJson(`${CANVAS_PATH}/999999999/graph`,
    { nodes: [{ x: 0, y: 0, user_data_id: u2.id }], edges: [], tag_descriptions: {}, source: SOURCE });
  ok(unknownCanvas.res.status === 404, `merging into an unknown canvas -> 404 (got ${unknownCanvas.res.status})`);
  await putJson(`${CANVAS_PATH}/trash`, [canvasId]);
  const onTrashedCanvas = await merge({ nodes: [{ x: 0, y: 0, user_data_id: u2.id }] });
  ok(onTrashedCanvas.res.status === 404, `merging into a trashed canvas -> 404 (got ${onTrashedCanvas.res.status})`);
  const attachOnTrashedCanvas = await putJson(nodePath(nodeB.id), { user_data_id: u2.id });
  ok(attachOnTrashedCanvas.res.status === 404, `attaching on a trashed canvas -> 404 (got ${attachOnTrashedCanvas.res.status})`);
} catch (err) {
  fail(`request failed: ${err.message} -- is the server running with auth_check=false?`);
}

finish();
