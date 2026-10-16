import { randomUUID } from "node:crypto";
import { createHarness } from "#test/lib/api-harness.mjs";
import { testNode, signNode, testEdge, signEdge } from "#test/lib/api-canvas.mjs";
import { connect_test_db, create_test_user, rejects_with } from "#test/lib/db-harness.mjs";
import { CanvasStorePostgres } from "#store/CanvasStorePostgres.mjs";
import { UserEntityStorePostgres } from "#store/UserEntityStorePostgres.mjs";
import { Graph, make_user_canvas_from_req, CanvasRequestError, CanvasConflictError } from "#model/Canvas.mjs";
import { make_user_node_from_req, make_user_edge_from_req } from "#model/UserEntity.mjs";

const { ok, fail, finish } = createHarness();
const { pool, signing_secret } = await connect_test_db();
const store = new CanvasStorePostgres(pool);
const entities = new UserEntityStorePostgres(pool);

const by_ref = (rows, ref) => rows.find((row) => row.ref === ref);
const by_user_data = (rows, user_data_id) => rows.find((row) => row.user_data_id === user_data_id);

console.log("# CanvasStorePostgres");
try {
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);
  const stamp = randomUUID();
  const ref_a = `DB_TEST:a-${stamp}`;
  const ref_b = `DB_TEST:b-${stamp}`;
  const ref_c = `DB_TEST:c-${stamp}`;
  const ref_missing = `DB_TEST:missing-${stamp}`;
  const graph = (nodes, edges = []) => Graph.from_req({ graph: { nodes: nodes, edges: edges } }, signing_secret);
  const node = (ref, x, y, extra = {}) => signNode(ref, testNode(ref, ref, "biolink:Disease", x, y, extra));
  const edge = (subject, object, extra = {}) =>
    signEdge(`${subject}->${object}`, testEdge(subject, object, "biolink:treats", extra));

  const [u1, u2, u3] = await entities.create_user_nodes([1, 2, 3].map((i) =>
    make_user_node_from_req(user.id, { label: `user node ${i}`, data: {} })));
  const [other_node] = await entities.create_user_nodes([
    make_user_node_from_req(other.id, { label: "other user's node", data: {} })]);
  const [ue_1a, ue_ab, ue_ba] = await entities.create_user_edges([
    make_user_edge_from_req(user.id, { label: "u1 -> a", data: {}, subject_user_node_id: u1.id, object_ref: ref_a }),
    make_user_edge_from_req(user.id, { label: "a -> b", data: {}, subject_ref: ref_a, object_ref: ref_b }),
    make_user_edge_from_req(user.id, { label: "b -> a", data: {}, subject_ref: ref_b, object_ref: ref_a })
  ]);

  const canvas = await store.create_user_canvas(
    make_user_canvas_from_req(user.id, { label: `db-test ${stamp}`, layout: "horizontal" }),
    graph([node(ref_a, 1, 2), node(ref_b, 3, 4)], [edge(ref_a, ref_b)]));
  const read = () => store.get_canvas_graph_by_user(user.id, canvas.id, false);
  const merge = (merge_graph) => store.merge_canvas_graph(user.id, canvas.id, merge_graph, null);

  let g = await read();
  const node_a = by_ref(g.nodes, ref_a);
  const node_b = by_ref(g.nodes, ref_b);
  const edge_ab = by_ref(g.edges, `${ref_a}->${ref_b}`);
  ok(g.nodes.length === 2 && g.edges.length === 1, "create writes the submitted nodes and edges");
  ok(g.nodes[0].id < g.nodes[1].id, "graph rows come back ordered by id");
  ok(g.nodes[0].time_created instanceof Date, "graph timestamps come back as Dates");
  ok(edge_ab.subject_id === node_a.id && edge_ab.object_id === node_b.id, "edge endpoints resolve to canvas node ids");
  ok(await store.get_canvas_graph_by_user(other.id, canvas.id, false) === null, "another user cannot read the canvas");

  await merge(graph([{ x: 5, y: 6, user_data_id: u1.id }], [{ user_data_id: ue_1a.id }]));
  g = await read();
  const user_node_1 = by_user_data(g.nodes, u1.id);
  const user_edge_1a = by_user_data(g.edges, ue_1a.id);
  ok(user_node_1 && user_node_1.data_id === null && user_node_1.label === u1.label && user_node_1.x === 5,
    "a user-only node takes its label from the user node and its placement from the entry");
  ok(user_edge_1a && user_edge_1a.subject_id === user_node_1.id && user_edge_1a.object_id === node_a.id,
    "a user-only edge connects a node added in the same merge to an existing node");

  await merge(graph([node(ref_b, 50, 60, { user_data_id: u2.id })]));
  g = await read();
  ok(by_ref(g.nodes, ref_b).user_data_id === u2.id && by_ref(g.nodes, ref_b).x === 3,
    "merge fills an active node's empty user data slot and keeps its placement");
  await merge(graph([node(ref_b, 50, 60, { user_data_id: u2.id })]));
  ok(by_ref((await read()).nodes, ref_b).user_data_id === u2.id, "merging the same user data again is a no-op");
  ok(await rejects_with(merge(graph([node(ref_b, 1, 1, { user_data_id: u3.id })])), CanvasConflictError),
    "merging different user data onto a filled slot is a conflict");
  ok(await rejects_with(merge(graph([node(ref_a, 1, 1, { user_data_id: u1.id })])), CanvasConflictError),
    "merging user data already on another node is a conflict");
  ok(await rejects_with(merge(graph([node(ref_a, 1, 1, { user_data_id: other_node.id })])), CanvasRequestError),
    "merging another user's user data is a bad request");
  await merge(graph([{ x: 0, y: 0, user_data_id: u2.id }]));
  ok((await read()).nodes.length === 3, "a user-only entry for user data already on a Translator node is a no-op");

  ok(await rejects_with(merge(graph([], [edge(ref_a, ref_b, { user_data_id: ue_ba.id })])), CanvasConflictError),
    "a user edge with different endpoints cannot fill a Translator edge's slot");
  await merge(graph([], [edge(ref_a, ref_b, { user_data_id: ue_ab.id })]));
  ok(by_ref((await read()).edges, `${ref_a}->${ref_b}`).user_data_id === ue_ab.id,
    "a user edge with the same endpoints fills a Translator edge's slot");

  ok(await rejects_with(merge(graph([node(ref_c, 7, 8)], [edge(ref_a, ref_missing)])), CanvasRequestError),
    "an edge to a node that is not on the canvas is a bad request");
  ok(!by_ref((await read()).nodes, ref_c), "a rejected merge writes nothing");
  ok(await store.merge_canvas_graph(other.id, canvas.id, graph([node(ref_c, 7, 8)]), null) === null,
    "another user cannot merge into the canvas");

  const attached = await store.update_canvas_node_by_user(user.id, canvas.id, node_a.id, { user_data_id: u3.id });
  ok(attached && attached.user_data_id === u3.id, "PUT attaches owned user data to a node");
  ok(await store.update_canvas_node_by_user(user.id, canvas.id, user_node_1.id, { user_data_id: null }) === null,
    "PUT cannot detach user data an active user edge connects through");
  ok(await store.update_canvas_node_by_user(user.id, canvas.id, node_b.id, { user_data_id: other_node.id }) === null,
    "PUT cannot attach another user's user data");
  ok(await rejects_with(
    store.update_canvas_node_by_user(user.id, canvas.id, node_b.id, { user_data_id: u3.id }), CanvasConflictError),
    "PUT cannot attach user data already on another node");
  const detached = await store.update_canvas_node_by_user(user.id, canvas.id, node_a.id, { user_data_id: null });
  ok(detached && detached.user_data_id === null, "PUT detaches user data from a node with Translator data");
  ok(await rejects_with(
    store.update_canvas_edge_by_user(user.id, canvas.id, user_edge_1a.id, { user_data_id: null }), CanvasConflictError),
    "PUT cannot detach the only source of an edge");
  await store.update_canvas_edge_by_user(user.id, canvas.id, edge_ab.id, { user_data_id: null });
  ok(await store.update_canvas_edge_by_user(user.id, canvas.id, edge_ab.id, { user_data_id: ue_ba.id }) === null,
    "PUT cannot attach a user edge that does not connect the edge's endpoints");
  const reattached = await store.update_canvas_edge_by_user(user.id, canvas.id, edge_ab.id, { user_data_id: ue_ab.id });
  ok(reattached && reattached.user_data_id === ue_ab.id, "PUT attaches a user edge that connects the edge's endpoints");

  g = await store.trash_canvas_graph_by_user(user.id, canvas.id, [node_a.id], [], []);
  ok(g.edges.length === 0, "trashing a node cascades to its edges");
  g = await store.restore_canvas_graph_by_user(user.id, canvas.id, [], [edge_ab.id, user_edge_1a.id], []);
  ok(g.edges.length === 0, "edges with a trashed endpoint are not restored");
  g = await store.restore_canvas_graph_by_user(user.id, canvas.id, [node_a.id], [edge_ab.id, user_edge_1a.id], []);
  ok(by_ref(g.nodes, ref_a) && g.edges.length === 2, "restoring the endpoint with its edges restores both");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
