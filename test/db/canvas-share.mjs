import { randomUUID } from "node:crypto";
import { createHarness } from "#test/lib/api-harness.mjs";
import { testNode, signNode, testEdge, signEdge } from "#test/lib/api-canvas.mjs";
import { connect_test_db, create_test_user } from "#test/lib/db-harness.mjs";
import { CanvasStorePostgres } from "#store/CanvasStorePostgres.mjs";
import { UserEntityStorePostgres } from "#store/UserEntityStorePostgres.mjs";
import { UserService } from "../../services/UserService.mjs";
import { Graph, CanvasShare, make_user_canvas_from_req } from "#model/Canvas.mjs";
import { make_user_node_from_req, make_user_edge_from_req } from "#model/UserEntity.mjs";

const { ok, fail, finish } = createHarness();
const { pool, signing_secret } = await connect_test_db();
const store = new CanvasStorePostgres(pool);
const entities = new UserEntityStorePostgres(pool);
const service = new UserService(null, null, null, store, null, entities, null, null, signing_secret, null);

const same = (actual, expected) => JSON.stringify(actual) === JSON.stringify(expected);
const by_ref = (rows, ref) => rows.find((row) => row.ref === ref);
const by_label = (rows, label) => rows.find((row) => row.label === label);
const sorted_by = (rows, field) => [...rows].sort((a, b) => (a[field] < b[field] ? -1 : a[field] > b[field] ? 1 : 0));
const ids = (rows) => rows.map((row) => row.id);
const ascending = (values) => [...values].sort((a, b) => a - b);

console.log("# CanvasStorePostgres share links and cross-user copy");
try {
  const owner = await create_test_user(pool);
  const other = await create_test_user(pool);
  const stamp = randomUUID();
  const ref_a = `DB_TEST:share-a-${stamp}`;
  const ref_b = `DB_TEST:share-b-${stamp}`;
  const ref_ab = `${ref_a}->${ref_b}`;
  const graph = (nodes, edges = []) => Graph.from_req({ graph: { nodes: nodes, edges: edges } }, signing_secret);
  const node = (ref, x, y, extra = {}) => signNode(ref, testNode(ref, ref, "biolink:Disease", x, y, extra));
  const edge = (subject, object, extra = {}) =>
    signEdge(`${subject}->${object}`, testEdge(subject, object, "biolink:treats", extra));

  const [u1, u2, u3, u4] = await entities.create_user_nodes([
    make_user_node_from_req(owner.id, { label: "note one", type: "biolink:Gene", data: { text: "first" } }),
    make_user_node_from_req(owner.id, { label: "note two", data: { text: "second" } }),
    make_user_node_from_req(owner.id, { label: "note three", data: { text: "third" } }),
    make_user_node_from_req(owner.id, { label: "note four", data: { text: "fourth" } })
  ]);
  const [ue_1a, ue_13, ue_ab] = await entities.create_user_edges([
    make_user_edge_from_req(owner.id,
      { label: "one to a", data: { weight: 1 }, subject_user_node_id: u1.id, object_ref: ref_a }),
    make_user_edge_from_req(owner.id,
      { label: "one to three", data: { weight: 2 }, subject_user_node_id: u1.id, object_user_node_id: u3.id }),
    make_user_edge_from_req(owner.id,
      { label: "a to b", data: { weight: 3 }, subject_ref: ref_a, object_ref: ref_b })
  ]);

  const canvas = await store.create_user_canvas(
    make_user_canvas_from_req(owner.id, { label: `db-test share ${stamp}`, layout: "horizontal" }),
    graph([
      node(ref_a, 1, 2, { user_data_id: u2.id }),
      node(ref_b, 3, 4),
      { x: 5, y: 6, user_data_id: u1.id },
      { x: 7, y: 8, user_data_id: u3.id },
      { x: 9, y: 10, user_data_id: u4.id }
    ], [
      edge(ref_a, ref_b, { user_data_id: ue_ab.id }),
      { user_data_id: ue_1a.id },
      { user_data_id: ue_13.id }
    ]));
  const second = await store.create_user_canvas(
    make_user_canvas_from_req(owner.id, { label: `db-test share second ${stamp}`, layout: "vertical" }));
  const other_canvas = await store.create_user_canvas(
    make_user_canvas_from_req(other.id, { label: `db-test share other ${stamp}`, layout: "vertical" }));
  await store.create_canvas_annotation(owner.id, canvas.id, { content: "kept note", x: 1, y: 2, width: 30, height: 40 });
  const built = await store.get_canvas_graph_by_user(owner.id, canvas.id, false);
  ok(built.nodes.length === 5 && built.edges.length === 3 && built.annotations.length === 1,
    "the source canvas is built with Translator and user-data nodes and edges and an annotation");
  await store.trash_canvas_graph_by_user(owner.id, canvas.id, [by_label(built.nodes, "note four").id], [], []);

  ok(same(await store.get_canvas_shares_by_user(owner.id, canvas.id), []), "a canvas with no links lists none");
  ok(same(await store.create_canvas_shares([]), []), "creating no links writes nothing");
  ok(same(await store.get_canvas_shares([]), []), "resolving no link ids returns nothing");

  const share_1 = new CanvasShare({ canvas_id: canvas.id, user_id: owner.id });
  const share_2 = new CanvasShare({ canvas_id: canvas.id, user_id: owner.id });
  const share_second = new CanvasShare({ canvas_id: second.id, user_id: owner.id });
  const created = await store.create_canvas_shares([share_1]);
  ok(created.length === 1 && created[0].id === share_1.id && created[0].canvas_id === canvas.id
    && created[0].user_id === owner.id, "creating a link returns the stored row");
  const created_batch = await store.create_canvas_shares([share_2, share_second]);
  ok(same(ids(created_batch).sort(), [share_2.id, share_second.id].sort()),
    "one batch creates links on different canvases");

  const listed = await store.get_canvas_shares_by_user(owner.id, canvas.id);
  ok(same(ids(listed), [share_1.id, share_2.id]), "a canvas lists its own links, oldest first");
  ok(listed.every((row) => row.canvas_id === canvas.id && row.user_id === owner.id),
    "listed links carry their canvas and creator");
  ok(listed[0].time_created instanceof Date, "listed link timestamps come back as Dates");
  ok(same(ids(await store.get_canvas_shares_by_user(owner.id, second.id)), [share_second.id]),
    "the second canvas lists only its own link");

  const resolved = sorted_by(await store.get_canvas_shares([share_1.id, share_second.id]), "canvas_id");
  ok(same(resolved.map((row) => ({ id: row.id, canvas_id: row.canvas_id, user_id: row.user_id })),
    sorted_by([
      { id: share_1.id, canvas_id: canvas.id, user_id: owner.id },
      { id: share_second.id, canvas_id: second.id, user_id: owner.id }
    ], "canvas_id")), "link ids resolve to their own canvas and creator");
  ok(same(await store.get_canvas_shares([randomUUID()]), []), "an unknown link id resolves to nothing");

  ok(same(await store.create_canvas_shares([new CanvasShare({ canvas_id: canvas.id, user_id: other.id })]), []),
    "a user cannot create a link on a canvas they do not own");
  const other_share = new CanvasShare({ canvas_id: other_canvas.id, user_id: other.id });
  const mixed = await store.create_canvas_shares([
    other_share, new CanvasShare({ canvas_id: second.id, user_id: other.id })]);
  ok(same(ids(mixed), [other_share.id]), "a batch creates only the links on canvases the user owns");
  ok(same(ids(await store.get_canvas_shares_by_user(owner.id, canvas.id)), [share_1.id, share_2.id])
    && same(ids(await store.get_canvas_shares_by_user(owner.id, second.id)), [share_second.id]),
    "refused links are not stored");
  ok(same(ids(await store.get_canvas_shares_by_user(other.id, other_canvas.id)), [other_share.id]),
    "the link the other user was allowed to create is stored");
  ok(await store.get_canvas_shares_by_user(other.id, canvas.id) === null,
    "another user cannot list a canvas's links");
  ok(await store.get_canvas_shares_by_user(owner.id, Number.MAX_SAFE_INTEGER) === null,
    "an unknown canvas has no link list");

  ok(same(await store.delete_canvas_shares_by_user(other.id, canvas.id, [share_1.id]), []),
    "another user cannot delete a link");
  ok(same(await store.delete_canvas_shares_by_user(owner.id, second.id, [share_1.id]), []),
    "a link is not deleted through another canvas");
  ok(same(await store.delete_canvas_shares_by_user(owner.id, canvas.id, []), []), "deleting no links removes nothing");
  ok(same(ids(await store.get_canvas_shares([share_1.id])), [share_1.id]), "refused deletes leave the link in place");
  ok(same(await store.delete_canvas_shares_by_user(owner.id, canvas.id, [share_2.id, randomUUID()]), [share_2.id]),
    "delete returns only the ids it removed");
  ok(same(ids(await store.get_canvas_shares_by_user(owner.id, canvas.id)), [share_1.id])
    && same(await store.get_canvas_shares([share_2.id]), []),
    "the deleted link is gone and the canvas's other link stays");

  const shared_graph = await service.get_shared_canvas_graph(share_1.id);
  ok(shared_graph !== null && shared_graph.nodes.length === 4 && shared_graph.edges.length === 3
    && shared_graph.annotations.length === 1, "a link reads the canvas's active graph without a user");
  ok(await service.get_shared_canvas_graph(share_2.id) === null, "a deleted link reads nothing");
  ok(await service.get_shared_canvas_graph(randomUUID()) === null, "an unknown link reads nothing");
  const source_node_a = by_ref(shared_graph.nodes, ref_a);
  const source_edge_ab = by_ref(shared_graph.edges, ref_ab);
  ok(await service.get_shared_node_data(share_1.id, source_node_a.id) !== null
    && await service.get_shared_edge_data(share_1.id, source_edge_ab.id) !== null,
    "a link reads node and edge data on its canvas");
  ok(await service.get_shared_node_data(other_share.id, source_node_a.id) === null
    && await service.get_shared_edge_data(other_share.id, source_edge_ab.id) === null,
    "a link does not read node or edge data on another canvas");

  ok(await store.copy_canvas_by_user(other.id, canvas.id, other.id, null, false) === null,
    "a user cannot copy a canvas they do not own without a link");
  ok(await service.copy_shared_canvas(other.id, randomUUID(), {}) === null, "an unknown link copies nothing");
  ok(await service.copy_shared_canvas(other.id, share_2.id, {}) === null, "a deleted link copies nothing");
  ok(same(ids(await store.get_canvases_by_user(other.id, true)), [other_canvas.id]),
    "the refused copies created no canvas for the other user");
  ok(same(await entities.get_user_nodes(other.id, true), []), "the refused copies created no user data");

  const copied = await service.copy_shared_canvas(other.id, share_1.id,
    { label: "copied by other", include_annotations: true });
  ok(copied !== null && copied.id !== canvas.id && copied.user_id === other.id && copied.label === "copied by other"
    && copied.layout === "horizontal", "copying through a link returns a new canvas owned by the copying user");
  ok(same(ids(await store.get_canvases_by_user(other.id, false)).sort(), [other_canvas.id, copied.id].sort()),
    "the copy is listed for the copying user");
  ok(same(ids(await store.get_canvases_by_user(owner.id, false)).sort(), [canvas.id, second.id].sort()),
    "the copy is not listed for the source owner");
  ok(await store.get_canvas_graph_by_user(owner.id, copied.id, false) === null,
    "the source owner cannot read the copy");

  const copy_graph = await store.get_canvas_graph_by_user(other.id, copied.id, true);
  const source_graph = await store.get_canvas_graph_by_user(owner.id, canvas.id, false);
  ok(same(sorted_by(copy_graph.nodes, "x").map((row) =>
    ({ ref: row.ref, label: row.label, type: row.type, x: row.x, y: row.y })), [
    { ref: ref_a, label: ref_a, type: "Disease", x: 1, y: 2 },
    { ref: ref_b, label: ref_b, type: "Disease", x: 3, y: 4 },
    { ref: null, label: "note one", type: "biolink:Gene", x: 5, y: 6 },
    { ref: null, label: "note three", type: "Other", x: 7, y: 8 }
  ]), "the copy has the source's active nodes and not the trashed one");
  ok(same(copy_graph.annotations.map((row) =>
    ({ content: row.content, x: row.x, y: row.y, width: row.width, height: row.height })),
    [{ content: "kept note", x: 1, y: 2, width: 30, height: 40 }]), "the copy has the source's annotation");
  const copy_a = by_ref(copy_graph.nodes, ref_a);
  const copy_b = by_ref(copy_graph.nodes, ref_b);
  const copy_one = by_label(copy_graph.nodes, "note one");
  const copy_three = by_label(copy_graph.nodes, "note three");
  ok(copy_a.data_id === source_node_a.data_id && copy_b.data_id === by_ref(source_graph.nodes, ref_b).data_id,
    "the copy's Translator nodes share the source's Translator data");

  const other_user_nodes = sorted_by(await entities.get_user_nodes(other.id, true), "label");
  ok(same(other_user_nodes.map((row) =>
    ({ user_id: row.user_id, label: row.label, type: row.type, data: row.data, time_deleted: row.time_deleted })), [
    { user_id: other.id, label: "note one", type: "biolink:Gene", data: { text: "first" }, time_deleted: null },
    { user_id: other.id, label: "note three", type: "Other", data: { text: "third" }, time_deleted: null },
    { user_id: other.id, label: "note two", type: "Other", data: { text: "second" }, time_deleted: null }
  ]), "the copying user gets their own user node for each one on the active canvas, and none for the trashed node");
  const [dup_one, dup_three, dup_two] = other_user_nodes;
  ok(copy_one.user_data_id === dup_one.id && copy_three.user_data_id === dup_three.id
    && copy_a.user_data_id === dup_two.id && copy_b.user_data_id === null,
    "each copied node points at the copying user's own user node");
  ok(new Set([u1.id, u2.id, u3.id, u4.id, dup_one.id, dup_two.id, dup_three.id]).size === 7,
    "the duplicated user nodes are new rows");

  const other_user_edges = sorted_by(await entities.get_user_edges(other.id, true), "label");
  ok(same(other_user_edges.map((row) => ({
    user_id: row.user_id, label: row.label, data: row.data,
    subject_ref: row.subject_ref, subject_user_node_id: row.subject_user_node_id,
    object_ref: row.object_ref, object_user_node_id: row.object_user_node_id
  })), [
    { user_id: other.id, label: "a to b", data: { weight: 3 },
      subject_ref: ref_a, subject_user_node_id: null, object_ref: ref_b, object_user_node_id: null },
    { user_id: other.id, label: "one to a", data: { weight: 1 },
      subject_ref: null, subject_user_node_id: dup_one.id, object_ref: ref_a, object_user_node_id: null },
    { user_id: other.id, label: "one to three", data: { weight: 2 },
      subject_ref: null, subject_user_node_id: dup_one.id, object_ref: null, object_user_node_id: dup_three.id }
  ]), "the copying user gets their own user edges, wired to their own user nodes");
  const [dup_ab, dup_1a, dup_13] = other_user_edges;
  ok(new Set([ue_1a.id, ue_13.id, ue_ab.id, dup_ab.id, dup_1a.id, dup_13.id]).size === 6,
    "the duplicated user edges are new rows");
  const copy_edge = (user_data_id) => copy_graph.edges.find((row) => row.user_data_id === user_data_id);
  ok(copy_graph.edges.length === 3
    && copy_edge(dup_ab.id).ref === ref_ab
    && copy_edge(dup_ab.id).subject_id === copy_a.id && copy_edge(dup_ab.id).object_id === copy_b.id
    && copy_edge(dup_1a.id).ref === null
    && copy_edge(dup_1a.id).subject_id === copy_one.id && copy_edge(dup_1a.id).object_id === copy_a.id
    && copy_edge(dup_13.id).ref === null
    && copy_edge(dup_13.id).subject_id === copy_one.id && copy_edge(dup_13.id).object_id === copy_three.id,
    "each copied edge points at the copying user's own user edge and connects the copy's own nodes");

  ok(same(ids(await entities.get_user_nodes(owner.id, true)), ascending([u1.id, u2.id, u3.id, u4.id]))
    && same(ids(await entities.get_user_edges(owner.id, true)), ascending([ue_1a.id, ue_13.id, ue_ab.id])),
    "the source owner's user data is untouched");
  ok(by_ref(source_graph.nodes, ref_a).user_data_id === u2.id
    && by_label(source_graph.nodes, "note one").user_data_id === u1.id
    && by_label(source_graph.nodes, "note three").user_data_id === u3.id
    && by_ref(source_graph.edges, ref_ab).user_data_id === ue_ab.id,
    "the source canvas still points at the owner's user data");

  const remerged = await store.merge_canvas_graph(other.id, copied.id,
    graph([{ x: 0, y: 0, user_data_id: dup_one.id }]), null);
  ok(remerged !== null && remerged.nodes.length === 4,
    "the copying user can merge their duplicated user node on the copy");
  const detached = await store.update_canvas_node_by_user(other.id, copied.id, copy_a.id, { user_data_id: null });
  const reattached = await store.update_canvas_node_by_user(other.id, copied.id, copy_a.id, { user_data_id: dup_two.id });
  ok(detached && detached.user_data_id === null && reattached && reattached.user_data_id === dup_two.id,
    "the copying user can detach and re-attach their duplicated user node");
  ok(await store.update_canvas_node_by_user(other.id, copied.id, copy_b.id, { user_data_id: u3.id }) === null,
    "the copying user cannot attach the source owner's user node");
  await entities.update_user_nodes(other.id, [{ id: dup_one.id, fields: { label: "renamed by other" } }]);
  ok((await entities.get_user_nodes(owner.id, false, [u1.id]))[0].label === "note one"
    && (await entities.get_user_nodes(other.id, false, [dup_one.id]))[0].label === "renamed by other",
    "editing a duplicated user node leaves the owner's user node untouched");

  const copied_again = await service.copy_shared_canvas(other.id, share_1.id, {});
  const again_graph = await store.get_canvas_graph_by_user(other.id, copied_again.id, false);
  ok(copied_again.label === `db-test share ${stamp}` && same(again_graph.annotations, []),
    "a copy made through a link without options keeps the source label and has no annotations");
  ok((await entities.get_user_nodes(other.id, true)).length === 6
    && (await entities.get_user_edges(other.id, true)).length === 6,
    "a second copy gets its own duplicates of the user data");
  ok(again_graph.nodes.every((row) => ![dup_one.id, dup_two.id, dup_three.id].includes(row.user_data_id)),
    "the second copy does not reuse the first copy's user nodes");

  const own_copy = await service.copy_shared_canvas(owner.id, share_1.id, {});
  const own_graph = await store.get_canvas_graph_by_user(owner.id, own_copy.id, false);
  ok(own_copy.user_id === owner.id && by_ref(own_graph.nodes, ref_a).user_data_id === u2.id
    && by_label(own_graph.nodes, "note one").user_data_id === u1.id
    && by_ref(own_graph.edges, ref_ab).user_data_id === ue_ab.id,
    "the owner copying through their own link keeps pointing at their own user data");
  ok(same(ids(await entities.get_user_nodes(owner.id, true)), ascending([u1.id, u2.id, u3.id, u4.id]))
    && same(ids(await entities.get_user_edges(owner.id, true)), ascending([ue_1a.id, ue_13.id, ue_ab.id])),
    "the owner copying through their own link creates no user data");

  await store.trash_canvases_by_user(owner.id, [canvas.id]);
  ok(await service.get_shared_canvas_graph(share_1.id) === null, "a link to a trashed canvas reads nothing");
  ok(await service.get_shared_node_data(share_1.id, source_node_a.id) === null,
    "a link to a trashed canvas reads no node data");
  ok(await service.copy_shared_canvas(other.id, share_1.id, {}) === null, "a link to a trashed canvas copies nothing");
  ok(same(await store.create_canvas_shares([new CanvasShare({ canvas_id: canvas.id, user_id: owner.id })]), []),
    "a link cannot be created on a trashed canvas");
  ok(await store.get_canvas_shares_by_user(owner.id, canvas.id) === null, "a trashed canvas has no link list");
  ok(same(ids(await store.get_canvas_shares([share_1.id])), [share_1.id]), "a link row survives its canvas being trashed");
  ok(same(await store.delete_canvas_shares_by_user(owner.id, canvas.id, [share_1.id]), [share_1.id]),
    "a link on a trashed canvas can be deleted");
  await store.restore_canvases_by_user(owner.id, [canvas.id]);
  ok(same(await store.get_canvas_shares_by_user(owner.id, canvas.id), []),
    "the restored canvas has no links left");
  ok((await store.get_canvas_graph_by_user(other.id, copied.id, false)).nodes.length === 4,
    "trashing the source and deleting its links leaves the copy intact");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
