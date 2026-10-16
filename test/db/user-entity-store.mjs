import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user, rejects_with } from "#test/lib/db-harness.mjs";
import { UserEntityStorePostgres } from "#store/UserEntityStorePostgres.mjs";
import { make_user_node_from_req, make_user_edge_from_req, UserEntityRequestError } from "#model/UserEntity.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const store = new UserEntityStorePostgres(pool);

console.log("# UserEntityStorePostgres");
try {
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);
  const [own_node, trashed_node] = await store.create_user_nodes([
    make_user_node_from_req(user.id, { label: "own", data: {} }),
    make_user_node_from_req(user.id, { label: "trashed", data: {} })
  ]);
  const [other_node] = await store.create_user_nodes([make_user_node_from_req(other.id, { label: "other", data: {} })]);
  await store.trash_user_nodes(user.id, [trashed_node.id]);
  const edge_from = (user_node_id) => make_user_edge_from_req(user.id, {
    label: "edge", data: {}, subject_user_node_id: user_node_id, object_ref: "MONDO:0005148"
  });

  const created = await store.create_user_edges([
    edge_from(own_node.id),
    make_user_edge_from_req(user.id, { label: "refs", data: {}, subject_ref: "A:1", object_ref: "B:2" })
  ]);
  ok(created.length === 2 && created[0].subject_user_node_id === own_node.id && created[1].subject_ref === "A:1",
    "user edges to an owned active user node or to refs are created");

  ok(await rejects_with(store.create_user_edges([edge_from(other_node.id)]), UserEntityRequestError),
    "an endpoint on another user's node is rejected");
  ok(await rejects_with(store.create_user_edges([edge_from(trashed_node.id)]), UserEntityRequestError),
    "an endpoint on a trashed user node is rejected");
  ok(await rejects_with(store.create_user_edges([edge_from(Number.MAX_SAFE_INTEGER)]), UserEntityRequestError),
    "an endpoint on a nonexistent user node is rejected");

  const before = (await store.get_user_edges(user.id, true)).length;
  ok(await rejects_with(store.create_user_edges([edge_from(own_node.id), edge_from(other_node.id)]), UserEntityRequestError),
    "a batch with one bad endpoint is rejected");
  const after = (await store.get_user_edges(user.id, true)).length;
  ok(after === before, `a rejected batch inserts nothing (before ${before}, after ${after})`);
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
