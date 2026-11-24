import { isDeepStrictEqual } from "node:util";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user, create_test_query, rejects_with } from "#test/lib/db-harness.mjs";
import { pgExec } from "#lib/postgres_preamble.mjs";
import { UserSavedData } from "#model/UserSavedData.mjs";
import { UserSavedDataStorePostgres } from "#store/UserSavedDataStorePostgres.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const store = new UserSavedDataStorePostgres(pool);

const list_saved_queries = async (user_id, pk) => {
  const saves = (await store.retrieveUserSavedDataBy(user_id, { ars_pkey: pk }, true)) ?? [];
  return saves.map((save) => ({ save_type: save.save_type, data: save.data, deleted: save.deleted }));
};

const count_ownerships = async (query_id, user_id) => {
  const res = await pgExec(pool,
    "SELECT COUNT(*)::int AS found FROM query_to_user WHERE qid = $1 AND uid = $2", [query_id, user_id]);
  return res.rows[0].found;
};

console.log("# UserSavedDataStorePostgres");
try {
  const submitter = await create_test_user(pool);
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);
  const query_a = await create_test_query(pool, submitter.id);
  const query_b = await create_test_query(pool, submitter.id);
  const query_c = await create_test_query(pool, submitter.id);

  const created = await store.create_user_query(new UserSavedData({
    user_id: user.id,
    save_type: "query",
    ars_pkey: query_a.pk,
    data: { description: { type: "drug" } }
  }), query_a.id);
  ok(Number.isInteger(created.id),
    "a user query is created with an integer id");
  ok(isDeepStrictEqual(await list_saved_queries(user.id, query_a.pk),
      [{ save_type: "query", data: { description: { type: "drug" } }, deleted: false }]),
    "the saved query is stored for the user and pk");
  ok((await count_ownerships(query_a.id, user.id)) === 1,
    "the user is stored as an owner of the query");
  ok(isDeepStrictEqual(await list_saved_queries(user.id, query_b.pk), []),
    "nothing is saved for a query that was not created");
  ok((await count_ownerships(query_b.id, user.id)) === 0,
    "the user does not own a query that was not created");

  const repeated = await store.create_user_query(new UserSavedData({
    user_id: user.id,
    save_type: "query",
    ars_pkey: query_a.pk,
    data: { description: { type: "gene" } }
  }), query_a.id);
  ok(repeated === null,
    "creating a query the user already owns returns null");
  ok(isDeepStrictEqual(await list_saved_queries(user.id, query_a.pk),
      [{ save_type: "query", data: { description: { type: "drug" } }, deleted: false }]),
    "a repeat create leaves the first saved query as the only one");

  const others = await store.create_user_query(new UserSavedData({
    user_id: other.id,
    save_type: "query",
    ars_pkey: query_a.pk,
    data: { description: { type: "chemical" } }
  }), query_a.id);
  ok(Number.isInteger(others.id) && others.id !== created.id,
    "another user can own the same query");
  ok(isDeepStrictEqual(await list_saved_queries(other.id, query_a.pk),
      [{ save_type: "query", data: { description: { type: "chemical" } }, deleted: false }]),
    "the other user gets their own saved query");
  ok(isDeepStrictEqual(await list_saved_queries(user.id, query_a.pk),
      [{ save_type: "query", data: { description: { type: "drug" } }, deleted: false }]),
    "the first user's saved query is unchanged by the other user's");

  ok(await rejects_with(store.create_user_query(new UserSavedData({
      user_id: user.id,
      save_type: "query",
      ars_pkey: "not-a-uuid",
      data: { description: { type: "drug" } }
    }), query_b.id), Error),
    "a saved query that cannot be stored is rejected");
  ok((await count_ownerships(query_b.id, user.id)) === 0,
    "a rejected create does not leave the user owning the query");

  const concurrent = await Promise.all([
    store.create_user_query(new UserSavedData({
      user_id: user.id,
      save_type: "query",
      ars_pkey: query_c.pk,
      data: { description: { type: "drug" } }
    }), query_c.id),
    store.create_user_query(new UserSavedData({
      user_id: user.id,
      save_type: "query",
      ars_pkey: query_c.pk,
      data: { description: { type: "drug" } }
    }), query_c.id)
  ]);
  ok(concurrent.filter((saved) => saved !== null).length === 1,
    "only one of two concurrent creates stores a saved query");
  ok(isDeepStrictEqual(await list_saved_queries(user.id, query_c.pk),
      [{ save_type: "query", data: { description: { type: "drug" } }, deleted: false }]),
    "concurrent creates leave one saved query");
  ok((await count_ownerships(query_c.id, user.id)) === 1,
    "concurrent creates leave one ownership");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
