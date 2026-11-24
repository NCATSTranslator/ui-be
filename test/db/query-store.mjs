import { isDeepStrictEqual } from "node:util";
import { randomUUID } from "node:crypto";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user } from "#test/lib/db-harness.mjs";
import { pgExec } from "#lib/postgres_preamble.mjs";
import { UserSavedData } from "#model/UserSavedData.mjs";
import { make_filter_state_from_req } from "#model/FilterState.mjs";
import { QueryStorePostgres } from "#store/QueryStorePostgres.mjs";
import { QueryViewStorePostgres } from "#store/QueryViewStorePostgres.mjs";
import { FilterStateStorePostgres } from "#store/FilterStateStorePostgres.mjs";
import { UserSavedDataStorePostgres } from "#store/UserSavedDataStorePostgres.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const store = new QueryStorePostgres(pool);
const view_store = new QueryViewStorePostgres(pool);
const filter_store = new FilterStateStorePostgres(pool);
const saved_store = new UserSavedDataStorePostgres(pool);

const count_query_rows = async (table, query_id) => {
  const res = await pgExec(pool, `SELECT COUNT(*)::int AS found FROM ${table} WHERE qid = $1`, [query_id]);
  return res.rows[0].found;
};

const count_saved_queries = async (user_id, pk) => {
  const saves = (await saved_store.retrieveUserSavedDataBy(user_id, { ars_pkey: pk, save_type: "query" }, true)) ?? [];
  return saves.length;
};

console.log("# QueryStorePostgres");
try {
  const user = await create_test_user(pool);
  const old_unowned_pk = randomUUID();
  const recent_unowned_pk = randomUUID();
  const old_owned_pk = randomUUID();

  await pgExec(pool,
    "INSERT INTO queries (pk, time_created) VALUES ($1, NOW() - INTERVAL '31 days')", [old_unowned_pk]);
  await pgExec(pool,
    "INSERT INTO queries (pk, time_created) VALUES ($1, NOW() - INTERVAL '29 days')", [recent_unowned_pk]);
  const old_owned = await pgExec(pool,
    "INSERT INTO queries (pk, time_created) VALUES ($1, NOW() - INTERVAL '31 days') RETURNING id", [old_owned_pk]);
  await pgExec(pool, "INSERT INTO query_to_user (qid, uid) VALUES ($1, $2)", [old_owned.rows[0].id, user.id]);

  ok((await store.retrieveQueryByPk(old_unowned_pk))?.pk === old_unowned_pk,
    "an unowned query older than 30 days is stored before the purge");

  const delete_count = await store.delete_unowned_queries();
  ok(delete_count >= 1,
    "the purge reports the rows it deleted");
  ok((await store.retrieveQueryByPk(old_unowned_pk)) === null,
    "an unowned query older than 30 days is deleted");
  ok((await store.retrieveQueryByPk(recent_unowned_pk))?.pk === recent_unowned_pk,
    "an unowned query newer than 30 days is kept");
  ok((await store.retrieveQueryByPk(old_owned_pk))?.pk === old_owned_pk,
    "an owned query older than 30 days is kept");

  const owner = await create_test_user(pool);
  const lost_pk = randomUUID();
  const young_pk = randomUUID();
  const complete_pk = randomUUID();
  const lost = await pgExec(pool,
    "INSERT INTO queries (pk, status, time_created) VALUES ($1, 'running', NOW() - INTERVAL '25 hours') RETURNING id",
    [lost_pk]);
  const young = await pgExec(pool,
    "INSERT INTO queries (pk, status, time_created) VALUES ($1, 'running', NOW() - INTERVAL '23 hours') RETURNING id",
    [young_pk]);
  const complete = await pgExec(pool,
    "INSERT INTO queries (pk, status, time_created) VALUES ($1, 'complete', NOW() - INTERVAL '25 hours') RETURNING id",
    [complete_pk]);
  const lost_id = lost.rows[0].id;
  const young_id = young.rows[0].id;
  const complete_id = complete.rows[0].id;
  await saved_store.create_user_query(new UserSavedData({
    user_id: owner.id, save_type: "query", ars_pkey: lost_pk, data: { description: { type: "drug" } }
  }), lost_id);
  await saved_store.create_user_query(new UserSavedData({
    user_id: owner.id, save_type: "query", ars_pkey: young_pk, data: { description: { type: "drug" } }
  }), young_id);
  await saved_store.create_user_query(new UserSavedData({
    user_id: owner.id, save_type: "query", ars_pkey: complete_pk, data: { description: { type: "drug" } }
  }), complete_id);
  const [filter_state] = await filter_store.create_filter_states([
    make_filter_state_from_req(owner.id, { label: "preset", version: 1, state: { filters: [] } })
  ]);
  await view_store.create_query_view(owner.id, lost_pk, filter_state.id);
  await view_store.set_working_view(owner.id, lost_pk, 1, { filters: [] });
  await view_store.create_query_view(owner.id, young_pk, filter_state.id);
  await view_store.set_working_view(owner.id, young_pk, 1, { filters: [] });
  const project = await saved_store.createUserSavedData(new UserSavedData({
    user_id: owner.id, save_type: "project", label: "project", data: { pks: [lost_pk, young_pk, complete_pk] }
  }));
  ok((await count_query_rows("query_view", lost_id)) === 1 && (await count_query_rows("query_working_view", lost_id)) === 1,
    "the lost query has a view and a working view before it is deleted");

  const lost_count = await store.delete_lost_queries([lost_pk, young_pk, complete_pk]);
  ok(lost_count === 1,
    "only the query running for more than 24 hours is deleted");
  ok((await store.retrieveQueryByPk(lost_pk)) === null,
    "a query running for more than 24 hours is deleted");
  ok((await store.retrieveQueryByPk(young_pk))?.status === "running",
    "a query running for less than 24 hours is kept");
  ok((await store.retrieveQueryByPk(complete_pk))?.status === "complete",
    "a complete query older than 24 hours is kept");
  ok(isDeepStrictEqual(
      [await count_query_rows("query_to_user", lost_id), await count_query_rows("query_to_user", young_id),
       await count_query_rows("query_to_user", complete_id)],
      [0, 1, 1]),
    "only the deleted query loses its ownership");
  ok(isDeepStrictEqual(
      [await count_query_rows("query_view", lost_id), await count_query_rows("query_view", young_id)],
      [0, 1]),
    "only the deleted query loses its views");
  ok(isDeepStrictEqual(
      [await count_query_rows("query_working_view", lost_id), await count_query_rows("query_working_view", young_id)],
      [0, 1]),
    "only the deleted query loses its working view");
  ok(isDeepStrictEqual(
      [await count_saved_queries(owner.id, lost_pk), await count_saved_queries(owner.id, young_pk),
       await count_saved_queries(owner.id, complete_pk)],
      [0, 1, 1]),
    "only the deleted query loses its saved query");
  const [stored_project] = await saved_store.retrieveUserSavedDataBy(owner.id, { id: project.id });
  ok(isDeepStrictEqual(stored_project.data, { pks: [young_pk, complete_pk] }),
    "the deleted query is dropped from the project and the other pks keep their order");

  let duplicate_error_code = null;
  try {
    await pgExec(pool, "INSERT INTO queries (pk) VALUES ($1)", [old_owned_pk]);
  } catch (err) {
    duplicate_error_code = err.code;
  }
  ok(duplicate_error_code === "23505",
    "a second query with the same pk is rejected");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
