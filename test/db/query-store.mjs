import { randomUUID } from "node:crypto";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user } from "#test/lib/db-harness.mjs";
import { pgExec } from "#lib/postgres_preamble.mjs";
import { QueryStorePostgres } from "#store/QueryStorePostgres.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const store = new QueryStorePostgres(pool);

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
