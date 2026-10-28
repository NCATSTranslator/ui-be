export { connect_test_db, create_test_user, create_test_query, rejects_with }

import { randomUUID } from "node:crypto";
import { pg, pgExec } from "#lib/postgres_preamble.mjs";
import { bootstrapConfig } from "#lib/config.mjs";
import { logger } from "#lib/logger.mjs";
import { User } from "#model/User.mjs";
import { UserStorePostgres } from "#store/UserStorePostgres.mjs";

const _BASE_CONFIG = process.env.DB_TEST_CONFIG ?? "configurations/mock.json";
const _OVERRIDE_CONFIG = "configurations/local-overrides.json";

async function connect_test_db() {
  logger.level = "silent";
  const config = await bootstrapConfig(_BASE_CONFIG, _OVERRIDE_CONFIG);
  const pool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return { pool: pool, signing_secret: config.secrets.hmac.key };
}

async function create_test_user(pool) {
  const id = randomUUID();
  return new UserStorePostgres(pool).createNewUser(new User({
    id: id,
    name: `db-test ${id}`,
    email: `db-test-${id}@local.invalid`
  }));
}

async function create_test_query(pool, user_id) {
  const pk = randomUUID();
  const res = await pgExec(pool,
    "INSERT INTO queries (pk, metadata) VALUES ($1, $2) RETURNING id, pk", [pk, { query: { type: "db-test" } }]);
  await pgExec(pool, "INSERT INTO query_to_user (qid, uid) VALUES ($1, $2)", [res.rows[0].id, user_id]);
  return res.rows[0];
}

async function rejects_with(promise, error_class) {
  try {
    await promise;
    return false;
  } catch (err) {
    return err instanceof error_class;
  }
}
