import { isDeepStrictEqual } from 'node:util';
import { randomUUID } from 'node:crypto';
import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { connect_test_db, create_test_user, create_test_query } from '../lib/db-harness.mjs';

const COPY_PATH = '/api/v1/users/me/queries/copy';
const TRASH_PATH = '/api/v1/users/me/queries/trash';
const SAVES_PATH = '/api/v1/users/me/saves?type=query&include_deleted=true';

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();

const list_saved_queries = async (pk) => {
  const res = await getJson(SAVES_PATH);
  return (res.json || []).filter((save) => save.ars_pkey === pk);
};

const deleted_flags = (saves) => saves.map((save) => save.deleted);

console.log(`# ${COPY_PATH}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const other_user = await create_test_user(pool);
  const query_a = await create_test_query(pool, other_user.id);
  const query_b = await create_test_query(pool, other_user.id);

  const first = await postJson(COPY_PATH, { pk: query_a.pk });
  ok(first.res.status === 200,
    `copying a query owned by someone else responds 200 (got ${first.res.status})`);
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_a.pk)), [false]),
    'the copy stores one active saved query for the copied pk');
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_b.pk)), []),
    'the copy stores nothing for a pk that was not copied');

  const second = await postJson(COPY_PATH, { pk: query_a.pk });
  ok(second.res.status === 200,
    `copying a query the user already owns responds 200 (got ${second.res.status})`);
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_a.pk)), [false]),
    'a repeat copy does not store a second saved query');

  const [saved_a] = await list_saved_queries(query_a.pk);
  await putJson(TRASH_PATH, [saved_a.id]);
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_a.pk)), [true]),
    'the saved query can be trashed');
  const after_trash = await postJson(COPY_PATH, { pk: query_a.pk });
  ok(after_trash.res.status === 200,
    `copying an owned query whose saved query is trashed responds 200 (got ${after_trash.res.status})`);
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_a.pk)), [true]),
    'a repeat copy leaves the trashed saved query trashed');

  const other_copy = await postJson(COPY_PATH, { pk: query_b.pk });
  ok(other_copy.res.status === 200,
    `copying a second query responds 200 (got ${other_copy.res.status})`);
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_b.pk)), [false]),
    'the second copy stores one active saved query for its own pk');
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_a.pk)), [true]),
    'the second copy leaves the first saved query unchanged');

  const query_c = await create_test_query(pool, other_user.id);
  const concurrent = await Promise.all([
    postJson(COPY_PATH, { pk: query_c.pk }),
    postJson(COPY_PATH, { pk: query_c.pk })
  ]);
  ok(isDeepStrictEqual(concurrent.map((copy) => copy.res.status), [200, 200]),
    'two simultaneous copies of one query both respond 200');
  ok(isDeepStrictEqual(deleted_flags(await list_saved_queries(query_c.pk)), [false]),
    'two simultaneous copies store one saved query');

  const unknown = await postJson(COPY_PATH, { pk: randomUUID() });
  ok(unknown.res.status === 400,
    `copying an unknown pk responds 400 (got ${unknown.res.status})`);
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
