import { isDeepStrictEqual } from 'node:util';
import { randomUUID } from 'node:crypto';
import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { connect_test_db, create_test_user, create_test_query } from '../lib/db-harness.mjs';

const FILTERS_PATH = '/api/v1/users/me/filters';
const QUERIES_PATH = '/api/v1/users/me/queries';
const SHARE_PATH = '/api/v1/query';
const UNKNOWN_ID = 999999999;

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();

const views_path = (qid) => `${QUERIES_PATH}/${qid}/views`;

const list_query_views = async (qid, query = '') => {
  const res = await getJson(`${views_path(qid)}${query}`);
  return (res.json || []);
};

const stored_fields = (query_view) => {
  if (!query_view) return query_view;
  const { id, time_created, time_updated, filter_state, ...fields } = query_view;
  const { time_created: fs_created, time_updated: fs_updated, ...filter_fields } = filter_state;
  return { ...fields, filter_state: filter_fields };
};

console.log(`# ${QUERIES_PATH}/:qid/views  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const run_id = Date.now();
  const query = await create_test_query(pool, TEST_USER_ID);
  const other_query = await create_test_query(pool, TEST_USER_ID);
  const other_user = await create_test_user(pool);
  const not_owned_query = await create_test_query(pool, other_user.id);

  const created = await postJson(views_path(query.pk), {
    version: 1,
    state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id }
  });
  ok(created.res.status === 200,
    `create a view from a version and state responds 200 (got ${created.res.status})`);
  const view_id = created.json.id;
  const hidden_filter_id = created.json.filter_state?.id;

  const preset = await postJson(FILTERS_PATH, {
    label: `api-test view preset ${run_id}`,
    version: 1,
    state: { run: run_id, preset: true }
  });
  const preset_id = preset.json.id;
  const from_preset = await postJson(views_path(query.pk), { filter_state_id: preset_id });
  ok(from_preset.res.status === 200,
    `create a view from a filter state id responds 200 (got ${from_preset.res.status})`);
  ok(from_preset.json.id !== view_id,
    'a view with another filter state gets a different id');
  const preset_view_id = from_preset.json.id;

  const listed = await list_query_views(query.pk);
  ok(isDeepStrictEqual(listed.map((query_view) => query_view.id), [preset_view_id, view_id]),
    'list returns the views of the query newest first');
  ok(
    isDeepStrictEqual(stored_fields(listed[1]), {
      user_id: TEST_USER_ID,
      pk: query.pk,
      filter_state: {
        id: hidden_filter_id,
        user_id: TEST_USER_ID,
        label: null,
        hidden: true,
        version: 1,
        state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id },
        time_deleted: null
      },
      time_deleted: null
    }),
    'list returns the view with its hidden filter state inline'
  );
  ok(
    isDeepStrictEqual(stored_fields(listed[0]), {
      user_id: TEST_USER_ID,
      pk: query.pk,
      filter_state: {
        id: preset_id,
        user_id: TEST_USER_ID,
        label: `api-test view preset ${run_id}`,
        hidden: false,
        version: 1,
        state: { run: run_id, preset: true },
        time_deleted: null
      },
      time_deleted: null
    }),
    'list returns the other view with the preset inline'
  );

  ok(!(await getJson(FILTERS_PATH)).json.some((filter_state) => filter_state.id === hidden_filter_id),
    'the filter state saved for a view is not listed as a preset');
  ok((await getJson(`${FILTERS_PATH}/${hidden_filter_id}`)).res.status === 200,
    'the filter state saved for a view can be fetched by id');

  const repeated = await postJson(views_path(query.pk), {
    version: 1,
    state: { run: run_id, filters: [{ negated: false, value: '', id: 'n/fda/4' }] }
  });
  ok(repeated.json.id === view_id && repeated.json.filter_state.id === hidden_filter_id,
    'creating a view with an equal version and state returns the existing view and filter state');
  ok(isDeepStrictEqual((await list_query_views(query.pk)).map((query_view) => query_view.id), [view_id, preset_view_id]),
    'a repeated view moves to the front of the list');

  const on_other_query = await postJson(views_path(other_query.pk), { filter_state_id: preset_id });
  ok(on_other_query.res.status === 200 && on_other_query.json.id !== preset_view_id,
    'the same filter state on another query is a separate view');
  ok(isDeepStrictEqual((await list_query_views(other_query.pk)).map((query_view) => query_view.id), [on_other_query.json.id]),
    'list is scoped to the query');

  const bad_pk = await postJson(views_path('not-a-uuid'), { filter_state_id: preset_id });
  ok(bad_pk.res.status === 400,
    `a non-UUID pk -> 400 (got ${bad_pk.res.status})`);
  const unknown_pk = await postJson(views_path(randomUUID()), { filter_state_id: preset_id });
  ok(unknown_pk.res.status === 404,
    `an unknown pk -> 404 (got ${unknown_pk.res.status})`);
  const not_owned = await postJson(views_path(not_owned_query.pk), { filter_state_id: preset_id });
  ok(not_owned.res.status === 404,
    `another user's query -> 404 (got ${not_owned.res.status})`);
  ok(isDeepStrictEqual(await list_query_views(not_owned_query.pk), []),
    "listing another user's query returns nothing");

  const unknown_filter = await postJson(views_path(query.pk), { filter_state_id: UNKNOWN_ID });
  ok(unknown_filter.res.status === 400,
    `an unknown filter state id -> 400 (got ${unknown_filter.res.status})`);
  const empty_body = await postJson(views_path(query.pk), {});
  ok(empty_body.res.status === 400,
    `an empty body -> 400 (got ${empty_body.res.status})`);
  const string_id = await postJson(views_path(query.pk), { filter_state_id: String(preset_id) });
  ok(string_id.res.status === 400,
    `a string filter state id -> 400 (got ${string_id.res.status})`);
  const labeled = await postJson(views_path(query.pk), { label: 'x', version: 1, state: {} });
  ok(labeled.res.status === 400,
    `a label in a view request -> 400 (got ${labeled.res.status})`);
  const no_version = await postJson(views_path(query.pk), { state: {} });
  ok(no_version.res.status === 400,
    `a state without a version -> 400 (got ${no_version.res.status})`);
  const too_big = await postJson(views_path(query.pk), { version: 1, state: 'x'.repeat(70000) });
  ok(too_big.res.status === 413,
    `an oversized body -> 413 (got ${too_big.res.status})`);
  ok((await list_query_views(query.pk)).length === 2,
    'rejected requests add no views');

  const trash = await putJson(`${views_path(query.pk)}/trash`, [view_id, on_other_query.json.id]);
  ok(trash.res.status === 200 && isDeepStrictEqual(trash.json, [view_id]),
    'trash responds with the trashed ids of the query only');
  ok(isDeepStrictEqual((await list_query_views(query.pk)).map((query_view) => query_view.id), [preset_view_id]),
    'a trashed view is excluded from the list');
  ok(isDeepStrictEqual((await list_query_views(other_query.pk)).map((query_view) => query_view.id), [on_other_query.json.id]),
    "the other query's view stays active");
  const with_deleted = await list_query_views(query.pk, '?include_deleted=true');
  ok(with_deleted.find((query_view) => query_view.id === view_id)?.time_deleted !== null
    && with_deleted.find((query_view) => query_view.id === view_id)?.time_deleted !== undefined,
    'list with include_deleted returns the trashed view with time_deleted');

  const shared = await getJson(`${SHARE_PATH}/${query.pk}/views/${preset_view_id}`);
  ok(shared.res.status === 200 && shared.json.id === preset_view_id,
    `a view is readable by query and id on the share route (got ${shared.res.status})`);
  ok(shared.json.filter_state?.id === preset_id,
    'the shared view carries its filter state');
  const shared_trashed = await getJson(`${SHARE_PATH}/${query.pk}/views/${view_id}`);
  ok(shared_trashed.res.status === 200 && shared_trashed.json.time_deleted !== null,
    `a trashed view still resolves on the share route (got ${shared_trashed.res.status})`);
  const shared_wrong_query = await getJson(`${SHARE_PATH}/${other_query.pk}/views/${view_id}`);
  ok(shared_wrong_query.res.status === 404,
    `a view requested through another query -> 404 (got ${shared_wrong_query.res.status})`);
  const shared_unknown = await getJson(`${SHARE_PATH}/${query.pk}/views/${UNKNOWN_ID}`);
  ok(shared_unknown.res.status === 404,
    `an unknown view id on the share route -> 404 (got ${shared_unknown.res.status})`);
  const shared_bad_id = await getJson(`${SHARE_PATH}/${query.pk}/views/abc`);
  ok(shared_bad_id.res.status === 400,
    `a non-integer view id on the share route -> 400 (got ${shared_bad_id.res.status})`);
  const shared_bad_pk = await getJson(`${SHARE_PATH}/not-a-uuid/views/${view_id}`);
  ok(shared_bad_pk.res.status === 400,
    `a non-UUID pk on the share route -> 400 (got ${shared_bad_pk.res.status})`);

  const recreated = await postJson(views_path(query.pk), { filter_state_id: hidden_filter_id });
  ok(recreated.json.id !== view_id,
    'a trashed view can be created again under a new id');
  const blocked_restore = await putJson(`${views_path(query.pk)}/restore`, [view_id]);
  ok(blocked_restore.res.status === 200 && isDeepStrictEqual(blocked_restore.json, []),
    'restore while an equal view is active responds with no ids');
  await putJson(`${views_path(query.pk)}/trash`, [recreated.json.id]);
  const double_restore = await putJson(`${views_path(query.pk)}/restore`, [view_id, recreated.json.id]);
  ok(double_restore.res.status === 400,
    `restoring two equal trashed views -> 400 (got ${double_restore.res.status})`);
  const restore = await putJson(`${views_path(query.pk)}/restore`, [view_id]);
  ok(restore.res.status === 200 && isDeepStrictEqual(restore.json, [view_id]),
    'restore responds with the restored ids');
  ok((await list_query_views(query.pk)).some((query_view) => query_view.id === view_id),
    'a restored view is listed again');

  const non_array_trash = await putJson(`${views_path(query.pk)}/trash`, { id: view_id });
  ok(non_array_trash.res.status === 400,
    `trash with a non-array body -> 400 (got ${non_array_trash.res.status})`);
  const bad_pk_trash = await putJson(`${views_path('not-a-uuid')}/trash`, [view_id]);
  ok(bad_pk_trash.res.status === 400,
    `trash with a non-UUID pk -> 400 (got ${bad_pk_trash.res.status})`);
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
