import { randomUUID } from 'node:crypto';
import { isDeepStrictEqual } from 'node:util';
import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';
import { connect_test_db, create_test_user, create_test_query } from '../lib/db-harness.mjs';

const FILTERS_PATH = '/api/v1/users/me/filters';
const QUERIES_PATH = '/api/v1/users/me/queries';
const UNKNOWN_ID = 999999999;

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();

const working_view_path = (qid) => `${QUERIES_PATH}/${qid}/working-view`;
const snapshot_path = (qid) => `${QUERIES_PATH}/${qid}/views/snapshot`;

console.log(`# ${QUERIES_PATH}/:qid/working-view  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const run_id = Date.now();
  const query = await create_test_query(pool, TEST_USER_ID);
  const other_query = await create_test_query(pool, TEST_USER_ID);
  const other_user = await create_test_user(pool);
  const not_owned_query = await create_test_query(pool, other_user.id);
  const state_a = { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id };
  const state_b = { filters: [{ id: 'p/pc/1', value: '', negated: true }], run: run_id };

  const unset = await getJson(working_view_path(query.pk));
  ok(unset.res.status === 404,
    `a query with no working view -> 404 (got ${unset.res.status})`);

  const set = await putJson(working_view_path(query.pk), { version: 1, state: state_a });
  ok(set.res.status === 200,
    `set the working view from a version and state responds 200 (got ${set.res.status})`);
  ok(set.json.pk === query.pk && set.json.user_id === TEST_USER_ID,
    'the response names the query and user');
  ok(set.json.version === 1 && isDeepStrictEqual(set.json.state, state_a) && set.json.filter_state === undefined,
    'the response carries the version and state inline');

  const read = await getJson(working_view_path(query.pk));
  ok(read.res.status === 200 && isDeepStrictEqual(read.json.state, state_a),
    `the working view is read back (got ${read.res.status})`);
  ok((await getJson(working_view_path(other_query.pk))).res.status === 404,
    'another query of the user still has no working view');
  const filter_states_before = (await getJson(`${FILTERS_PATH}?include_hidden=true`)).json;
  ok(filter_states_before.every((filter_state) => !isDeepStrictEqual(filter_state.state, state_a)),
    'setting the working view does not create a filter state');

  const preset = await postJson(FILTERS_PATH, {
    label: `api-test state preset ${run_id}`,
    version: 2,
    state: state_b
  });
  const preset_id = preset.json.id;
  const replaced = await putJson(working_view_path(query.pk), { filter_state_id: preset_id });
  ok(replaced.res.status === 200 && replaced.json.version === 2 && isDeepStrictEqual(replaced.json.state, state_b),
    `set the working view from a filter state id responds 200 with its version and state (got ${replaced.res.status})`);
  ok(isDeepStrictEqual((await getJson(working_view_path(query.pk))).json.state, state_b),
    'the replacement is read back');

  await putJson(working_view_path(other_query.pk), { version: 1, state: state_a });
  ok(isDeepStrictEqual((await getJson(working_view_path(query.pk))).json.state, state_b)
    && isDeepStrictEqual((await getJson(working_view_path(other_query.pk))).json.state, state_a),
    'each query reads back its own working view');

  const unknown_pk = await putJson(working_view_path(randomUUID()), { version: 1, state: state_a });
  ok(unknown_pk.res.status === 404,
    `set for an unknown pk -> 404 (got ${unknown_pk.res.status})`);
  const not_owned = await putJson(working_view_path(not_owned_query.pk), { version: 1, state: state_a });
  ok(not_owned.res.status === 404,
    `set for another user's query -> 404 (got ${not_owned.res.status})`);
  ok((await getJson(working_view_path(not_owned_query.pk))).res.status === 404,
    "get for another user's query -> 404");

  const unknown_filter = await putJson(working_view_path(query.pk), { filter_state_id: UNKNOWN_ID });
  ok(unknown_filter.res.status === 400,
    `an unknown filter state id -> 400 (got ${unknown_filter.res.status})`);
  const empty_body = await putJson(working_view_path(query.pk), {});
  ok(empty_body.res.status === 400,
    `an empty body -> 400 (got ${empty_body.res.status})`);
  const labeled = await putJson(working_view_path(query.pk), { label: 'x', version: 1, state: {} });
  ok(labeled.res.status === 400,
    `a label in the body -> 400 (got ${labeled.res.status})`);
  const bad_pk = await putJson(working_view_path('not-a-uuid'), { version: 1, state: state_a });
  ok(bad_pk.res.status === 400,
    `a non-UUID pk -> 400 (got ${bad_pk.res.status})`);
  const too_big = await putJson(working_view_path(query.pk), { version: 1, state: 'x'.repeat(70000) });
  ok(too_big.res.status === 413,
    `an oversized body -> 413 (got ${too_big.res.status})`);
  ok(isDeepStrictEqual((await getJson(working_view_path(query.pk))).json.state, state_b),
    'rejected requests leave the working view unchanged');

  const snapshot = await postJson(snapshot_path(query.pk), undefined);
  ok(snapshot.res.status === 200 && Number.isInteger(snapshot.json.id),
    `snapshot responds 200 with a query view id (got ${snapshot.res.status})`);
  ok(snapshot.json.filter_state?.id === preset_id && snapshot.json.pk === query.pk,
    'a snapshot equal to a preset carries the preset and the query pk');
  const snapshot_again = await postJson(snapshot_path(query.pk), undefined);
  ok(snapshot_again.json.id === snapshot.json.id,
    'snapshotting the same working view again returns the same query view');
  const moved_on = await putJson(working_view_path(query.pk), { version: 1, state: state_a });
  ok(moved_on.res.status === 200,
    `the working view moves to another state after the snapshot (got ${moved_on.res.status})`);
  const snapshot_read = await getJson(`/api/v1/query/${query.pk}/views/${snapshot.json.id}`);
  ok(snapshot_read.res.status === 200 && snapshot_read.json.filter_state?.id === preset_id,
    'the snapshot keeps its filter state after the working view changes');
  const second_snapshot = await postJson(snapshot_path(query.pk), undefined);
  ok(second_snapshot.json.id !== snapshot.json.id && isDeepStrictEqual(second_snapshot.json.filter_state?.state, state_a),
    'snapshotting the moved working view makes a different query view');
  ok(second_snapshot.json.filter_state?.hidden === true && second_snapshot.json.filter_state?.label === null,
    'a snapshot of a new state saves it as a hidden filter state');
  const hidden_filter_id = second_snapshot.json.filter_state?.id;
  ok((await getJson(`${FILTERS_PATH}/${hidden_filter_id}`)).res.status === 200,
    'the hidden filter state of a snapshot exists');
  const no_working_view = await postJson(snapshot_path(not_owned_query.pk), undefined);
  ok(no_working_view.res.status === 404,
    `snapshot of a query without a working view -> 404 (got ${no_working_view.res.status})`);
  const bad_pk_snapshot = await postJson(snapshot_path('not-a-uuid'), undefined);
  ok(bad_pk_snapshot.res.status === 400,
    `snapshot with a non-UUID pk -> 400 (got ${bad_pk_snapshot.res.status})`);

  await putJson(`${FILTERS_PATH}/trash`, [preset_id]);
  ok(isDeepStrictEqual((await getJson(working_view_path(other_query.pk))).json.state, state_a),
    'trashing a preset leaves working views unchanged');
  await putJson(working_view_path(query.pk), { version: 2, state: state_b });
  const on_trashed = await postJson(snapshot_path(query.pk), undefined);
  ok(on_trashed.res.status === 200 && on_trashed.json.filter_state?.id === preset_id
    && on_trashed.json.filter_state?.time_deleted !== null,
    `a snapshot equal to a trashed preset views the trashed preset (got ${on_trashed.res.status})`);
  ok((await getJson(`${FILTERS_PATH}/${preset_id}`)).res.status === 404,
    'the preset stays trashed after the snapshot');
  const restored = await putJson(`${FILTERS_PATH}/restore`, [preset_id]);
  ok(isDeepStrictEqual(restored.json, [preset_id]),
    'the trashed preset can still be restored');
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
