import { isDeepStrictEqual } from 'node:util';
import { createHarness, getJson, postJson, putJson, BASE_URL, TEST_USER_ID } from '../lib/api-harness.mjs';

const FILTERS_PATH = '/api/v1/users/me/filters';
const UNKNOWN_ID = 999999999;

const { ok, fail, finish } = createHarness();

const list_filter_states = async (query = '') => {
  const res = await getJson(`${FILTERS_PATH}${query}`);
  return new Map((res.json || []).map((filter_state) => [filter_state.id, filter_state]));
};

const stored_fields = (filter_state) => {
  if (!filter_state) return filter_state;
  const { id, time_created, time_updated, ...fields } = filter_state;
  return fields;
};

console.log(`# ${FILTERS_PATH}  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const run_id = Date.now();

  const created = await postJson(FILTERS_PATH, {
    label: `api-test preset ${run_id}`,
    version: 1,
    state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id }
  });
  ok(created.res.status === 200,
    `create filter state responds 200 (got ${created.res.status})`);
  const preset_id = created.json.id;

  const other = await postJson(FILTERS_PATH, {
    label: `api-test other ${run_id}`,
    version: 2,
    state: { run: run_id, other: true }
  });
  ok(other.res.status === 200,
    `create a second filter state responds 200 (got ${other.res.status})`);
  ok(other.json.id !== preset_id,
    'a different state gets a different id');
  const other_id = other.json.id;

  const stored_preset = (await getJson(`${FILTERS_PATH}/${preset_id}`)).json;
  ok(stored_preset?.id === preset_id,
    'get by id returns the created filter state');
  ok(
    isDeepStrictEqual(stored_fields(stored_preset), {
      user_id: TEST_USER_ID,
      label: `api-test preset ${run_id}`,
      hidden: false,
      version: 1,
      state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id },
      time_deleted: null
    }),
    'get by id returns the stored fields: owned by the user, visible, active, no state hash'
  );

  const stored_other = (await getJson(`${FILTERS_PATH}/${other_id}`)).json;
  ok(
    isDeepStrictEqual(stored_fields(stored_other), {
      user_id: TEST_USER_ID,
      label: `api-test other ${run_id}`,
      hidden: false,
      version: 2,
      state: { run: run_id, other: true },
      time_deleted: null
    }),
    'get by id returns the other filter state, not the first'
  );

  const listed = await list_filter_states();
  ok(
    isDeepStrictEqual(stored_fields(listed.get(preset_id)), {
      user_id: TEST_USER_ID,
      label: `api-test preset ${run_id}`,
      hidden: false,
      version: 1,
      state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id },
      time_deleted: null
    }),
    'list returns the first filter state with its stored fields'
  );
  ok(
    isDeepStrictEqual(stored_fields(listed.get(other_id)), {
      user_id: TEST_USER_ID,
      label: `api-test other ${run_id}`,
      hidden: false,
      version: 2,
      state: { run: run_id, other: true },
      time_deleted: null
    }),
    'list returns the other filter state with its own stored fields'
  );

  const duplicate = await postJson(FILTERS_PATH, {
    label: `api-test duplicate ${run_id}`,
    version: 1,
    state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id }
  });
  ok(duplicate.res.status === 200,
    `create a duplicate state responds 200 (got ${duplicate.res.status})`);
  ok(duplicate.json.id === preset_id,
    'creating an equal state returns the existing one');
  ok(duplicate.json.label === `api-test preset ${run_id}`,
    'the existing visible label is kept');

  const hidden = await postJson(FILTERS_PATH, {
    hidden: true,
    version: 1,
    state: { run: run_id, hidden: true }
  });
  ok(hidden.res.status === 200,
    `create a hidden filter state responds 200 (got ${hidden.res.status})`);
  const hidden_id = hidden.json.id;

  ok(!(await list_filter_states()).has(hidden_id),
    'list excludes hidden filter states by default');

  const listed_with_hidden = await list_filter_states('?include_hidden=true');
  ok(
    isDeepStrictEqual(stored_fields(listed_with_hidden.get(hidden_id)), {
      user_id: TEST_USER_ID,
      label: null,
      hidden: true,
      version: 1,
      state: { run: run_id, hidden: true },
      time_deleted: null
    }),
    'list with include_hidden returns the hidden filter state, without a label'
  );
  ok(
    isDeepStrictEqual(stored_fields(listed_with_hidden.get(other_id)), {
      user_id: TEST_USER_ID,
      label: `api-test other ${run_id}`,
      hidden: false,
      version: 2,
      state: { run: run_id, other: true },
      time_deleted: null
    }),
    'list with include_hidden still returns visible filter states'
  );

  const hidden_get = await getJson(`${FILTERS_PATH}/${hidden_id}`);
  ok(hidden_get.res.status === 200,
    `a hidden filter state is returned by id (got ${hidden_get.res.status})`);

  const promoted = await postJson(FILTERS_PATH, {
    label: `api-test promoted ${run_id}`,
    version: 1,
    state: { run: run_id, hidden: true }
  });
  ok(promoted.res.status === 200,
    `create a visible duplicate of a hidden state responds 200 (got ${promoted.res.status})`);
  ok(promoted.json.id === hidden_id,
    'creating a visible duplicate of a hidden state returns the hidden one');
  ok(promoted.json.hidden === false,
    'creating a visible duplicate of a hidden state un-hides it');

  const renamed = await putJson(`${FILTERS_PATH}/${preset_id}`, { label: 'renamed' });
  ok(renamed.res.status === 200,
    `update the label responds 200 (got ${renamed.res.status})`);
  ok(renamed.json.label === 'renamed',
    'update changes the label');
  ok(
    isDeepStrictEqual(renamed.json.state, {
      filters: [{ id: 'n/fda/4', value: '', negated: false }],
      run: run_id
    }),
    'update leaves the state unchanged'
  );

  const hide = await putJson(`${FILTERS_PATH}/${preset_id}`, { hidden: true });
  ok(hide.res.status === 200 && hide.json.hidden === true,
    'update can hide a filter state');

  const show = await putJson(`${FILTERS_PATH}/${preset_id}`, { hidden: false });
  ok(show.res.status === 200 && show.json.hidden === false,
    'update can show a labeled filter state');

  const unlabeled = await postJson(FILTERS_PATH, {
    hidden: true,
    version: 1,
    state: { run: run_id, unlabeled: true }
  });
  const show_unlabeled = await putJson(`${FILTERS_PATH}/${unlabeled.json.id}`, { hidden: false });
  ok(show_unlabeled.res.status === 400,
    `showing an unlabeled filter state -> 400 (got ${show_unlabeled.res.status})`);

  const state_update = await putJson(`${FILTERS_PATH}/${preset_id}`, { state: {} });
  ok(state_update.res.status === 400,
    `updating the state -> 400 (got ${state_update.res.status})`);

  const version_update = await putJson(`${FILTERS_PATH}/${preset_id}`, { version: 2 });
  ok(version_update.res.status === 400,
    `updating the version -> 400 (got ${version_update.res.status})`);

  const empty_update = await putJson(`${FILTERS_PATH}/${preset_id}`, {});
  ok(empty_update.res.status === 400,
    `empty update -> 400 (got ${empty_update.res.status})`);

  const unknown_update = await putJson(`${FILTERS_PATH}/${UNKNOWN_ID}`, { label: 'x' });
  ok(unknown_update.res.status === 404,
    `update of an unknown filter state -> 404 (got ${unknown_update.res.status})`);

  const non_numeric_get = await getJson(`${FILTERS_PATH}/not-a-number`);
  ok(non_numeric_get.res.status === 400,
    `non-numeric filter state id -> 400 (got ${non_numeric_get.res.status})`);

  const unknown_get = await getJson(`${FILTERS_PATH}/${UNKNOWN_ID}`);
  ok(unknown_get.res.status === 404,
    `get of an unknown filter state -> 404 (got ${unknown_get.res.status})`);

  const no_label = await postJson(FILTERS_PATH, { version: 1, state: {} });
  ok(no_label.res.status === 400,
    `create a visible filter state without a label -> 400 (got ${no_label.res.status})`);

  const no_version = await postJson(FILTERS_PATH, { label: 'x', state: {} });
  ok(no_version.res.status === 400,
    `create without a version -> 400 (got ${no_version.res.status})`);

  const no_state = await postJson(FILTERS_PATH, { label: 'x', version: 1 });
  ok(no_state.res.status === 400,
    `create without a state -> 400 (got ${no_state.res.status})`);

  const too_big = await postJson(FILTERS_PATH, {
    label: 'x',
    version: 1,
    state: 'x'.repeat(70000)
  });
  ok(too_big.res.status === 413,
    `create with an oversized body -> 413 (got ${too_big.res.status})`);

  const unknown_field = await postJson(FILTERS_PATH, {
    label: 'x',
    version: 1,
    state: {},
    user_id: 'someone'
  });
  ok(unknown_field.res.status === 400,
    `create with an unknown field -> 400 (got ${unknown_field.res.status})`);

  const trash = await putJson(`${FILTERS_PATH}/trash`, [preset_id]);
  ok(trash.res.status === 200,
    `trash filter state responds 200 (got ${trash.res.status})`);

  const trashed_get = await getJson(`${FILTERS_PATH}/${preset_id}`);
  ok(trashed_get.res.status === 404,
    `a trashed filter state is not found by default (got ${trashed_get.res.status})`);

  const trashed_get_deleted = await getJson(`${FILTERS_PATH}/${preset_id}?include_deleted=true`);
  ok(trashed_get_deleted.res.status === 200 && trashed_get_deleted.json.time_deleted !== null,
    'a trashed filter state is returned with include_deleted and carries time_deleted');

  ok(!(await list_filter_states()).has(preset_id),
    'list excludes a trashed filter state');

  const listed_with_deleted = await list_filter_states('?include_deleted=true');
  const { time_deleted, ...listed_trashed } = stored_fields(listed_with_deleted.get(preset_id)) ?? {};
  ok(time_deleted !== null && time_deleted !== undefined,
    'list with include_deleted returns the trashed filter state with time_deleted');
  ok(
    isDeepStrictEqual(listed_trashed, {
      user_id: TEST_USER_ID,
      label: 'renamed',
      hidden: false,
      version: 1,
      state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id }
    }),
    'list with include_deleted returns the trashed filter state with its stored fields'
  );
  ok(
    isDeepStrictEqual(stored_fields(listed_with_deleted.get(other_id)), {
      user_id: TEST_USER_ID,
      label: `api-test other ${run_id}`,
      hidden: false,
      version: 2,
      state: { run: run_id, other: true },
      time_deleted: null
    }),
    'list with include_deleted still returns active filter states'
  );

  const trashed_update = await putJson(`${FILTERS_PATH}/${preset_id}`, { label: 'x' });
  ok(trashed_update.res.status === 404,
    `a trashed filter state cannot be updated (got ${trashed_update.res.status})`);

  const recreated = await postJson(FILTERS_PATH, {
    label: `api-test recreated ${run_id}`,
    version: 1,
    state: { filters: [{ id: 'n/fda/4', value: '', negated: false }], run: run_id }
  });
  ok(recreated.res.status === 200,
    `recreate a trashed state responds 200 (got ${recreated.res.status})`);
  ok(recreated.json.id !== preset_id,
    'a trashed state can be created again under a new id');
  const recreated_id = recreated.json.id;

  const blocked_restore = await putJson(`${FILTERS_PATH}/restore`, [preset_id]);
  ok(blocked_restore.res.status === 200,
    `restore while an equal state is active responds 200 (got ${blocked_restore.res.status})`);

  const blocked_get = await getJson(`${FILTERS_PATH}/${preset_id}`);
  ok(blocked_get.res.status === 404,
    'a trashed filter state stays trashed while an equal state is active');

  await putJson(`${FILTERS_PATH}/trash`, [recreated_id]);
  const double_restore = await putJson(`${FILTERS_PATH}/restore`, [preset_id, recreated_id]);
  ok(double_restore.res.status === 400,
    `restoring two equal trashed states -> 400 (got ${double_restore.res.status})`);

  const restore = await putJson(`${FILTERS_PATH}/restore`, [preset_id]);
  ok(restore.res.status === 200,
    `restore filter state responds 200 (got ${restore.res.status})`);

  const restored_get = await getJson(`${FILTERS_PATH}/${preset_id}`);
  ok(restored_get.res.status === 200 && restored_get.json.time_deleted === null,
    'a restored filter state is active again');

  const non_array_trash = await putJson(`${FILTERS_PATH}/trash`, { id: preset_id });
  ok(non_array_trash.res.status === 400,
    `trash with a non-array body -> 400 (got ${non_array_trash.res.status})`);
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

finish();
