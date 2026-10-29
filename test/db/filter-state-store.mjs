import { isDeepStrictEqual } from "node:util";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user, rejects_with } from "#test/lib/db-harness.mjs";
import { FilterStateStorePostgres } from "#store/FilterStateStorePostgres.mjs";
import { make_filter_state_from_req, FilterStateRequestError } from "#model/FilterState.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const store = new FilterStateStorePostgres(pool);

const create_filter_state = async (user_id, filter_state_req) => {
  const [filter_state] = await store.create_filter_states([make_filter_state_from_req(user_id, filter_state_req)]);
  return filter_state;
};

const get_filter_state = async (user_id, id) => {
  const [filter_state] = await store.get_filter_states(user_id, true, true, [id]);
  return filter_state;
};

const list_filter_states = async (user_id, include_deleted, include_hidden) => {
  const filter_states = await store.get_filter_states(user_id, include_deleted, include_hidden);
  return new Map(filter_states.map((filter_state) => [filter_state.id, filter_state]));
};

const stored_fields = (filter_state) => {
  if (!filter_state) return filter_state;
  const { id, state_hash, time_created, time_updated, ...fields } = filter_state;
  return fields;
};

console.log("# FilterStateStorePostgres");
try {
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);

  const preset = await create_filter_state(user.id, {
    label: "preset",
    version: 1,
    state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] }
  });
  ok(Number.isInteger(preset.id),
    "a filter state is created with an integer id");
  ok(preset.state_hash?.length === 32,
    "the state hash is generated");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, preset.id)), {
      user_id: user.id,
      label: "preset",
      hidden: false,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "the created filter state is stored visible, labeled and active"
  );

  const duplicate = await create_filter_state(user.id, {
    label: "renamed",
    version: 1,
    state: { order: ["b", "a"], filters: [{ negated: false, value: "", id: "n/fda/4" }] }
  });
  ok(duplicate.id === preset.id,
    "an equal state with different key order resolves to the existing row");
  ok((await get_filter_state(user.id, preset.id)).label === "preset",
    "a duplicate of a visible preset keeps the existing label");

  const hidden_duplicate = await create_filter_state(user.id, {
    hidden: true,
    version: 1,
    state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] }
  });
  ok(hidden_duplicate.id === preset.id,
    "a hidden duplicate of a visible preset resolves to the existing row");
  ok((await get_filter_state(user.id, preset.id)).hidden === false,
    "a hidden duplicate of a visible preset leaves it visible");

  const hidden = await create_filter_state(user.id, { hidden: true, version: 1, state: ["hidden"] });
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, hidden.id)), {
      user_id: user.id,
      label: null,
      hidden: true,
      version: 1,
      state: ["hidden"],
      time_deleted: null
    }),
    "a hidden filter state is stored without a label"
  );

  const promoted = await create_filter_state(user.id, { label: "promoted", version: 1, state: ["hidden"] });
  ok(promoted.id === hidden.id,
    "a visible duplicate of a hidden state resolves to the hidden row");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, hidden.id)), {
      user_id: user.id,
      label: "promoted",
      hidden: false,
      version: 1,
      state: ["hidden"],
      time_deleted: null
    }),
    "a visible duplicate of a hidden state un-hides and labels it"
  );

  const others = await create_filter_state(other.id, {
    label: "other",
    version: 1,
    state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] }
  });
  ok(others.id !== preset.id,
    "the same state for another user is a separate row");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(other.id, others.id)), {
      user_id: other.id,
      label: "other",
      hidden: false,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "the other user's filter state is stored under that user"
  );

  const scalar = await create_filter_state(user.id, { label: "scalar", version: 3, state: "just a string" });
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, scalar.id)), {
      user_id: user.id,
      label: "scalar",
      hidden: false,
      version: 3,
      state: "just a string",
      time_deleted: null
    }),
    "a scalar JSON state round-trips"
  );

  const reversioned = await create_filter_state(user.id, { label: "reversioned", version: 4, state: "just a string" });
  ok(reversioned.id !== scalar.id,
    "the same state with a different version is a separate row");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, reversioned.id)), {
      user_id: user.id,
      label: "reversioned",
      hidden: false,
      version: 4,
      state: "just a string",
      time_deleted: null
    }),
    "the reversioned filter state is stored with its own version"
  );
  ok((await get_filter_state(user.id, scalar.id)).version === 3,
    "the original version is left unchanged by the reversioned state"
  );

  const hidden_again = await create_filter_state(user.id, { hidden: true, version: 1, state: [1, 2, 3] });

  const visible = await list_filter_states(user.id, false, false);
  ok(visible.has(preset.id) && visible.has(promoted.id),
    "listing without hidden returns visible filter states");
  ok(!visible.has(hidden_again.id),
    "listing without hidden excludes hidden filter states");
  ok(!visible.has(others.id),
    "listing is scoped to the user");

  const with_hidden = await list_filter_states(user.id, false, true);
  ok(with_hidden.has(hidden_again.id),
    "listing with hidden includes hidden filter states");

  const by_id = await store.get_filter_states(user.id, false, true, [hidden_again.id]);
  ok(isDeepStrictEqual(by_id.map((filter_state) => filter_state.id), [hidden_again.id]),
    "filter states are fetched by id");

  await store.update_filter_states(user.id, [{ id: preset.id, fields: { label: "updated" } }]);
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, preset.id)), {
      user_id: user.id,
      label: "updated",
      hidden: false,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "an update changes only the given fields"
  );

  await store.update_filter_states(user.id, [{ id: preset.id, fields: { hidden: true } }]);
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, preset.id)), {
      user_id: user.id,
      label: "updated",
      hidden: true,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "an update can hide a filter state"
  );

  ok(
    await rejects_with(
      store.update_filter_states(user.id, [{ id: hidden_again.id, fields: { hidden: false } }]),
      FilterStateRequestError),
    "an unlabeled filter state cannot be made visible"
  );
  ok((await get_filter_state(user.id, hidden_again.id)).hidden === true,
    "a rejected update leaves the filter state hidden");

  await store.update_filter_states(user.id, [{ id: hidden_again.id, fields: { label: "shown", hidden: false } }]);
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, hidden_again.id)), {
      user_id: user.id,
      label: "shown",
      hidden: false,
      version: 1,
      state: [1, 2, 3],
      time_deleted: null
    }),
    "an unlabeled filter state can be labeled and shown at once"
  );

  const not_owned = await store.update_filter_states(user.id, [{ id: others.id, fields: { label: "stolen" } }]);
  ok(not_owned.length === 0,
    "updating another user's filter state returns nothing");
  ok((await get_filter_state(other.id, others.id)).label === "other",
    "another user's filter state is left unchanged");

  const trashed_ids = await store.trash_filter_states(user.id, [preset.id, others.id]);
  ok(isDeepStrictEqual(trashed_ids, [preset.id]),
    "trash affects only the user's own filter states");
  ok((await get_filter_state(other.id, others.id)).time_deleted === null,
    "another user's filter state stays active after trash");

  ok(!(await list_filter_states(user.id, false, true)).has(preset.id),
    "a trashed filter state is excluded by default");

  const with_deleted = await list_filter_states(user.id, true, true);
  ok(with_deleted.get(preset.id)?.time_deleted instanceof Date,
    "a trashed filter state is included with include_deleted and carries time_deleted");

  const trashed_update = await store.update_filter_states(user.id, [{ id: preset.id, fields: { label: "x" } }]);
  ok(trashed_update.length === 0,
    "updating a trashed filter state returns nothing");
  ok((await get_filter_state(user.id, preset.id)).label === "updated",
    "a trashed filter state is left unchanged");

  const trashed_before = await get_filter_state(user.id, preset.id);
  const resaved_hidden = await create_filter_state(user.id, {
    hidden: true,
    version: 1,
    state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] }
  });
  ok(resaved_hidden.id === preset.id,
    "a hidden duplicate of a trashed preset resolves to the trashed row");
  ok(
    isDeepStrictEqual(stored_fields(resaved_hidden), {
      user_id: user.id,
      label: "updated",
      hidden: true,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: resaved_hidden.time_deleted
    }) && resaved_hidden.time_deleted instanceof Date,
    "a hidden duplicate leaves the trashed preset trashed with its fields intact"
  );
  ok(resaved_hidden.time_updated.getTime() === trashed_before.time_updated.getTime()
    && !(await list_filter_states(user.id, false, true)).has(preset.id),
    "a hidden duplicate does not restore or touch the trashed preset");

  const restored_ids = await store.restore_filter_states(user.id, [preset.id]);
  ok(isDeepStrictEqual(restored_ids, [preset.id]),
    "a trashed filter state is restored");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, preset.id)), {
      user_id: user.id,
      label: "updated",
      hidden: true,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "a restored filter state is active again with its fields intact"
  );

  await store.trash_filter_states(user.id, [preset.id]);
  const resaved = await create_filter_state(user.id, {
    label: "recreated",
    version: 1,
    state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] }
  });
  ok(resaved.id === preset.id,
    "a visible duplicate of a trashed preset resolves to the trashed row");
  ok(
    isDeepStrictEqual(stored_fields(await get_filter_state(user.id, preset.id)), {
      user_id: user.id,
      label: "recreated",
      hidden: false,
      version: 1,
      state: { filters: [{ id: "n/fda/4", value: "", negated: false }], order: ["b", "a"] },
      time_deleted: null
    }),
    "a visible duplicate restores the trashed preset with the new label"
  );

  const unlabeled = await create_filter_state(user.id, { hidden: true, version: 1, state: ["unlabeled"] });
  ok(isDeepStrictEqual(await store.trash_filter_states(user.id, [unlabeled.id, promoted.id]), [promoted.id]),
    "trash skips filter states without a label");
  ok((await get_filter_state(user.id, unlabeled.id)).time_deleted === null,
    "an unlabeled filter state stays active");
  ok(isDeepStrictEqual(await store.restore_filter_states(user.id, [promoted.id]), [promoted.id]),
    "a labeled filter state is trashed and restored");

  await store.trash_filter_states(user.id, [scalar.id]);
  const restored_reversioned_ids = await store.restore_filter_states(user.id, [scalar.id]);
  ok(isDeepStrictEqual(restored_reversioned_ids, [scalar.id]),
    "a trashed filter state is restored while an equal state with another version is active");
  ok((await get_filter_state(user.id, scalar.id)).time_deleted === null,
    "the restored filter state is active alongside the other version");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
