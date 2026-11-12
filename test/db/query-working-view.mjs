import { randomUUID } from "node:crypto";
import { isDeepStrictEqual } from "node:util";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user, create_test_query, rejects_with } from "#test/lib/db-harness.mjs";
import { FilterStateStorePostgres } from "#store/FilterStateStorePostgres.mjs";
import { QueryViewStorePostgres } from "#store/QueryViewStorePostgres.mjs";
import { UserService } from "../../services/UserService.mjs";
import { make_filter_state_from_req } from "#model/FilterState.mjs";
import { WorkingViewRequestError } from "#model/WorkingView.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const filter_store = new FilterStateStorePostgres(pool);
const store = new QueryViewStorePostgres(pool);
const service = new UserService(null, null, null, null, null, null, filter_store, store);

const create_filter_state = async (user_id, filter_state_req) => {
  const [filter_state] = await filter_store.create_filter_states([make_filter_state_from_req(user_id, filter_state_req)]);
  return filter_state;
};

const get_filter_state = async (user_id, id) => {
  const [filter_state] = await filter_store.get_filter_states(user_id, true, true, [id]);
  return filter_state;
};

console.log("# QueryViewStorePostgres working view");
try {
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);
  const query_a = await create_test_query(pool, user.id);
  const query_b = await create_test_query(pool, user.id);
  const query_c = await create_test_query(pool, other.id);

  const preset = await create_filter_state(user.id, { label: "preset", version: 1, state: { filters: ["n/fda/4"] } });
  const hidden = await create_filter_state(user.id, { hidden: true, version: 1, state: { filters: ["p/pc/1"] } });
  const others_preset = await create_filter_state(other.id, { label: "other", version: 1, state: { filters: [] } });

  ok((await store.get_working_view(user.id, query_a.pk)) === null,
    "a query with no working view returns null");

  const set_a = await store.set_working_view(user.id, query_a.pk, 1, { filters: ["n/fda/4"] });
  ok(set_a.user_id === user.id && set_a.pk === query_a.pk && set_a.time_updated instanceof Date,
    "setting the working view returns it with the user, query and time_updated");
  ok(set_a.version === 1 && isDeepStrictEqual(set_a.state, { filters: ["n/fda/4"] }),
    "the returned working view carries the version and state");
  ok(isDeepStrictEqual((await store.get_working_view(user.id, query_a.pk)).state, { filters: ["n/fda/4"] }),
    "the working view is read back");
  ok((await store.get_working_view(user.id, query_b.pk)) === null,
    "another query of the user is unaffected");
  ok((await store.get_working_view(other.id, query_a.pk)) === null,
    "another user does not see the working view");

  const replaced = await store.set_working_view(user.id, query_a.pk, 2, "just a string");
  ok(replaced.version === 2 && replaced.state === "just a string" && replaced.time_updated > set_a.time_updated,
    "setting again replaces the version and state and bumps time_updated");
  ok((await store.get_working_view(user.id, query_a.pk)).state === "just a string",
    "the replacement is read back");

  await store.set_working_view(user.id, query_b.pk, 1, [1, 2, 3]);
  ok(isDeepStrictEqual((await store.get_working_view(user.id, query_b.pk)).state, [1, 2, 3])
    && (await store.get_working_view(user.id, query_a.pk)).state === "just a string",
    "each query keeps its own working view");

  ok((await store.set_working_view(user.id, randomUUID(), 1, {})) === null,
    "setting the working view of an unknown query returns null");
  ok((await store.set_working_view(user.id, query_c.pk, 1, {})) === null,
    "setting the working view of another user's query returns null");

  const from_state = await service.set_working_view(user.id, query_a.pk, { version: 3, state: { filters: ["e/pred/x"] } });
  ok(from_state.version === 3 && isDeepStrictEqual(from_state.state, { filters: ["e/pred/x"] }),
    "the service saves a version and state");
  ok((await filter_store.get_filter_states(user.id, true, true)).every(
      (filter_state) => !isDeepStrictEqual(filter_state.state, { filters: ["e/pred/x"] })),
    "the service does not create a filter state for a working view");
  const from_id = await service.set_working_view(user.id, query_b.pk, { filter_state_id: preset.id });
  ok(from_id.version === 1 && isDeepStrictEqual(from_id.state, { filters: ["n/fda/4"] }) && from_id.pk === query_b.pk,
    "the service copies the version and state of a filter state id");
  const from_hidden_id = await service.set_working_view(user.id, query_b.pk, { filter_state_id: hidden.id });
  ok(isDeepStrictEqual(from_hidden_id.state, { filters: ["p/pc/1"] }),
    "the service copies a hidden filter state");
  ok(await rejects_with(service.set_working_view(user.id, query_a.pk, { filter_state_id: others_preset.id }),
      WorkingViewRequestError),
    "setting from another user's filter state is rejected");
  ok(await rejects_with(service.set_working_view(user.id, query_a.pk, { filter_state_id: 999999999 }),
      WorkingViewRequestError),
    "setting from an unknown filter state is rejected");
  ok(isDeepStrictEqual((await store.get_working_view(user.id, query_a.pk)).state, { filters: ["e/pred/x"] }),
    "rejected sets leave the working view unchanged");

  const snapshot = await service.snapshot_working_view(user.id, query_a.pk);
  ok(Number.isInteger(snapshot.id) && snapshot.pk === query_a.pk,
    "a snapshot is a query view");
  ok(snapshot.filter_state.hidden === true && snapshot.filter_state.label === null
    && snapshot.filter_state.version === 3 && isDeepStrictEqual(snapshot.filter_state.state, { filters: ["e/pred/x"] }),
    "a snapshot saves the working view as a hidden filter state");
  ok((await service.snapshot_working_view(user.id, query_a.pk)).id === snapshot.id,
    "snapshotting the same working view again returns the same query view");
  await service.set_working_view(user.id, query_a.pk, { filter_state_id: preset.id });
  ok((await store.get_query_view(query_a.pk, snapshot.id)).filter_state.version === 3,
    "the snapshot keeps its filter state after the working view changes");
  const preset_snapshot = await service.snapshot_working_view(user.id, query_a.pk);
  ok(preset_snapshot.id !== snapshot.id && preset_snapshot.filter_state.id === preset.id,
    "snapshotting a working view equal to a preset views the preset");
  ok((await get_filter_state(user.id, preset.id)).hidden === false,
    "snapshotting leaves the preset visible");
  ok((await service.snapshot_working_view(user.id, query_c.pk)) === null,
    "snapshotting a query without a working view returns null");

  await filter_store.trash_filter_states(user.id, [preset.id]);
  ok(isDeepStrictEqual((await store.get_working_view(user.id, query_a.pk)).state, { filters: ["n/fda/4"] }),
    "trashing a preset leaves the working view content in place");
  const trashed_snapshot = await service.snapshot_working_view(user.id, query_b.pk);
  ok(trashed_snapshot.filter_state.id === hidden.id,
    "a snapshot equal to a hidden filter state views it");
  await service.set_working_view(user.id, query_b.pk, { version: 1, state: { filters: ["n/fda/4"] } });
  const on_trashed = await service.snapshot_working_view(user.id, query_b.pk);
  ok(on_trashed.filter_state.id === preset.id && on_trashed.filter_state.time_deleted instanceof Date,
    "a snapshot equal to a trashed preset views the trashed preset");
  const trashed_preset = await get_filter_state(user.id, preset.id);
  ok(trashed_preset.label === "preset" && trashed_preset.hidden === false && trashed_preset.time_deleted instanceof Date,
    "the trashed preset is left untouched by the snapshot");
  ok(isDeepStrictEqual(await filter_store.restore_filter_states(user.id, [preset.id]), [preset.id]),
    "the trashed preset can still be restored");
  ok((await store.get_query_view(query_b.pk, on_trashed.id)).filter_state.time_deleted === null,
    "the snapshot sees the restored preset");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
