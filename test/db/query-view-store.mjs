import { isDeepStrictEqual } from "node:util";
import { randomUUID } from "node:crypto";
import { createHarness } from "#test/lib/api-harness.mjs";
import { connect_test_db, create_test_user, create_test_query, rejects_with } from "#test/lib/db-harness.mjs";
import { FilterStateStorePostgres } from "#store/FilterStateStorePostgres.mjs";
import { QueryViewStorePostgres } from "#store/QueryViewStorePostgres.mjs";
import { UserService } from "../../services/UserService.mjs";
import { make_filter_state_from_req } from "#model/FilterState.mjs";
import { QueryViewRequestError } from "#model/QueryView.mjs";

const { ok, fail, finish } = createHarness();
const { pool } = await connect_test_db();
const filter_store = new FilterStateStorePostgres(pool);
const store = new QueryViewStorePostgres(pool);
const service = new UserService(null, null, null, null, null, null, filter_store, store);

const create_filter_state = async (user_id, filter_state_req) => {
  const [filter_state] = await filter_store.create_filter_states([make_filter_state_from_req(user_id, filter_state_req)]);
  return filter_state;
};

const stored_fields = (query_view) => {
  if (!query_view) return query_view;
  const { id, time_created, time_updated, filter_state, ...fields } = query_view;
  const { time_created: fs_created, time_updated: fs_updated, ...filter_fields } = filter_state;
  return { ...fields, filter_state: filter_fields };
};

console.log("# QueryViewStorePostgres");
try {
  const user = await create_test_user(pool);
  const other = await create_test_user(pool);
  const query_a = await create_test_query(pool, user.id);
  const query_b = await create_test_query(pool, user.id);
  const query_c = await create_test_query(pool, other.id);

  const preset = await create_filter_state(user.id, { label: "preset", version: 1, state: { filters: ["n/fda/4"] } });
  const hidden = await create_filter_state(user.id, { hidden: true, version: 1, state: { filters: ["p/pc/1"] } });
  const others_preset = await create_filter_state(other.id, { label: "other", version: 1, state: { filters: [] } });

  const view_a1 = await store.create_query_view(user.id, query_a.pk, preset.id);
  ok(Number.isInteger(view_a1.id),
    "a query view is created with an integer id");
  ok(
    isDeepStrictEqual(stored_fields(await store.get_query_view(query_a.pk, view_a1.id)), {
      user_id: user.id,
      pk: query_a.pk,
      filter_state: {
        id: preset.id,
        user_id: user.id,
        label: "preset",
        hidden: false,
        version: 1,
        state: { filters: ["n/fda/4"] },
        time_deleted: null
      },
      time_deleted: null
    }),
    "the created query view is stored active with its filter state inline"
  );

  const view_a2 = await store.create_query_view(user.id, query_a.pk, hidden.id);
  ok(view_a2.id !== view_a1.id,
    "a view with another filter state is a separate row");
  ok(
    isDeepStrictEqual(stored_fields(await store.get_query_view(query_a.pk, view_a2.id)), {
      user_id: user.id,
      pk: query_a.pk,
      filter_state: {
        id: hidden.id,
        user_id: user.id,
        label: null,
        hidden: true,
        version: 1,
        state: { filters: ["p/pc/1"] },
        time_deleted: null
      },
      time_deleted: null
    }),
    "a view can use a hidden filter state"
  );

  const view_b = await store.create_query_view(user.id, query_b.pk, preset.id);
  ok(view_b.id !== view_a1.id,
    "the same filter state on another query is a separate row");

  const duplicate = await store.create_query_view(user.id, query_a.pk, preset.id);
  ok(duplicate.id === view_a1.id,
    "creating the same view again resolves to the existing row");
  ok(duplicate.time_updated > view_a1.time_updated,
    "creating the same view again bumps time_updated");

  ok(
    isDeepStrictEqual((await store.get_query_views(user.id, query_a.pk, false)).map((query_view) => query_view.id),
      [view_a1.id, view_a2.id]),
    "listing a query's views returns them newest time_updated first and excludes other queries"
  );
  ok((await store.get_query_views(other.id, query_a.pk, true)).length === 0,
    "listing is scoped to the user");
  ok((await store.get_query_views(user.id, randomUUID(), true)).length === 0,
    "listing an unknown query returns nothing");
  ok((await store.get_query_view(query_a.pk, view_a2.id)).id === view_a2.id,
    "a view is fetched by query and id without a user");
  ok((await store.get_query_view(query_a.pk, view_a2.id)).filter_state.id === hidden.id,
    "the fetched view carries its filter state");
  ok((await store.get_query_view(query_b.pk, view_a2.id)) === null,
    "a view is not fetched through another query");
  ok((await store.get_query_view(randomUUID(), view_a2.id)) === null,
    "fetching a view of an unknown query returns null");
  ok((await store.get_query_view(query_a.pk, 999999999)) === null,
    "fetching an unknown view returns null");

  ok((await store.create_query_view(user.id, randomUUID(), preset.id)) === null,
    "creating a view for an unknown query returns null");
  ok((await store.create_query_view(user.id, query_c.pk, preset.id)) === null,
    "creating a view for another user's query returns null");
  ok(await rejects_with(store.create_query_view(user.id, query_a.pk, others_preset.id), QueryViewRequestError),
    "creating a view with another user's filter state is rejected");
  ok(await rejects_with(store.create_query_view(user.id, query_a.pk, 999999999), QueryViewRequestError),
    "creating a view with an unknown filter state is rejected");
  ok((await store.get_query_views(user.id, query_a.pk, true)).length === 2,
    "rejected creates add no views");

  const from_state = await service.create_query_view(user.id, query_a.pk, { version: 2, state: { filters: ["e/pred/x"] } });
  ok(
    isDeepStrictEqual(stored_fields(await store.get_query_view(query_a.pk, from_state.id)), {
      user_id: user.id,
      pk: query_a.pk,
      filter_state: {
        id: from_state.filter_state.id,
        user_id: user.id,
        label: null,
        hidden: true,
        version: 2,
        state: { filters: ["e/pred/x"] },
        time_deleted: null
      },
      time_deleted: null
    }),
    "the service saves a version and state as a hidden filter state and views it"
  );
  ok(!(await filter_store.get_filter_states(user.id, false, false)).some((filter_state) => filter_state.id === from_state.filter_state.id),
    "the filter state saved for a view is not listed as a preset");
  const from_state_again = await service.create_query_view(user.id, query_a.pk, { version: 2, state: { filters: ["e/pred/x"] } });
  ok(from_state_again.id === from_state.id && from_state_again.filter_state.id === from_state.filter_state.id,
    "the service resolves the same version and state to the existing filter state and view");
  const from_preset = await service.create_query_view(user.id, query_a.pk, { version: 1, state: { filters: ["n/fda/4"] } });
  ok(from_preset.id === view_a1.id && from_preset.filter_state.label === "preset",
    "the service resolves a version and state equal to a preset to the preset's view");
  const from_id = await service.create_query_view(user.id, query_b.pk, { filter_state_id: hidden.id });
  ok(from_id.filter_state.id === hidden.id && from_id.pk === query_b.pk,
    "the service creates a view from a filter state id");

  const trashed_ids = await store.trash_query_views(user.id, query_a.pk, [view_a1.id, view_b.id]);
  ok(isDeepStrictEqual(trashed_ids, [view_a1.id]),
    "trash affects only the views of the given query");
  ok((await store.get_query_view(query_b.pk, view_b.id)).time_deleted === null,
    "a view of another query stays active after trash");
  ok(!(await store.get_query_views(user.id, query_a.pk, false)).some((query_view) => query_view.id === view_a1.id),
    "a trashed view is excluded by default");
  ok((await store.get_query_view(query_a.pk, view_a1.id)).time_deleted instanceof Date,
    "a trashed view is included with include_deleted and carries time_deleted");

  ok((await store.trash_query_views(other.id, query_a.pk, [view_a2.id])).length === 0,
    "trashing another user's view returns nothing");
  ok((await store.get_query_view(query_a.pk, view_a2.id)).time_deleted === null,
    "another user's view is left active");

  const recreated = await store.create_query_view(user.id, query_a.pk, preset.id);
  ok(recreated.id !== view_a1.id,
    "a trashed view can be created again as a new row");
  ok((await store.restore_query_views(user.id, query_a.pk, [view_a1.id])).length === 0,
    "restore returns nothing while an equal view is active");
  ok((await store.get_query_view(query_a.pk, view_a1.id)).time_deleted !== null,
    "a trashed view stays trashed while an equal view is active");

  await store.trash_query_views(user.id, query_a.pk, [recreated.id]);
  ok(await rejects_with(store.restore_query_views(user.id, query_a.pk, [view_a1.id, recreated.id]), QueryViewRequestError),
    "restoring two trashed views with the same filter state is rejected");
  ok(isDeepStrictEqual(await store.restore_query_views(user.id, query_a.pk, [view_a1.id]), [view_a1.id]),
    "a trashed view is restored once no equal view is active");
  ok((await store.get_query_view(query_a.pk, view_a1.id)).time_deleted === null,
    "a restored view is active again");
  ok((await store.restore_query_views(user.id, query_b.pk, [recreated.id])).length === 0,
    "restore affects only the views of the given query");

  await filter_store.trash_filter_states(user.id, [hidden.id]);
  const on_trashed_filter = await store.create_query_view(user.id, query_b.pk, hidden.id);
  ok(on_trashed_filter.id === from_id.id && on_trashed_filter.filter_state.time_deleted instanceof Date,
    "a view keeps using a trashed filter state and reports it as trashed");
} catch (err) {
  fail(`unexpected error: ${err.stack}`);
}

await pool.end();
finish();
