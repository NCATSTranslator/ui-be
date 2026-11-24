export { test_query_reconcile }

import * as ast from 'node:assert';
import { main } from '../utilities/node/reconcile-query-state.mjs';

function make_fixture(stale_queries, notifications) {
  const calls = { requested_pks: [], updates: [], lost_pks: [] };
  const translator_service = {
    async get_notification_statuses(pks) {
      calls.requested_pks.push(pks);
      return [null, notifications];
    }
  };
  const query_service = {
    async get_stale_queries() { return stale_queries; },
    async UNSAFE_batch_update(queries) {
      calls.updates.push(queries.map((query) => ({
        pk: query.pk,
        status: query.status,
        aras: query.metadata.aras
      })));
      return queries.length;
    },
    async delete_lost_queries(pks) {
      calls.lost_pks.push(pks);
      return pks.length;
    }
  };
  return { translator_service: translator_service, query_service: query_service, calls: calls };
}

async function test_no_stale_queries_skips_the_ars() {
  const { translator_service, query_service, calls } = make_fixture([], []);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 0, delete_count: 0 },
    'a run with no stale queries must report no changes');
  ast.deepStrictEqual(calls.requested_pks, [], 'a run with no stale queries must not ask the ARS anything');
}

async function test_unchanged_running_query_is_left_alone() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: 'Running', merged_list: [['mv-1', 'ara-a']] }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 0, delete_count: 0 },
    'a query the ARS still reports as running with the same ARAs must not count as a change');
  ast.deepStrictEqual(calls.requested_pks, [['pk-1']], 'the ARS must be asked about the stale pk');
  ast.deepStrictEqual(calls.updates, [], 'an unchanged running query must not be written');
  ast.deepStrictEqual(calls.lost_pks, [], 'an unchanged running query must not be deleted');
}

async function test_running_query_gains_new_aras() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: 'Running', merged_list: [['mv-1', 'ara-a'], ['mv-2', 'ara-b']] }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 1, delete_count: 0 },
    'a running query with a new ARA must count as one update');
  ast.deepStrictEqual(calls.updates, [[{ pk: 'pk-1', status: 'running', aras: ['ara-a', 'ara-b'] }]],
    'a running query must be written with the ARAs the ARS has merged');
}

async function test_done_query_is_completed() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: 'Done', merged_list: [['mv-1', 'ara-a']] }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 1, delete_count: 0 },
    'a query the ARS reports as done must count as one update');
  ast.deepStrictEqual(calls.updates, [[{ pk: 'pk-1', status: 'complete', aras: ['ara-a'] }]],
    'a query the ARS reports as done must be written as complete');
}

async function test_errored_query_without_a_merged_list_keeps_its_aras() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: 'Error', merged_list: null }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 1, delete_count: 0 },
    'a query the ARS reports as errored must count as one update');
  ast.deepStrictEqual(calls.updates, [[{ pk: 'pk-1', status: 'error', aras: ['ara-a'] }]],
    'an errored query with no merged list must be written as error with its existing ARAs');
}

async function test_query_without_an_ars_record_is_lost() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: null, merged_list: null }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 0, delete_count: 1 },
    'a query the ARS has no record of must count as one delete');
  ast.deepStrictEqual(calls.lost_pks, [['pk-1']], 'a query the ARS has no record of must be deleted');
  ast.deepStrictEqual(calls.updates, [], 'a query the ARS has no record of must not be written');
}

async function test_query_with_an_unknown_status_is_lost() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-1', status: 'Queued', merged_list: [['mv-1', 'ara-a'], ['mv-2', 'ara-b']] }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 0, delete_count: 1 },
    'a query with an unknown ARS status must count as one delete');
  ast.deepStrictEqual(calls.lost_pks, [['pk-1']], 'a query with an unknown ARS status must be deleted');
  ast.deepStrictEqual(calls.updates, [], 'a query with an unknown ARS status must not be written');
}

async function test_unrequested_pk_is_ignored() {
  const { translator_service, query_service, calls } = make_fixture(
    [{ pk: 'pk-1', status: 'running', metadata: { aras: ['ara-a'] } }],
    [{ pk: 'pk-2', status: null, merged_list: null }, { pk: 'pk-3', status: 'Done', merged_list: [['mv-1', 'ara-a']] }]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 0, delete_count: 0 },
    'records for pks that were not asked about must not count as changes');
  ast.deepStrictEqual(calls.lost_pks, [], 'a pk that was not asked about must not be deleted');
  ast.deepStrictEqual(calls.updates, [], 'a pk that was not asked about must not be written');
}

async function test_each_query_gets_its_own_outcome() {
  const { translator_service, query_service, calls } = make_fixture(
    [
      { pk: 'pk-1', status: 'running', metadata: { aras: [] } },
      { pk: 'pk-2', status: 'running', metadata: { aras: ['ara-a'] } },
      { pk: 'pk-3', status: 'running', metadata: { aras: ['ara-a'] } },
      { pk: 'pk-4', status: 'running', metadata: { aras: [] } }
    ],
    [
      { pk: 'pk-1', status: 'Done', merged_list: [['mv-1', 'ara-b']] },
      { pk: 'pk-2', status: null, merged_list: null },
      { pk: 'pk-3', status: 'Running', merged_list: [['mv-2', 'ara-a']] },
      { pk: 'pk-4', status: 'Error', merged_list: null }
    ]);
  const counts = await main(translator_service, query_service);

  ast.deepStrictEqual(counts, { update_count: 2, delete_count: 1 },
    'a mixed batch must count its updates and deletes separately');
  ast.deepStrictEqual(calls.requested_pks, [['pk-1', 'pk-2', 'pk-3', 'pk-4']],
    'the ARS must be asked about every stale pk in one request');
  ast.deepStrictEqual(calls.updates, [[
    { pk: 'pk-1', status: 'complete', aras: ['ara-b'] },
    { pk: 'pk-4', status: 'error', aras: [] }
  ]], 'only the queries whose state changed must be written, each with its own state');
  ast.deepStrictEqual(calls.lost_pks, [['pk-2']], 'only the query without an ARS record must be deleted');
}

async function test_query_reconcile() {
  await test_no_stale_queries_skips_the_ars();
  await test_unchanged_running_query_is_left_alone();
  await test_running_query_gains_new_aras();
  await test_done_query_is_completed();
  await test_errored_query_without_a_merged_list_keeps_its_aras();
  await test_query_without_an_ars_record_is_lost();
  await test_query_with_an_unknown_status_is_lost();
  await test_unrequested_pk_is_ignored();
  await test_each_query_gets_its_own_outcome();
}
