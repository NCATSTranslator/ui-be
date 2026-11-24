export { test_query_submission }

import * as ast from 'node:assert';
import * as auth from '../services/AuthService.mjs';
import { QueryAPIController } from '../controllers/QueryAPIController.mjs';
import { QueryServicexFEAdapter } from '../adapters/QueryServicexFEAdapter.mjs';

const SESSION_PARAMS = {
  tokenTTLSec: 1800,
  sessionAbsoluteTTLSec: 15780000,
  sessionMaxIdleTimeSec: 15780000,
  loginRequestTTLSec: 3600,
  apiKeyTouchIntervalSec: 300
};

function make_fixture(use_pubsub) {
  const calls = { submitted: [], created: [], user_queries: [], relationships: [], project_updates: [] };
  const config = { secrets: { hmac: { key: 'test-key' } }, ars_endpoint: { use_pubsub: use_pubsub } };
  const translator_service = {
    inputToQuery(query_request) { return { message: query_request }; },
    async submitQuery(trapi_query) {
      calls.submitted.push(trapi_query);
      return { pk: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa' };
    },
    async subscribeQuery() { return {}; }
  };
  const translator_service_fe_adapter = {
    querySubmitToFE(submit_resp) { return { status: 'complete', data: submit_resp.pk }; }
  };
  const query_service = {
    async createQuery(pk, query_request) {
      calls.created.push({ pk: pk, query_request: query_request });
      return { id: 7, pk: pk, metadata: { query: query_request } };
    },
    async addQueryUserRelationship(query, user_query) {
      calls.relationships.push({ qid: query.id, uid: user_query.user_id });
      return true;
    }
  };
  const user_service = {
    async createUserQuery(user_id, pk, query_request) {
      calls.user_queries.push({ user_id: user_id, pk: pk, query_request: query_request });
      return { id: 3, user_id: user_id };
    },
    async getUserSavesBy() { return [{ id: 5, data: { pks: [] } }]; },
    async updateUserSave(project) {
      calls.project_updates.push(project);
      return project;
    }
  };
  const controller = new QueryAPIController(config, translator_service, translator_service_fe_adapter,
    query_service, new QueryServicexFEAdapter(), user_service, {},
    new auth.AuthService(SESSION_PARAMS, null, null, null));
  return { controller: controller, calls: calls };
}

function make_res() {
  return {
    code: null,
    body: null,
    status(code) { this.code = code; return this; },
    json(body) { this.body = body; return this; }
  };
}

async function test_anonymous_submit_creates_only_the_query() {
  const { controller, calls } = make_fixture(true);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: null },
    sessionData: { status: auth.SESSION_NO_TOKEN, user: null, session: null },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'an anonymous submission must be accepted');
  ast.deepStrictEqual(res.body, { status: 'complete', data: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa' },
    'an anonymous submission must return the query pk');
  ast.deepStrictEqual(calls.created, [{
    pk: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    query_request: { type: 'drug', curie: 'MONDO:0005148', pid: null }
  }], 'an anonymous submission must still create the query');
  ast.deepStrictEqual(calls.user_queries, [], 'an anonymous submission must not create a user query');
  ast.deepStrictEqual(calls.relationships, [], 'an anonymous submission must not create an ownership row');
}

async function test_logged_in_submit_creates_the_user_query() {
  const { controller, calls } = make_fixture(true);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: null },
    sessionData: { status: auth.SESSION_VALID, user: { id: 'u1' }, session: { token: 't1' } },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'a logged in submission must be accepted');
  ast.deepStrictEqual(calls.user_queries, [{
    user_id: 'u1',
    pk: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    query_request: { type: 'drug', curie: 'MONDO:0005148', pid: null }
  }], 'a logged in submission must create a user query for the session user');
  ast.deepStrictEqual(calls.relationships, [{ qid: 7, uid: 'u1' }],
    'a logged in submission must record the session user as an owner');
}

async function test_expired_session_submits_as_anonymous() {
  const { controller, calls } = make_fixture(true);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: null },
    sessionData: { status: auth.SESSION_SESSION_EXPIRED, user: { id: 'u1' }, session: { token: 't1' } },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'a submission on an expired session must be accepted');
  ast.deepStrictEqual(calls.created, [{
    pk: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    query_request: { type: 'drug', curie: 'MONDO:0005148', pid: null }
  }], 'a submission on an expired session must still create the query');
  ast.deepStrictEqual(calls.user_queries, [],
    'an expired session must not write to the history of the user it names');
  ast.deepStrictEqual(calls.relationships, [],
    'an expired session must not make the user it names an owner');
}

async function test_anonymous_submit_to_a_project_is_unauthorized() {
  const { controller, calls } = make_fixture(true);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: 5 },
    sessionData: { status: auth.SESSION_NO_TOKEN, user: null, session: null },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 401, 'an anonymous submission to a project must be unauthorized');
  ast.deepStrictEqual(calls.submitted, [], 'a rejected submission must not reach the ARS');
  ast.deepStrictEqual(calls.created, [], 'a rejected submission must not create a query');
}

async function test_logged_in_submit_to_a_project_adds_the_pk() {
  const { controller, calls } = make_fixture(true);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: 5 },
    sessionData: { status: auth.SESSION_VALID, user: { id: 'u1' }, session: { token: 't1' } },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'a logged in submission to a project must be accepted');
  ast.deepStrictEqual(calls.project_updates, [{ id: 5, data: { pks: ['aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa'] } }],
    'a logged in submission to a project must add the pk to the project');
}

async function test_anonymous_polling_submit_creates_no_user_query() {
  const { controller, calls } = make_fixture(false);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: null },
    sessionData: { status: auth.SESSION_NO_TOKEN, user: null, session: null },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'an anonymous polling submission must be accepted');
  ast.deepStrictEqual(res.body, { status: 'complete', data: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa' },
    'an anonymous polling submission must return the query pk');
  ast.deepStrictEqual(calls.user_queries, [], 'an anonymous polling submission must not create a user query');
}

async function test_logged_in_polling_submit_creates_the_user_query() {
  const { controller, calls } = make_fixture(false);
  const res = make_res();
  await controller.submit_query({
    body: { type: 'drug', curie: 'MONDO:0005148', pid: null },
    sessionData: { status: auth.SESSION_VALID, user: { id: 'u1' }, session: { token: 't1' } },
    log: { info() {}, error() {} }
  }, res);

  ast.strictEqual(res.code, 200, 'a logged in polling submission must be accepted');
  ast.deepStrictEqual(calls.user_queries, [{
    user_id: 'u1',
    pk: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    query_request: { type: 'drug', curie: 'MONDO:0005148', pid: null }
  }], 'a logged in polling submission must create a user query for the session user');
}

async function test_query_submission() {
  await test_anonymous_submit_creates_only_the_query();
  await test_logged_in_submit_creates_the_user_query();
  await test_expired_session_submits_as_anonymous();
  await test_anonymous_submit_to_a_project_is_unauthorized();
  await test_logged_in_submit_to_a_project_adds_the_pk();
  await test_anonymous_polling_submit_creates_no_user_query();
  await test_logged_in_polling_submit_creates_the_user_query();
}
