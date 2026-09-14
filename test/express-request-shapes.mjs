export { test_express_request_shapes }

import * as ast from 'node:assert';
import { default as express } from 'express';
import * as wutil from '../lib/webutils.mjs';
import { QueryAPIController } from '../controllers/QueryAPIController.mjs';
import { UserAPIController } from '../controllers/UserAPIController.mjs';
import { BiolinkAPIController } from '../controllers/BiolinkAPIController.mjs';
import { load_biolink } from '#lib/biolink-model.mjs';
import { _test_biolink_config } from '#test/data/biolink-model.mjs';

const QID = 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa';
const RESULT_ROUTE = '/api/v1/query/:qid/result';
const PARAMLESS_ROUTE = '/api/v1/query/result';

const CONFIG = {
  secrets: { hmac: { key: 'test-key' } },
  ars_endpoint: { use_pubsub: false },
  max_hops: 3,
  ara_to_infores_map: {}
};

function make_user_service() {
  return {
    async getUserSavesBy() { return null; },
    async updateUserSave() { return null; }
  };
}

function make_query_controller() {
  const translator_service = {
    async getResults(qid) { return { qid: qid }; }
  };
  const translator_service_fe_adapter = {
    async queryResultsToFE(results) { return { data: { qid: results.qid } }; }
  };
  return new QueryAPIController(CONFIG, translator_service, translator_service_fe_adapter,
    null, null, make_user_service(), { whitelistRx: /^ara-/ });
}

function make_app() {
  const query_api = make_query_controller();
  const user_api = new UserAPIController(CONFIG, make_user_service(), null);
  const uncaught = [];
  const app = express();
  app.use(express.json());
  app.use((req, _res, next) => {
    req.log = { info: () => {}, warn: () => {}, error: () => {} };
    req.sessionData = { user: { id: 'u1' } };
    next();
  });
  const result_handler = query_api.get_query_result.bind(query_api);
  app.get(RESULT_ROUTE, result_handler);
  app.get(PARAMLESS_ROUTE, result_handler);
  app.post('/api/v1/users/me/queries/copy', query_api.copy_user_query.bind(query_api));
  app.put('/api/v1/users/me/queries/touch', query_api.touch_user_query.bind(query_api));
  app.put('/api/v1/users/me/queries', query_api.update_user_query.bind(query_api));
  app.put('/api/v1/users/me/queries/trash', query_api.delete_user_queries.bind(query_api));
  app.post('/api/v1/users/me/projects', user_api.create_user_project.bind(user_api));
  const biolink_api = new BiolinkAPIController();
  app.post('/api/v1/biolink/node/description', biolink_api.get_node_descriptions.bind(biolink_api));
  app.get('/api/v1/users/me/saves', (req, res) => {
    const injected = wutil.inject_query_params(req, { type: 'query' });
    return res.json({ type: injected.query.type, include_deleted: req.query.include_deleted });
  });
  app.use((err, _req, res, _next) => {
    uncaught.push(err);
    if (res.headersSent) return;
    res.status(500).send('uncaught');
  });
  return { app: app, uncaught: uncaught };
}

async function with_server(app, run) {
  const server = app.listen(0, '127.0.0.1');
  await new Promise((resolve) => server.once('listening', resolve));
  try {
    return await run(`http://127.0.0.1:${server.address().port}`);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
}

async function test_route_params_reach_the_translator_service() {
  const { app, uncaught } = make_app();
  await with_server(app, async (base) => {
    const res = await fetch(`${base}/api/v1/query/${QID}/result`);
    const raw = await res.text();
    ast.strictEqual(res.status, 200,
      `a well formed result request must not fail (got ${res.status}: ${raw.slice(0, 120)})`);
    ast.strictEqual(JSON.parse(raw).data.qid, QID,
      'the routed qid must reach the translator service');
  });
  ast.deepStrictEqual(uncaught.map((err) => err.message), [],
    'reading routed params must not throw');
}

async function test_missing_qid_is_a_bad_request() {
  await with_server(make_app().app, async (base) => {
    const res = await fetch(`${base}${PARAMLESS_ROUTE}`);
    ast.strictEqual(res.status, 400,
      `a request with no qid must be rejected as malformed (got ${res.status})`);
  });
}

function test_validation_tolerates_null_prototype_params() {
  const controller = make_query_controller();
  const routed = Object.assign(Object.create(null), { qid: QID });
  ast.ok(controller._is_valid_query_result_request({ params: routed }),
    'a routed qid on a null prototype params object must validate');
  ast.ok(!controller._is_valid_query_result_request({ params: Object.create(null) }),
    'an absent qid must not validate');
  ast.ok(!controller._is_valid_query_result_request({ params: Object.assign(Object.create(null), { qid: '' }) }),
    'an empty qid must not validate');
}

async function test_body_less_requests_are_rejected_cleanly() {
  const { app, uncaught } = make_app();
  const cases = [
    ['POST', '/api/v1/users/me/queries/copy', 400],
    ['POST', '/api/v1/users/me/projects', 400],
    ['PUT', '/api/v1/users/me/queries', 400],
    ['PUT', '/api/v1/users/me/queries/trash', 400],
    ['POST', '/api/v1/biolink/node/description', 400]
  ];
  await with_server(app, async (base) => {
    for (const [method, path, expected] of cases) {
      const res = await fetch(`${base}${path}`, { method: method });
      const raw = await res.text();
      ast.strictEqual(res.status, expected,
        `${method} ${path} with no body must answer ${expected} (got ${res.status}: ${raw.slice(0, 120)})`);
    }
    const touched = await fetch(`${base}/api/v1/users/me/queries/touch`, { method: 'PUT' });
    const raw = await touched.text();
    ast.ok(raw !== 'uncaught',
      `PUT queries/touch with no body must be handled, not thrown (got ${raw.slice(0, 120)})`);
  });
  ast.deepStrictEqual(uncaught.map((err) => err.message), [],
    'a request with no body must never reach the error handler');
}

async function test_wrong_content_type_is_rejected_cleanly() {
  const { app, uncaught } = make_app();
  await with_server(app, async (base) => {
    const res = await fetch(`${base}/api/v1/users/me/queries/copy`, {
      method: 'POST',
      headers: { 'Content-Type': 'text/plain' },
      body: 'not json'
    });
    ast.strictEqual(res.status, 400,
      `an unparsed body must be rejected as malformed (got ${res.status})`);
  });
  ast.deepStrictEqual(uncaught.map((err) => err.message), [],
    'an unparsed body must never reach the error handler');
}

async function test_injected_query_params_survive_the_express_getter() {
  await with_server(make_app().app, async (base) => {
    const defaulted = await (await fetch(`${base}/api/v1/users/me/saves?include_deleted=true`)).json();
    ast.strictEqual(defaulted.type, 'query',
      'an injected query param must be readable back off the request');
    ast.strictEqual(defaulted.include_deleted, 'true',
      'injecting a param must preserve the params the client sent');

    const supplied = await (await fetch(`${base}/api/v1/users/me/saves?type=tag`)).json();
    ast.strictEqual(supplied.type, 'tag',
      'injection must not overwrite a param the client sent');
  });
}

async function test_non_array_body_is_rejected_cleanly() {
  const { app, uncaught } = make_app();
  const path = '/api/v1/biolink/node/description';
  await with_server(app, async (base) => {
    for (const body of ['{"biolink:Disease": true}', '{}']) {
      const res = await fetch(`${base}${path}`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: body
      });
      ast.strictEqual(res.status, 400,
        `a non array body (${body}) must be rejected as malformed (got ${res.status})`);
    }

    const valid = await fetch(`${base}${path}`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify(['biolink:Disease', 'NotAType'])
    });
    ast.strictEqual(valid.status, 200, `an array body must still be served (got ${valid.status})`);
    const mapping = await valid.json();
    ast.ok(mapping['biolink:Disease'],
      'a known node type must still resolve to a description');
    ast.strictEqual(mapping['NotAType'], null,
      'an unknown node type must still map to null');
  });
  ast.deepStrictEqual(uncaught.map((err) => err.message), [],
    'a non array body must never reach the error handler');
}

async function test_express_request_shapes() {
  console.log('START MODULE TEST express 5 request shapes');
  await load_biolink(_test_biolink_config());
  await test_route_params_reach_the_translator_service();
  await test_missing_qid_is_a_bad_request();
  test_validation_tolerates_null_prototype_params();
  await test_body_less_requests_are_rejected_cleanly();
  await test_wrong_content_type_is_rejected_cleanly();
  await test_injected_query_params_survive_the_express_getter();
  await test_non_array_body_is_rejected_cleanly();
  console.log('END MODULE TEST express 5 request shapes');
}
