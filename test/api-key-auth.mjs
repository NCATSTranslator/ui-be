export { test_api_key_auth }

import * as ast from 'node:assert';
import * as wutil from '../lib/webutils.mjs';
import { SessionController } from '../controllers/SessionController.mjs';
import { API_KEY_PREFIX } from '#model/ApiKey.mjs';

const KEY = `${API_KEY_PREFIX}${'a'.repeat(43)}`;

function make_req(headers = {}) {
  return { headersDistinct: headers };
}

function make_controller() {
  return new SessionController({}, {});
}

function test_request_to_header() {
  ast.strictEqual(wutil.request_to_header(make_req({ 'x-thing': ['one'] }), 'x-thing'), 'one',
    'a single header value is returned as a string');
  ast.strictEqual(wutil.request_to_header(make_req({ 'x-thing': ['one'] }), 'X-Thing'), 'one',
    'the header name is matched case-insensitively');
  ast.strictEqual(wutil.request_to_header(make_req({ 'x-thing': ['one', 'two'] }), 'x-thing'), null,
    'a repeated header is rejected');
  ast.strictEqual(wutil.request_to_header(make_req({ 'x-thing': [] }), 'x-thing'), null,
    'an empty header list is rejected');
  ast.strictEqual(wutil.request_to_header(make_req(), 'x-thing'), null,
    'a missing header is rejected');
}

function test_extract_bearer_api_key() {
  const controller = make_controller();
  const bearer = (value) => controller._extractBearerApiKey(make_req({ authorization: [value] }));

  ast.strictEqual(bearer(`Bearer ${KEY}`), KEY, 'a bearer key is extracted');
  ast.strictEqual(bearer(`bearer ${KEY}`), KEY, 'the scheme is matched case-insensitively');
  ast.strictEqual(bearer(`Bearer   ${KEY}`), KEY, 'runs of spaces between scheme and key are allowed');
  ast.strictEqual(bearer(` Bearer ${KEY} `), KEY, 'surrounding spaces are allowed');
  ast.strictEqual(bearer(`Bearer ${KEY} extra`), null, 'a trailing word is rejected');
  ast.strictEqual(bearer(`Basic ${KEY}`), null, 'a non-bearer scheme is rejected');
  ast.strictEqual(bearer(`Bearer not_a_key`), null, 'a token without the key prefix is rejected');
  ast.strictEqual(bearer('Bearer'), null, 'a scheme with no token is rejected');
  ast.strictEqual(bearer(''), null, 'an empty header is rejected');
  ast.strictEqual(controller._extractBearerApiKey(make_req()), null, 'a missing header is rejected');
  ast.strictEqual(controller._extractBearerApiKey(make_req({ authorization: [`Bearer ${KEY}`, `Bearer ${KEY}`] })),
    null, 'a repeated authorization header is rejected');
}

function test_extract_api_key_header() {
  const controller = make_controller();
  const header = (value) => controller._extractApiKeyHeader(make_req({ 'x-api-key': [value] }));

  ast.strictEqual(header(KEY), KEY, 'a prefixed key is extracted');
  ast.strictEqual(header('not_a_key'), null, 'a value without the key prefix is rejected');
  ast.strictEqual(header(''), null, 'an empty header is rejected');
  ast.strictEqual(controller._extractApiKeyHeader(make_req()), null, 'a missing header is rejected');
  ast.strictEqual(controller._extractApiKeyHeader(make_req({ 'x-api-key': [KEY, KEY] })), null,
    'a repeated header is rejected');
}

function test_extract_api_key_precedence() {
  const controller = make_controller();
  const other = `${API_KEY_PREFIX}${'b'.repeat(43)}`;
  ast.strictEqual(controller._extractApiKey(make_req({ authorization: [`Bearer ${KEY}`], 'x-api-key': [other] })),
    KEY, 'a bearer key wins over an x-api-key header');
  ast.strictEqual(controller._extractApiKey(make_req({ authorization: ['Basic abc'], 'x-api-key': [other] })),
    other, 'a non-key authorization header falls through to x-api-key');
  ast.strictEqual(controller._extractApiKey(make_req()), null, 'no credential yields null');
}

function make_res() {
  return {
    status_code: null,
    body: null,
    status(code) { this.status_code = code; return this; },
    send(body) { this.body = body; return this; }
  };
}

async function run_gate(gate, api_key_status) {
  const controller = new SessionController({}, { isApiKeyStatusValid: (s) => s === 'valid' });
  const req = { apiKeyData: { status: api_key_status, user: null, apiKey: null }, sessionData: null };
  const res = make_res();
  let next_called = false;
  await controller[gate](req, res, () => { next_called = true; });
  return { res, next_called };
}

async function test_gates_reject_invalid_keys() {
  for (const gate of ['authenticatePrivilegedRequest', 'authenticateUnprivilegedRequest']) {
    const valid = await run_gate(gate, 'valid');
    ast.strictEqual(valid.next_called, true, `${gate} passes a valid key through`);
    ast.strictEqual(valid.res.status_code, null, `${gate} sends nothing for a valid key`);

    const invalid = await run_gate(gate, 'revoked');
    ast.strictEqual(invalid.next_called, false, `${gate} does not pass an invalid key through`);
    ast.strictEqual(invalid.res.status_code, 401, `${gate} answers 401 for an invalid key`);
  }
}

async function test_api_key_auth() {
  console.log('START MODULE TEST API key header extraction');
  test_request_to_header();
  test_extract_bearer_api_key();
  test_extract_api_key_header();
  test_extract_api_key_precedence();
  await test_gates_reject_invalid_keys();
  console.log('END MODULE TEST API key header extraction');
}
