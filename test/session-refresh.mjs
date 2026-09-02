export { test_session_refresh }

import * as ast from 'node:assert';
import * as auth from '../services/AuthService.mjs';
import { SessionController } from '../controllers/SessionController.mjs';
import { Session } from '#model/Session.mjs';

const SESSION_PARAMS = {
  tokenTTLSec: 1800,
  sessionAbsoluteTTLSec: 15780000,
  sessionMaxIdleTimeSec: 15780000,
  loginRequestTTLSec: 3600
};

const COOKIE_CONFIG = {
  session_cookie: { name: 'sid', http_only: true, secure: true, same_site: 'lax' }
};

class FakeSessionStore {
  constructor(row) {
    this.row = row;
    this.committed_updates = 0;
  }

  async retrieveSessionByToken(token) {
    await yield_to_event_loop();
    return this.row.token === token ? new Session({...this.row}) : null;
  }

  async retrieveSessionById(id) {
    await yield_to_event_loop();
    return this.row.id === id ? new Session({...this.row}) : null;
  }

  async updateSession(session, expected_token = session.token) {
    await yield_to_event_loop();
    if (this.row.id !== session.id || this.row.token !== expected_token) {
      return null;
    }
    this.committed_updates++;
    this.row = {...session};
    return new Session({...this.row});
  }
}

async function yield_to_event_loop() {
  return new Promise((resolve) => setImmediate(resolve));
}

function make_session_row(token_age_sec) {
  const now = Date.now();
  return {
    id: 's1',
    token: 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa',
    time_token_created: new Date(now - token_age_sec * 1000),
    time_session_created: new Date(now - token_age_sec * 1000),
    time_session_updated: new Date(now),
    linked_from: null,
    force_kill: false,
    user_id: 'u1',
    data: null,
    auth_provider: 'una'
  };
}

function make_fixture(token_age_sec) {
  const store = new FakeSessionStore(make_session_row(token_age_sec));
  const user_store = { retrieveUserById: async () => ({ id: 'u1', deleted: false }) };
  const auth_service = new auth.AuthService(SESSION_PARAMS, store, user_store);
  return { store, auth_service, controller: new SessionController(COOKIE_CONFIG, auth_service) };
}

function make_res() {
  return {
    issued_cookies: [],
    cookie(name, value) { this.issued_cookies.push(value); }
  };
}

async function make_req(auth_service, token) {
  const req = { cookies: { sid: token } };
  req.sessionData = await auth_service.getSessionData(token);
  return req;
}

async function test_concurrent_refresh_converges_on_one_token() {
  const { store, auth_service, controller } = make_fixture(3600);
  const cookie_token = store.row.token;
  const reqs = await Promise.all(
    [1, 2, 3, 4, 5].map(() => make_req(auth_service, cookie_token)));

  for (const req of reqs) {
    ast.strictEqual(req.sessionData.status, auth.SESSION_TOKEN_EXPIRED,
      'every request should read the same expired token');
  }

  const results = await Promise.all(reqs.map(async (req) => {
    const res = make_res();
    const [success, errstr] = await controller._refreshSession(req, res, req.sessionData);
    ast.strictEqual(success, true, `concurrent refresh must not fail: ${errstr}`);
    ast.ok(req.sessionData.session, 'refreshed sessionData must carry a session row');
    ast.strictEqual(res.issued_cookies.length, 1, 'each refresh should issue one cookie');
    return res.issued_cookies[0];
  }));

  ast.strictEqual(new Set(results).size, 1,
    'concurrent refreshes must all issue the same token');
  ast.strictEqual(results[0], store.row.token,
    'the issued token must be the one actually stored');
  ast.strictEqual(store.committed_updates, 1,
    'only one concurrent writer should win the compare-and-set');
}

async function test_expired_token_is_rotated() {
  const { store, auth_service, controller } = make_fixture(3600);
  const original_token = store.row.token;
  const req = await make_req(auth_service, original_token);
  const res = make_res();

  const [success] = await controller._refreshSession(req, res, req.sessionData);
  ast.strictEqual(success, true, 'refreshing an expired token should succeed');
  ast.strictEqual(res.issued_cookies.length, 1, 'a rotated token must be sent to the client');
  ast.notStrictEqual(res.issued_cookies[0], original_token, 'the token should have rotated');
  ast.strictEqual(res.issued_cookies[0], store.row.token, 'the cookie must match the stored token');
}

async function test_valid_session_does_not_reissue_cookie() {
  const { auth_service, controller } = make_fixture(0);
  const req = await make_req(auth_service, 'aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa');
  ast.strictEqual(req.sessionData.status, auth.SESSION_VALID,
    'a fresh token should read as valid');
  const res = make_res();

  const [success] = await controller._refreshSession(req, res, req.sessionData);
  ast.strictEqual(success, true, 'touching a valid session should succeed');
  ast.strictEqual(res.issued_cookies.length, 0,
    'a valid session must not rotate the cookie');
}

async function test_vanished_session_fails_cleanly() {
  const { store, auth_service, controller } = make_fixture(3600);
  const req = await make_req(auth_service, store.row.token);
  store.updateSession = async () => null;
  store.retrieveSessionById = async () => null;

  const [success, errstr, errcode] = await controller._refreshSession(req, make_res(), req.sessionData);
  ast.strictEqual(success, false, 'a vanished session row must report failure');
  ast.strictEqual(errcode, 500, 'a vanished session row must map to a 500');
  ast.ok(errstr, 'a failed refresh must carry an error message');
}

async function test_force_killed_session_is_unauthorized() {
  const { store, auth_service, controller } = make_fixture(3600);
  const req = await make_req(auth_service, store.row.token);
  const original_update = store.updateSession.bind(store);
  store.updateSession = async (session, expected_token) => {
    const updated = await original_update(session, expected_token);
    store.row.force_kill = true;
    return updated;
  };

  const [success, , errcode] = await controller._refreshSession(req, make_res(), req.sessionData);
  ast.strictEqual(success, false, 'a session killed mid-refresh must report failure');
  ast.strictEqual(errcode, 401, 'a session killed mid-refresh must map to a 401');
  ast.ok(req.sessionData.session, 'the caller still needs the refreshed session data');
}

async function test_session_refresh() {
  console.log('START MODULE TEST controllers/SessionController.mjs');
  await test_concurrent_refresh_converges_on_one_token();
  await test_expired_token_is_rotated();
  await test_valid_session_does_not_reissue_cookie();
  await test_vanished_session_fails_cleanly();
  await test_force_killed_session_is_unauthorized();
  console.log('END MODULE TEST controllers/SessionController.mjs');
}
