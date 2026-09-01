'use strict'
import { logger } from './lib/logger.mjs';

import { bootstrapConfig } from './lib/config.mjs';

import { load_biolink } from './lib/biolink-model.mjs';
import { loadChebi } from './lib/chebi.mjs';
import { load_trapi } from './lib/trapi/core.mjs';
import { TranslatorService } from './services/TranslatorService.mjs';
import { ARSClient } from './lib/ARSClient.mjs';
import * as httpserver from './http_server.mjs';
import { AuthService } from './services/AuthService.mjs';
import { UserService } from './services/UserService.mjs';
import { QueryService } from './services/QueryService.mjs';
import { ARSCallbackxQueryServiceAdapter } from './adapters/ARSCallbackxQueryServiceAdapter.mjs';

import { SessionStorePostgres } from './stores/SessionStorePostgres.mjs';
import { UserStorePostgres } from './stores/UserStorePostgres.mjs';
import { pg } from './lib/postgres_preamble.mjs';
import { UserPreferenceStorePostgres } from './stores/UserPreferenceStorePostgres.mjs';
import { UserSavedDataStorePostgres } from './stores/UserSavedDataStorePostgres.mjs';
import { CanvasStorePostgres } from './stores/CanvasStorePostgres.mjs';
import { QueryStorePostgres } from './stores/QueryStorePostgres.mjs';
import { ApiKeyStoreMemory } from './stores/ApiKeyStoreMemory.mjs';


// Load the config asap as basically everything depends on it
const SERVER_CONFIG = await (async function() {
  let basefile, overrides = null;
  if (process.argv.length === 3) {
    basefile = process.argv[2];
  } else if (process.argv.length === 4) {
    basefile = process.argv[2];
    overrides = process.argv[3]
  } else {
    throw new Error(`Unsupported number of args (${process.argv.length}) at startup. Exiting.`);
  }
  return bootstrapConfig(basefile, overrides);
})();

await load_biolink(SERVER_CONFIG.biolink);
await load_trapi(SERVER_CONFIG.trapi);
await loadChebi();

// Bootstrap the translator service.
// All these bootstraps feel kludgy.

const TRANSLATOR_SERVICE = await (async function (config) {
  let query_client = null;
  if (config.ars_endpoint.host === "mock") {
    const client_module = await import("./mock/client.mjs");
    query_client = new client_module.ARSClient(config.ars_endpoint.data_path);
  } else {
    query_client = new ARSClient(config.ars_endpoint, config.secrets.hmac.key);
  }
  return new TranslatorService(query_client);
})(SERVER_CONFIG);

/* API keys are demo-only for now and live in memory rather than in a table, so there is no
 * api_keys migration to run. Unlike the Postgres stores below -- which are constructed
 * per-service but all reach the same database -- this store IS the table, so the auth service
 * and the user service must share this one instance or keys minted through one would be
 * invisible to the other. Keys do not survive a restart. */
const API_KEY_STORE = new ApiKeyStoreMemory();

// Bootstrap the auth service
const AUTH_SERVICE = (function (config) {
  const dbPool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return new AuthService({
    tokenTTLSec: config.sessions.token_ttl_sec,
    sessionAbsoluteTTLSec: config.sessions.session_absolute_ttl_sec,
    sessionMaxIdleTimeSec: config.sessions.session_max_idle_time_sec,
    loginRequestTTLSec: config.sessions.login_request_ttl_sec
  },
  new SessionStorePostgres(dbPool),
  new UserStorePostgres(dbPool),
  API_KEY_STORE);
})(SERVER_CONFIG);

/* Session auth checking is controlled from here so the HTTP layer and SessionController stay
 * untouched. Setting "auth_check": false enables a bypass that ensures a fixed test user exists in
 * the DB and makes every request resolve to it; any other value (or its absence) leaves normal auth
 * in place. As a safety rail, disabling auth is only permitted under the mock configuration
 * (ars_endpoint.host === "mock"): anywhere else we refuse to start so a real deployment can never
 * accidentally run with session auth turned off. */
const IS_MOCK_CONFIG = SERVER_CONFIG.ars_endpoint.host === 'mock';
if (SERVER_CONFIG.auth_check === false) {
  if (!IS_MOCK_CONFIG) {
    throw new Error('Refusing to start: "auth_check" is false but this is not the mock '
      + 'configuration (ars_endpoint.host !== "mock"). Session auth may only be disabled under mock.');
  }
  const { bypassSessionAuth } = await import('./mock/auth.mjs');
  await bypassSessionAuth(AUTH_SERVICE);
}

/* Seed the fixed demo API key. It rides the same rail as the auth bypass above: the key is
 * hardcoded and therefore public, so it is only ever registered under the mock configuration
 * and can never grant access in a real deployment. */
if (IS_MOCK_CONFIG) {
  const { seedDemoApiKey } = await import('./mock/api-key.mjs');
  await seedDemoApiKey(API_KEY_STORE, AUTH_SERVICE.userStore);
}

// Bootstrap the user service
const USER_SERVICE = (function (config) {
  const dbPool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return new UserService(
    new UserStorePostgres(dbPool),
    new UserPreferenceStorePostgres(dbPool),
    new UserSavedDataStorePostgres(dbPool),
    new CanvasStorePostgres(dbPool),
    config.secrets.hmac.key,
    API_KEY_STORE
  );
})(SERVER_CONFIG);

/* Seed the demo user's query list. Same mock-only rail as the demo API key above. Unlike the
 * key, this writes to Postgres and persists, so the seeder is idempotent. */
if (IS_MOCK_CONFIG) {
  const { seedDemoUserQuery } = await import('./mock/query.mjs');
  await seedDemoUserQuery(USER_SERVICE);
}

const QUERY_SERVICE = (function (config) {
  const dbPool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return new QueryService(new QueryStorePostgres(dbPool),
                          new ARSCallbackxQueryServiceAdapter());
})(SERVER_CONFIG);
logger.info(SERVER_CONFIG, "Server configuration");

httpserver.start_server(SERVER_CONFIG, {
  translatorService: TRANSLATOR_SERVICE,
  authService: AUTH_SERVICE,
  userService: USER_SERVICE,
  queryService: QUERY_SERVICE
});
