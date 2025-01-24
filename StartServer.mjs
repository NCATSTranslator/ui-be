'use strict'
import { logger } from './lib/logger.mjs';

import { bootstrapConfig } from './lib/config.mjs';

import { loadBiolink } from './lib/biolink-model.mjs';
import { loadChebi } from './lib/chebi.mjs';
import { loadTrapi } from './lib/trapi.mjs';
import { TranslatorService } from './services/TranslatorService.mjs';
import { ARSClient } from './lib/ARSClient.mjs';
import * as httpserver from './HTTPServer.mjs';
import { AuthService } from './services/AuthService.mjs';
import { UserService } from './services/UserService.mjs';
import { QueryService } from './services/QueryService.mjs';
import { ARSCallbackxQueryServiceAdapter } from './adapters/ARSCallbackxQueryServiceAdapter.mjs';

import { SessionStorePostgres } from './stores/SessionStorePostgres.mjs';
import { UserStorePostgres } from './stores/UserStorePostgres.mjs';
import { pg } from './lib/postgres_preamble.mjs';
import { UserPreferenceStorePostgres } from './stores/UserPreferenceStorePostgres.mjs';
import { UserSavedDataStorePostgres } from './stores/UserSavedDataStorePostgres.mjs';
import { UserWorkspaceStorePostgres } from './stores/UserWorkspaceStorePostgres.mjs';
import { QueryStorePostgres } from './stores/QueryStorePostgres.mjs';


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

await loadBiolink(SERVER_CONFIG.biolink);
await loadTrapi(SERVER_CONFIG.trapi);
await loadChebi();

// Bootstrap the translator service.
// All these bootstraps feel kludgy.
const TRANSLATOR_SERVICE = (function (config) {
  const queryClient = new ARSClient(
    config.ars_endpoint.client_id,
    config.secrets.hmac.key,
    `${config.ars_endpoint.protocol}://${config.ars_endpoint.host}`,
    config.ars_endpoint.pull_uri,
    config.ars_endpoint.post_uri,
    config.ars_endpoint.retain_uri,
    config.ars_endpoint.subscribe_uri,
    config.ars_endpoint.unsubscribe_uri,
    config.ars_endpoint.use_ars_merging);
  return new TranslatorService(queryClient);
})(SERVER_CONFIG);

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
    sessionMaxIdleTimeSec: config.sessions.session_max_idle_time_sec
  },
  new SessionStorePostgres(dbPool),
  new UserStorePostgres(dbPool));
})(SERVER_CONFIG);

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
    new UserWorkspaceStorePostgres(dbPool)
  );
})(SERVER_CONFIG);

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

httpserver.startServer(SERVER_CONFIG, {
  translatorService: TRANSLATOR_SERVICE,
  authService: AUTH_SERVICE,
  userService: USER_SERVICE,
  queryService: QUERY_SERVICE
});
