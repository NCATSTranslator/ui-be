'use strict'

import { loadConfigFromFile, postProcessConfig }  from './config.mjs';
import { loadBiolink } from './biolink-model.mjs';
import { loadChebi } from './chebi.mjs';
import { TranslatorService } from './TranslatorService.mjs';
import { TranslatorServicexFEAdapter } from './TranslatorServicexFEAdapter.mjs';
import { ARSClient } from './ARSClient.mjs';
import { KGAnnotationClient } from './KGAnnotationClient.mjs';
import * as httpserver from './HTTPServer.mjs';
import { AuthService } from './auth/AuthService.mjs';
import { UserService } from './UserService.mjs';

import { SessionStorePostgres } from './auth/SessionStorePostgres.mjs';
import { UserStorePostgres } from './users/UserStorePostgres.mjs';
import { pg } from './postgres_preamble.mjs';
import { UserPreferenceStorePostgres } from './users/UserPreferenceStorePostgres.mjs';
import { UserSavedDataStorePostgres } from './users/UserSavedDataStorePostgres.mjs';
import { overwriteObj, readJson } from './common.mjs';

// Load the config asap as basically everything depends on it
//let SERVER_CONFIG = await loadConfigFromFile(process.argv.length < 3 ? './configurations/mock.json' : './' + process.argv[2]);
let SERVER_CONFIG;
if (process.argv.length === 3) {
  SERVER_CONFIG = await loadConfigFromFile(process.argv[2]);
} else if (process.argv.length ===  4) {
  SERVER_CONFIG = await loadConfigFromFile(process.argv[2]);
  let overrides = await loadConfigFromFile(process.argv[3]);
  SERVER_CONFIG = overwriteObj(SERVER_CONFIG, overrides);
} else {
  throw new Error(`Unsupported number of args (${process.argv.length}) at startup. Exiting.`);
}
postProcessConfig(SERVER_CONFIG);

await loadBiolink(SERVER_CONFIG.biolink.version,
                  SERVER_CONFIG.biolink.support_deprecated_predicates,
                  SERVER_CONFIG.biolink.infores_catalog,
                  SERVER_CONFIG.biolink.prefix_catalog);
await loadChebi();

// Bootstrap the translator service.
// All these bootstraps feel kludgy.
const TRANSLATOR_SERVICE = (function (config) {
  const queryClient = new ARSClient(
    `${config.ars_endpoint.protocol}://${config.ars_endpoint.host}`,
    config.ars_endpoint.pull_uri,
    config.ars_endpoint.post_uri,
    config.ars_endpoint.retain_uri,
    config.ars_endpoint.use_ars_merging);
  const annotationClient = new KGAnnotationClient(
    `https://${config.annotation_endpoint.host}`,
    config.annotation_endpoint.pull_uri,
    config.annotation_endpoint.timeout_ms);
  const outputAdapter = new TranslatorServicexFEAdapter(annotationClient);
  return new TranslatorService(queryClient, outputAdapter);
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
    new UserSavedDataStorePostgres(dbPool)
  );
})(SERVER_CONFIG);

let log_config = { ...SERVER_CONFIG};
log_config.secrets = '[REDACTED]'
console.log(log_config);

httpserver.startServer(SERVER_CONFIG, {
  translatorService: TRANSLATOR_SERVICE,
  authService: AUTH_SERVICE,
  userService: USER_SERVICE
});
