'use strict'

import { loadConfigFromFile }  from './config.mjs';
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

// Load the config asap as basically everything depends on it
const SERVER_CONFIG = await loadConfigFromFile(process.argv.length < 3 ? './configurations/mock.json' : './' + process.argv[2]);
await loadBiolink(SERVER_CONFIG.biolink.version,
                  SERVER_CONFIG.biolink.support_deprecated_predicates,
                  SERVER_CONFIG.biolink.infores_catalog,
                  SERVER_CONFIG.biolink.prefix_catalog);
await loadChebi();

/*
if (process.argv.length > 3) {
  const secrets = await loadConfigFromFile(process.argv[3]);
  SERVER_CONFIG.secrets = secrets;
}*/

// Bootstrap the translator service.
// All these bootstraps feel kludgy.
const TRANSLATOR_SERVICE = (function (config) {
  const queryClient = new ARSClient(
    `https://${config.ars_endpoint.host}`,
    config.ars_endpoint.pull_uri, config.ars_endpoint.post_uri);
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
    password: config.secrets.pg.password
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
    password: config.secrets.pg.password
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
