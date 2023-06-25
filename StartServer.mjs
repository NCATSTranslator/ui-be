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
import { SessionStorePostgres } from './auth/SessionStorePostgres.mjs';

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

// Bootstrap the service -- this is a kludge. Services should offer factory or builder methods.
const queryClient = new ARSClient(
  `https://${SERVER_CONFIG.ars_endpoint.host}`,
  SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
const annotationClient = new KGAnnotationClient(
  `https://${SERVER_CONFIG.annotation_endpoint.host}`,
  SERVER_CONFIG.annotation_endpoint.pull_uri,
  SERVER_CONFIG.annotation_endpoint.timeout_ms);
const outputAdapter = new TranslatorServicexFEAdapter(annotationClient);
const TRANSLATOR_SERVICE = new TranslatorService(queryClient, outputAdapter);

const AUTH_SERVICE = new AuthService({
    tokenTTLSec: SERVER_CONFIG.sessions.token_ttl_sec,
    sessionAbsoluteTTLSec: SERVER_CONFIG.sessions.session_absolute_ttl_sec,
    sessionMaxIdleTimeSec: SERVER_CONFIG.sessions.session_max_idle_time_sec
  }, new SessionStorePostgres({
    ...SERVER_CONFIG.storage.sessions_pg,
    password: SERVER_CONFIG.secrets.storage.sessions_pg.password
  })
);

console.log("alles gut");
httpserver.startServer(SERVER_CONFIG, TRANSLATOR_SERVICE, AUTH_SERVICE);
