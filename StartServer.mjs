'use strict'

import { loadConfigFromFile }  from './config.mjs';
import { loadBiolink } from './biolink-model.mjs';
import { loadChebi } from './chebi.mjs';
import { TranslatorService } from './TranslatorService.mjs';
import { TranslatorServicexFEAdapter } from './TranslatorServicexFEAdapter.mjs';
import { ARSClient } from './ARSClient.mjs';
import { KGAnnotationClient } from './KGAnnotationClient.mjs';
import * as httpserver from './HTTPServer.mjs';

// Load the config asap as basically everything depends on it
const SERVER_CONFIG = await loadConfigFromFile(process.argv.length < 3 ? './configurations/mock.json' : './' + process.argv[2]);
await loadBiolink(SERVER_CONFIG.biolink.version,
                  SERVER_CONFIG.biolink.support_deprecated_predicates,
                  SERVER_CONFIG.biolink.infores_catalog,
                  SERVER_CONFIG.biolink.prefix_catalog);
await loadChebi();

if (process.argv.length > 3) {
  const secrets = await loadConfigFromFile(process.argv[3]);
  SERVER_CONFIG.secrets = secrets;
}

// Bootstrap the service -- this is a kludge. Services should offer factory or builder methods.
const queryClient = new ARSClient(
  `https://${SERVER_CONFIG.ars_endpoint.host}`,
  SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
const annotationClient = new KGAnnotationClient(
  `https://${SERVER_CONFIG.annotation_endpoint.host}`,
  SERVER_CONFIG.annotation_endpoint.pull_uri,
  SERVER_CONFIG.annotation_endpoint.timeout_ms);
const outputAdapter = new TranslatorServicexFEAdapter(annotationClient);
const service = new TranslatorService(queryClient, outputAdapter);

console.log("alles gut");
httpserver.startServer(SERVER_CONFIG, service);
