'use strict'

import { loadConfigFromFile }  from './config.mjs';
import { loadBiolink } from './biolink-model.mjs';
import { TranslatorService } from './TranslatorService.mjs';
import { TranslatorServicexFEAdapter } from './TranslatorServicexFEAdapter.mjs';
import { ARSClient } from './ARSClient.mjs';
import { MoleProClient } from './MoleProClient.mjs';
import * as httpserver from './HTTPServer.mjs';

// Load the config asap as basically everything depends on it
const SERVER_CONFIG = await loadConfigFromFile(process.argv.length < 3 ? './configurations/mock.json' : './' + process.argv[2]);
await loadBiolink(SERVER_CONFIG.biolink.version,
                  SERVER_CONFIG.biolink.support_deprecated_predicates,
                  SERVER_CONFIG.biolink.infores_catalog);

// Bootstrap the service -- this is a kludge. Services should offer factory or builder methods.
const queryClient = new ARSClient(
  `https://${SERVER_CONFIG.ars_endpoint.host}`,
  SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
const annotationClient = new MoleProClient(
  `https://${SERVER_CONFIG.molepro_endpoint.host}`,
  SERVER_CONFIG.molepro_endpoint.pull_uri);
const outputAdapter = new TranslatorServicexFEAdapter(annotationClient);
const service = new TranslatorService(queryClient, outputAdapter);

console.log("alles gut");
httpserver.startServer(SERVER_CONFIG, service);
