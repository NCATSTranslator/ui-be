'use strict'

import { loadConfigFromFile }  from './config.mjs';
import * as httpserver from './HTTPServer.mjs';

// Load the config asap as basically everything depends on it
const SERVER_CONFIG = await loadConfigFromFile(process.argv.length < 3 ? './configurations/mock.json' : './' + process.argv[2]);

var Service;
// Bootstrap the service -- this is a kludge. Services should offer factory or builder methods.
await (async function () {
    const ARSClientModule = await import('./ARSClient.mjs');
    const svcModule = await import(`./${SERVER_CONFIG.service.name}`);
    const client = new ARSClientModule.ARSClient(`https://${SERVER_CONFIG.ars_endpoint.host}`,
        SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
    Service = new svcModule.TranslatorService(client);
})();

console.log("alles gut");
httpserver.startServer(SERVER_CONFIG, Service);
