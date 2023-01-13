'use strict'

import { SERVER_CONFIG } from './config.mjs';
import * as httpserver from './HTTPServer.mjs';

var svc;
// Bootstrap the service -- this is a kludge. Services should offer factory or builder methods.
await (async function () {
    const ARSClientModule = await import('./ARSClient.mjs');
    const svcModule = await import(`./${SERVER_CONFIG.service.name}`);
    const client = new ARSClientModule.ARSClient(`https://${SERVER_CONFIG.ars_endpoint.host}`,
        SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
    svc = new svcModule.TranslatorService(client);
})();

console.log("alles gut");
httpserver.startServer(SERVER_CONFIG, svc);
