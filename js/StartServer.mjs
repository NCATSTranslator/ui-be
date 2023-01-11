'use strict'

import { SERVER_CONFIG } from './config.mjs';
import * as httpserver from './HTTPServer.mjs';

console.log(SERVER_CONFIG);

const svcModule = await import(`./${SERVER_CONFIG.service.name}`);
const svcAdapterModule = await import(`./${SERVER_CONFIG.service.adapter}`);
var svc;
// Bootstrap the service -- this is a kludge
await (async function () {
    let ARSClientModule = await import('./ARSClient.mjs');
    let client = new ARSClientModule.ARSClient(`https://${SERVER_CONFIG.ars_endpoint.host}`,
        SERVER_CONFIG.ars_endpoint.pull_uri, SERVER_CONFIG.ars_endpoint.post_uri);
    svc = new svcModule.TranslatorService(client);
})();

console.log("alles gut");
httpserver.serverStart(SERVER_CONFIG, svc);
//console.log(svc);
//console.log(svcAdapterModule);
