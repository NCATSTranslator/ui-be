import * as fs from 'node:fs'
import * as cmn from '../../common.mjs';
import { ARSClient } from '../../ARSClient.mjs';

const configRoot = '../../configurations';
const env = process.argv[2];
const filePath = `${configRoot}/frontend/${env}.json`;
const frontendConfig = await cmn.readJson(filePath);
const pks = frontendConfig.cached_queries.map(q => q.uuid);
const config = await cmn.readJson(`${configRoot}/${env}.json`)
const client = new ARSClient(`${config.ars_endpoint.protocol}://${config.ars_endpoint.host}`,
    '', '', config.ars_endpoint.retain_uri);

for (let pk of pks) {
  console.log(`Retaining ${pk}`);
  await client.retainQuery(pk);
  await new Promise(r => setTimeout(r, 5000));
}
