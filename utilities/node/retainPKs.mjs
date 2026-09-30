import path from 'path';
import * as cmn from '../../lib/common.mjs';
import { ARSClient } from '../../lib/ARSClient.mjs';

const configRoot = path.join(import.meta.dirname, '../../configurations');
const env = process.argv[2];
const filePath = `${configRoot}/frontend/${env}.json`;
const frontendConfig = await cmn.read_json(filePath);
const pks = frontendConfig.cached_queries.map(q => q.uuid);
const config = await cmn.read_json(`${configRoot}/${env}.json`)
const client = new ARSClient(config.ars_endpoint, '');

for (let pk of pks) {
  console.log(`Retaining ${pk}`);
  await client.retainQuery(pk);
  await new Promise(r => setTimeout(r, 5000));
}
