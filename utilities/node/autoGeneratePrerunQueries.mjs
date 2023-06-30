import * as fs from 'fs';
import * as cmn from '../../common.mjs';
import { ARSClient } from '../../ARSClient.mjs';
import { TranslatorServicexFEAdapter } from '../../TranslatorServicexFEAdapter.mjs';
import { TranslatorService } from '../../TranslatorService.mjs';

const configRoot = '../../configurations';
const environments = ['ci', 'test', 'production'];
const outputAdapter = new TranslatorServicexFEAdapter(null);
const queries = await cmn.readJson(`${configRoot}/frontend/prerun-queries.json`);

const preRunQueries = {};
for (const env of environments) {
  const config = await cmn.readJson(`${configRoot}/${env}.json`);
  const client = new ARSClient(`https://${config.ars_endpoint.host}`,
                               '',
                               config.ars_endpoint.post_uri);
  const service = new TranslatorService(client, outputAdapter);
  preRunQueries[env] = [];
  for (const query of queries) {
    if (query.env.includes(env)) {
      try {
        const arsQuery = service.inputToQuery(query);
        const arsResp = await service.submitQuery(arsQuery);
        preRunQueries[env].push({
          "name": query.name,
          "id": query.curie,
          "type": query.type,
          "direction": query.direction,
          "uuid": arsResp.pk
        });
      } catch (err) {
        console.error(err);
        process.exit();
      }
    }
  }
}

for (const [env, queries] of Object.entries(preRunQueries)) {
  try {
    fs.writeFileSync(`${configRoot}/frontend/${env}.json`, JSON.stringify(queries));
  } catch (err) {
    console.error(err);
    process.exit();
  }
}
