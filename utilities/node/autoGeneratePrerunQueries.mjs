import * as fs from 'fs';
import * as cmn from '../../common.mjs';
import { ARSClient } from '../../ARSClient.mjs';
import { TranslatorServicexFEAdapter } from '../../TranslatorServicexFEAdapter.mjs';
import { TranslatorService } from '../../TranslatorService.mjs';

const configRoot = '../../configurations';
const environments = ['dev', 'ci', 'test', 'production'];
const arsWaitTime = 60; // seconds
const outputAdapter = new TranslatorServicexFEAdapter(null);
const queriesPath = process.argv[2];
const outputPath = process.argv[3];
const queries = await cmn.readJson(queriesPath);

const preRunQueries = {};
for (const env of environments) {
  const config = await cmn.readJson(`${configRoot}/${env}.json`);
  const client = new ARSClient(`https://${config.ars_endpoint.host}`,
                               '',
                               config.ars_endpoint.post_uri);
  const service = new TranslatorService(client, outputAdapter);
  preRunQueries[env] = [];
  let qc = 0;
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
        qc += 1;
        console.log(`[${env}] ${qc}/${queries.length} queries submitted`);
        console.log(`Sleeping for ${arsWaitTime} seconds...`)
        await new Promise(r => setTimeout(r, arsWaitTime * 1000));
      } catch (err) {
        preRunQueries[env] = [];
        console.error(err);
        break;
      }
    }
  }

  const envPath = `${outputPath}/${env}.json`;
  console.log(`[${env}] writing to ${envPath}`);
  for (const queries of Object.values(preRunQueries[env])) {
    try {
      fs.writeFileSync(envPath, JSON.stringify(queries));
    } catch (err) {
      console.error(err);
      process.exit();
    }
  }
}