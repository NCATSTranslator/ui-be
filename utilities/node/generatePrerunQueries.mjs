'use strict';
import fs from 'fs';
import path from 'path';
import * as cmn from '../../lib/common.mjs';
import { load_trapi } from '../../lib/trapi/core.mjs';
import { ARSClient } from '../../lib/ARSClient.mjs';
import { TranslatorService } from '../../services/TranslatorService.mjs';

const configRoot = path.join(import.meta.dirname, '../../configurations');
const timeBetweenQueries = 300000; // milliseconds -> 5 minutes

function writeFrontendConfig(filePath, frontendConfig) {
  fs.writeFileSync(filePath, JSON.stringify(frontendConfig, null, 2) + '\n');
}

async function regenerateEnv(env) {
  const config = await cmn.read_json(`${configRoot}/${env}.json`);
  const frontendPath = `${configRoot}/frontend/${env}.json`;
  const frontendConfig = await cmn.read_json(frontendPath);
  const queries = frontendConfig.cached_queries;
  load_trapi(config.trapi);
  const service = new TranslatorService(new ARSClient(config.ars_endpoint, ''));
  let qc = 0;
  for (const query of queries) {
    qc += 1;
    try {
      const arsQuery = service.inputToQuery({
        type: query.type,
        curie: query.id,
        direction: query.direction
      });
      const arsResp = await service.submitQuery(arsQuery);
      const oldUuid = query.uuid;
      query.uuid = arsResp.pk;
      writeFrontendConfig(frontendPath, frontendConfig);
      console.log(`[${env}] ${qc}/${queries.length} submitted [${query.id} ${query.direction ?? ''}] ${oldUuid} -> ${query.uuid}`);
    } catch (err) {
      console.error(`[${env}] ${qc}/${queries.length} FAILED [${query.id} ${query.direction ?? ''}], keeping ${query.uuid}`);
      console.error(err);
    }
    if (qc < queries.length) {
      await new Promise(r => setTimeout(r, timeBetweenQueries));
    }
  }
}

async function main() {
  const envs = process.argv.slice(2);
  if (cmn.is_array_empty(envs)) {
    console.error('Usage: node generatePrerunQueries.mjs <env> [<env> ...]');
    process.exit(1);
  }
  for (const env of envs) {
    await regenerateEnv(env);
  }
}

await main();
