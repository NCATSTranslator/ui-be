'use strict';
import fs from 'fs';
import * as cmn from '../../lib/common.mjs';
import { load_trapi } from '../../lib/trapi.mjs';
import { ARSClient } from '../../lib/ARSClient.mjs';
import { TranslatorServicexFEAdapter } from '../../adapters/TranslatorServicexFEAdapter.mjs';
import { TranslatorService } from '../../services/TranslatorService.mjs';
import { query } from 'express';

function parseLine(line) {
    const fields = line.split('\t');
    if (fields.length < 6) {
        console.error(`Failed to parse line: ${line}`);
        return [];
    }

    const type = fields[0];
    const allowInbound = fields[3].toUpperCase() === 'TRUE';
    const allowOutbound = fields[4].toUpperCase() === 'TRUE';

    if (type === 'drug') {
        return [{
            name: fields[1],
            curie: fields[2],
            type: type,
            allow_inbound: allowInbound,
            allow_outbound: allowOutbound,
            direction: null
            //, uuid: 'PLACEHOLDER'
        }];
    } else {
        const directions = fields[5].split(',').map(d => d.trim());
        return directions.map(direction => ({
            name: fields[1],
            curie: fields[2],
            type: type,
            allow_inbound: allowInbound,
            allow_outbound: allowOutbound,
            direction: direction
            //, uuid: 'PLACEHOLDER'
        }));
    }
}

function initializeQueryList(filePath, startIndex) {

  const data = fs.readFileSync(filePath, 'utf-8');
  const lines = data.split('\n').filter(line => line.trim() !== '');


  const results = [];
  for (let i = startIndex; i < lines.length; i++) {
      results.push(...parseLine(lines[i]));
  }
  // at least one reason should exist to prerun the query
  return results.filter(e => e.allow_inbound || e.allow_outbound);
}

async function main() {
  const [,, headerOption, headerValue, envsOption, envsValue, inputFile, outputPath] = process.argv;

  // Check and process the 'envs' option
  if (envsOption !== '--envs' || !envsValue) {
      console.error('The --envs option must be specified with a comma-separated list of values.');
      return;
  }
  const envs = envsValue.split(',').map(e => e.trim()).filter(e => e !== '');

  if (headerOption !== '--skip-header') {
      console.error('Invalid header option provided.');
      return;
  }
  const validSkipValues = ['yes', 'true', 'no', 'false'];
  const normalizedValue = headerValue.toLowerCase();
  if (!validSkipValues.includes(normalizedValue)) {
      console.error(`Invalid value for --skip-header: ${headerValue}`);
      return;
  }
  let startIndex = 0;
  if (normalizedValue === 'yes' || normalizedValue === 'true') {
      startIndex = 1;
  }

  if (!fs.existsSync(inputFile)) {
    console.error(`File ${inputFile} does not exist.`);
    return;
  }

  let queryList = initializeQueryList(inputFile, startIndex);

  // Begin actual work
  load_trapi({
    query_subject_key: 'sn',
    query_object_key: 'on',
  });
  const timeBetweenQueries = 300000; // milliseconds -> 5 minutes
  const configRoot = '../../configurations';
  const preRunQueries = {};
  for (const env of envs) {
    const config = await cmn.readJson(`${configRoot}/${env}.json`);
    const client = new ARSClient(
      config.ars_endpoint.client_id,
      '',
      `${config.ars_endpoint.protocol}://${config.ars_endpoint.host}`,
      '',
      config.ars_endpoint.post_uri,
      '',
      '',
      '',
      config.use_ars_merging);
    const service = new TranslatorService(client);
    preRunQueries[env] = [];
    let qc = 0;
    for (const query of queryList) {
      try {
        const arsQuery = service.inputToQuery(query);
        const arsResp = await service.submitQuery(arsQuery);
        const qElem = {...query};
        qElem.uuid = arsResp.pk;
        qElem.id = query.curie;
        delete qElem.curie;

        preRunQueries[env].push(qElem);
        qc += 1;
        console.log(`[${env}] ${qc}/${queryList.length} queries submitted [${qElem.id} -- ${qElem.uuid}]`);
        await new Promise(r => setTimeout(r, timeBetweenQueries));
      } catch (err) {
        console.error(err);
      }
    }
  }

  for (const [env, queries] of Object.entries(preRunQueries)) {
    try {
      if (!cmn.isArrayEmpty(queries)) {
        fs.writeFileSync(`${outputPath}/${env}.json`, JSON.stringify(queries, null, 4));
      }
    } catch (err) {
      console.error(err);
      process.exit();
    }
  }
}

await main();
