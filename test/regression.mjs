'use strict'
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import * as tsmy from './lib/summarization.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import { TranslatorServicexFEAdapter } from '../adapters/TranslatorServicexFEAdapter.mjs';
import { loadTrapi } from '../lib/trapi.mjs';

// We have to do this because the 'before' hook does not seem to work
async function loadConfig() {
  const config = await cfg.bootstrapConfig('test/data/regression/config.json');
  await loadBiolink(config.biolink);
  await loadChebi();
  loadTrapi(config.trapi);
}

async function regressionTest(testFile) {
  await loadConfig();
  const input = await cmn.readJson(`test/data/regression/in/${testFile}`);
  const expected = await cmn.readJson(`test/data/regression/out/${testFile}`);
  const maxHops = 3;
  const translatorAdapter = new TranslatorServicexFEAdapter();
  const actual = await translatorAdapter.queryResultsToFE(input, maxHops);
  tsmy.testSummary(actual.data, expected);
}

await regressionTest('00881bc8-5bcd-472b-aafa-dbc4e8992dcd.json');
await regressionTest('020d41bd-1709-416f-befc-392b7ca56e2a.json');
await regressionTest('050daf46-2233-4603-bec5-e71812290494.json');
await regressionTest('055991c4-37c7-4c2d-b048-82041f7a2137.json');
await regressionTest('0dea8612-2e19-423e-85ae-c021b5d132ac.json');
await regressionTest('24798984-a55f-460e-b286-531f2eeb541d.json');
await regressionTest('285a6a67-df50-414c-8b86-19e1a2ff1412.json');
await regressionTest('28a4edfd-03d6-4b30-a899-fe37fbd85265.json');
await regressionTest('2ad7c20f-c252-4c15-bdf2-f4e4b5e7b50c.json');
await regressionTest('2d4b2a6a-887c-4e73-90e5-e62119071396.json');
await regressionTest('35462d4e-4d67-439b-97e2-3c6d7c5920bf.json');
await regressionTest('497b7e08-a541-492a-9930-417fac4dfaea.json');
await regressionTest('4e1346f1-d3f2-462f-954f-546084b8dd37.json');
await regressionTest('52000f6d-a3ea-4dc1-ae44-636fa5b6e447.json');
await regressionTest('577a5ce3-8f31-4a6b-bc7a-372977265802.json');
await regressionTest('58b410ec-5a46-4d53-9709-781e20a2c9d0.json');
await regressionTest('5fab4b98-7825-4b1e-ba79-8638687ed6de.json');
await regressionTest('6990d9c1-0a5d-46dc-83d0-c608178a3fb4.json');
await regressionTest('898407e1-e07d-4f33-bdbf-e8c4eacd24f6.json');
await regressionTest('89fcc19e-6c86-47f5-84a2-4053be34a49b.json');
await regressionTest('9b461266-37be-4d35-b665-23f67eacdae8.json');
await regressionTest('ae3db825-c2e8-40dc-acbc-831d4b8325a8.json');
await regressionTest('b1276f27-97aa-4e7b-8559-069bd6fb8f67.json');
await regressionTest('c4b08dec-2c29-40c2-9e0a-4ffdf5ffe0cc.json');
await regressionTest('ce985250-4701-4c66-941f-97242c764ab0.json');
await regressionTest('d88b2261-00ce-4a30-a684-75ac1a9d2a70.json');
await regressionTest('ddb0c5e8-35fd-4ef9-8d97-11698d87addf.json');
await regressionTest('de949c79-7364-473c-a57b-f9a3d7922e0a.json');
await regressionTest('dee141c2-2566-4983-91b0-b7e0166bcd82.json');
await regressionTest('dfb87e9f-96a4-44ef-a015-7704392ac62a.json');
await regressionTest('e07b41c5-1a2f-47ca-9511-d0d7de70ca9f.json');
await regressionTest('eafb6dcf-ef65-46b6-bd5b-d237e23907da.json');
await regressionTest('f18a3b53-b309-4978-93f2-0b38d8f3c701.json');
await regressionTest('f6b094da-ad8a-40a5-839f-d2a3c26ae99c.json');
await regressionTest('fa9c31cf-6d13-4284-b657-96acee6c387d.json');
await regressionTest('fbdf1b14-179b-44c0-a41f-ee2bc84047a6.json');
await regressionTest('fe681a8f-b240-4d07-a2dd-67e789907778.json');
console.log('Regression tests passed');
