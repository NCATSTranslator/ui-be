'use strict'
import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
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

function reduceSummaryNoise(summary) {
  summary.meta = null;
  summary.errors = null;
  return JSON.stringify(summary);
}

async function regressionTest(testFile) {
  await loadConfig();
  const input = await cmn.readJson(`test/data/regression/in/${testFile}`);
  const expected = await cmn.readJson(`test/data/regression/out/${testFile}`);
  const maxHops = 3;
  const translatorAdapter = new TranslatorServicexFEAdapter();
  const actual = await translatorAdapter.queryResultsToFE(input, maxHops);
  assert.strictEqual(reduceSummaryNoise(actual.data), reduceSummaryNoise(expected));
}

describe('Regression Tests', async () => {

  it('Regression test for 00881bc8-5bcd-472b-aafa-dbc4e8992dcd.json', async () => {
    await regressionTest('00881bc8-5bcd-472b-aafa-dbc4e8992dcd.json');
  });

//  it('Regression test for 020d41bd-1709-416f-befc-392b7ca56e2a.json', async () => {
//    await regressionTest('020d41bd-1709-416f-befc-392b7ca56e2a.json');
//  });
//
//  it('Regression test for 050daf46-2233-4603-bec5-e71812290494.json', async () => {
//    await regressionTest('050daf46-2233-4603-bec5-e71812290494.json');
//  });
//
//  it('Regression test for 055991c4-37c7-4c2d-b048-82041f7a2137.json', async () => {
//    await regressionTest('055991c4-37c7-4c2d-b048-82041f7a2137.json');
//  });
//
//  it('Regression test for 0dea8612-2e19-423e-85ae-c021b5d132ac.json', async () => {
//    await regressionTest('0dea8612-2e19-423e-85ae-c021b5d132ac.json');
//  });
//
//  it('Regression test for 24798984-a55f-460e-b286-531f2eeb541d.json', async () => {
//    await regressionTest('24798984-a55f-460e-b286-531f2eeb541d.json');
//  });
//
//  it('Regression test for 285a6a67-df50-414c-8b86-19e1a2ff1412.json', async () => {
//    await regressionTest('285a6a67-df50-414c-8b86-19e1a2ff1412.json');
//  });
//
//  it('Regression test for 28a4edfd-03d6-4b30-a899-fe37fbd85265.json', async () => {
//    await regressionTest('28a4edfd-03d6-4b30-a899-fe37fbd85265.json');
//  });
//
//  it('Regression test for 2ad7c20f-c252-4c15-bdf2-f4e4b5e7b50c.json', async () => {
//    await regressionTest('2ad7c20f-c252-4c15-bdf2-f4e4b5e7b50c.json');
//  });
//
//  it('Regression test for 2d4b2a6a-887c-4e73-90e5-e62119071396.json', async () => {
//    await regressionTest('2d4b2a6a-887c-4e73-90e5-e62119071396.json');
//  });
//
//  it('Regression test for 35462d4e-4d67-439b-97e2-3c6d7c5920bf.json', async () => {
//    await regressionTest('35462d4e-4d67-439b-97e2-3c6d7c5920bf.json');
//  });
//
//  it('Regression test for 497b7e08-a541-492a-9930-417fac4dfaea.json', async () => {
//    await regressionTest('497b7e08-a541-492a-9930-417fac4dfaea.json');
//  });
//
//  it('Regression test for 4e1346f1-d3f2-462f-954f-546084b8dd37.json', async () => {
//    await regressionTest('4e1346f1-d3f2-462f-954f-546084b8dd37.json');
//  });
//
//  it('Regression test for 52000f6d-a3ea-4dc1-ae44-636fa5b6e447.json', async () => {
//    await regressionTest('52000f6d-a3ea-4dc1-ae44-636fa5b6e447.json');
//  });
//
//  it('Regression test for 577a5ce3-8f31-4a6b-bc7a-372977265802.json', async () => {
//    await regressionTest('577a5ce3-8f31-4a6b-bc7a-372977265802.json');
//  });
//
//  it('Regression test for 58b410ec-5a46-4d53-9709-781e20a2c9d0.json', async () => {
//    await regressionTest('58b410ec-5a46-4d53-9709-781e20a2c9d0.json');
//  });
//
//  it('Regression test for 5fab4b98-7825-4b1e-ba79-8638687ed6de.json', async () => {
//    await regressionTest('5fab4b98-7825-4b1e-ba79-8638687ed6de.json');
//  });
//
//  it('Regression test for 6990d9c1-0a5d-46dc-83d0-c608178a3fb4.json', async () => {
//    await regressionTest('6990d9c1-0a5d-46dc-83d0-c608178a3fb4.json');
//  });
//
//  it('Regression test for 898407e1-e07d-4f33-bdbf-e8c4eacd24f6.json', async () => {
//    await regressionTest('898407e1-e07d-4f33-bdbf-e8c4eacd24f6.json');
//  });
//
//  it('Regression test for 89fcc19e-6c86-47f5-84a2-4053be34a49b.json', async () => {
//    await regressionTest('89fcc19e-6c86-47f5-84a2-4053be34a49b.json');
//  });
//
//  it('Regression test for 9b461266-37be-4d35-b665-23f67eacdae8.json', async () => {
//    await regressionTest('9b461266-37be-4d35-b665-23f67eacdae8.json');
//  });
//
//  it('Regression test for ae3db825-c2e8-40dc-acbc-831d4b8325a8.json', async () => {
//    await regressionTest('ae3db825-c2e8-40dc-acbc-831d4b8325a8.json');
//  });
//
//  it('Regression test for b1276f27-97aa-4e7b-8559-069bd6fb8f67.json', async () => {
//    await regressionTest('b1276f27-97aa-4e7b-8559-069bd6fb8f67.json');
//  });
//
//  it('Regression test for c4b08dec-2c29-40c2-9e0a-4ffdf5ffe0cc.json', async () => {
//    await regressionTest('c4b08dec-2c29-40c2-9e0a-4ffdf5ffe0cc.json');
//  });
//
//  it('Regression test for ce985250-4701-4c66-941f-97242c764ab0.json', async () => {
//    await regressionTest('ce985250-4701-4c66-941f-97242c764ab0.json');
//  });
//
//  it('Regression test for d88b2261-00ce-4a30-a684-75ac1a9d2a70.json', async () => {
//    await regressionTest('d88b2261-00ce-4a30-a684-75ac1a9d2a70.json');
//  });
//
//  it('Regression test for ddb0c5e8-35fd-4ef9-8d97-11698d87addf.json', async () => {
//    await regressionTest('ddb0c5e8-35fd-4ef9-8d97-11698d87addf.json');
//  });
//
//  it('Regression test for de949c79-7364-473c-a57b-f9a3d7922e0a.json', async () => {
//    await regressionTest('de949c79-7364-473c-a57b-f9a3d7922e0a.json');
//  });
//
//  it('Regression test for dee141c2-2566-4983-91b0-b7e0166bcd82.json', async () => {
//    await regressionTest('dee141c2-2566-4983-91b0-b7e0166bcd82.json');
//  });
//
//  it('Regression test for dfb87e9f-96a4-44ef-a015-7704392ac62a.json', async () => {
//    await regressionTest('dfb87e9f-96a4-44ef-a015-7704392ac62a.json');
//  });
//
//  it('Regression test for e07b41c5-1a2f-47ca-9511-d0d7de70ca9f.json', async () => {
//    await regressionTest('e07b41c5-1a2f-47ca-9511-d0d7de70ca9f.json');
//  });
//
//  it('Regression test for eafb6dcf-ef65-46b6-bd5b-d237e23907da.json', async () => {
//    await regressionTest('eafb6dcf-ef65-46b6-bd5b-d237e23907da.json');
//  });
//
//  it('Regression test for f18a3b53-b309-4978-93f2-0b38d8f3c701.json', async () => {
//    await regressionTest('f18a3b53-b309-4978-93f2-0b38d8f3c701.json');
//  });
//
//  it('Regression test for f6b094da-ad8a-40a5-839f-d2a3c26ae99c.json', async () => {
//    await regressionTest('f6b094da-ad8a-40a5-839f-d2a3c26ae99c.json');
//  });
//
//  it('Regression test for fa9c31cf-6d13-4284-b657-96acee6c387d.json', async () => {
//    await regressionTest('fa9c31cf-6d13-4284-b657-96acee6c387d.json');
//  });
//
//  it('Regression test for fbdf1b14-179b-44c0-a41f-ee2bc84047a6.json', async () => {
//    await regressionTest('fbdf1b14-179b-44c0-a41f-ee2bc84047a6.json');
//  });
//
//  it('Regression test for fe681a8f-b240-4d07-a2dd-67e789907778.json', async () => {
//    await regressionTest('fe681a8f-b240-4d07-a2dd-67e789907778.json');
//  });
});
