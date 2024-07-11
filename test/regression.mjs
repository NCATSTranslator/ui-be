'use strict'
import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import { TranslatorServicexFEAdapter } from '../adapters/TranslatorServicexFEAdapter.mjs';

// We have to do this because the 'before' hook does not seem to work
async function loadConfig() {
  const config = await cfg.bootstrapConfig('test/data/regression/config.json')
  await loadBiolink(config.biolink.version,
                    config.biolink.support_deprecated_predicates,
                    config.biolink.infores_catalog,
                    config.biolink.prefix_catalog);
  await loadChebi();
}

function reduceSummaryNoise(summary) {
  summary.meta = null;
  summary.errors = null;
}

async function regressionTest(testFile) {
  await loadConfig();
  const input = cmn.readJson(`test/data/regression/in/${testFile}.json`);
  const expected = cmn.readJson(`test/data/regression/out/${testFile}.json`);
  const maxHops = 3;
  const translatorAdapter = new TranslatorServicexFEAdapter();
  const actual = await translatorAdapter.queryResultsToFE(await input, maxHops);
  assert.deepEqual(reduceSummaryNoise(actual.data), reduceSummaryNoise(await expected));
}

describe('Regression Tests', async () => {

  it('Regression test for cb16276d-8379-4cae-92b2-0d4f0ba421ba', async () => {
    await regressionTest('cb16276d-8379-4cae-92b2-0d4f0ba421ba');
  });

  it('Regression test for 0eec85f0-88bf-4ec7-ad4e-3dc9b41391a1', async () => {
    await regressionTest('0eec85f0-88bf-4ec7-ad4e-3dc9b41391a1');
  });

  it('Regression test for e559666b-f7de-489b-a34f-4eb4b456630b', async () => {
    await regressionTest('e559666b-f7de-489b-a34f-4eb4b456630b');
  });

  it('Regression test for 907143de-e017-4c6d-8ff0-2b0d05fd3377', async () => {
    await regressionTest('907143de-e017-4c6d-8ff0-2b0d05fd3377');
  });

  it('Regression test for bd58b7a6-73b7-4a3f-8c6a-430f6947a2fa', async () => {
    await regressionTest('bd58b7a6-73b7-4a3f-8c6a-430f6947a2fa');
  });

  it('Regression test for f2f60055-b59f-4b75-80bf-55bf67d01688', async () => {
    await regressionTest('f2f60055-b59f-4b75-80bf-55bf67d01688');
  });

  it('Regression test for 225150d1-1705-4ef3-83fd-6daab63b6fa8', async () => {
    await regressionTest('225150d1-1705-4ef3-83fd-6daab63b6fa8');
  });

  it('Regression test for a4b63cba-1b6e-49d0-a130-64dbcf50271b', async () => {
    await regressionTest('a4b63cba-1b6e-49d0-a130-64dbcf50271b');
  });

  it('Regression test for bfbcb4f2-574a-42dc-be8d-48583fb7167a', async () => {
    await regressionTest('bfbcb4f2-574a-42dc-be8d-48583fb7167a');
  });

  it('Regression test for b165076f-f37a-4423-ac0e-15472151d30a', async () => {
    await regressionTest('b165076f-f37a-4423-ac0e-15472151d30a');
  });

  it('Regression test for 12b5ddda-995b-47c5-966d-140b1c277ca4', async () => {
    await regressionTest('12b5ddda-995b-47c5-966d-140b1c277ca4');
  });

  it('Regression test for 1be4815d-4849-422b-a55a-4aede6006726', async () => {
    await regressionTest('1be4815d-4849-422b-a55a-4aede6006726');
  });

  it('Regression test for 768b07da-715b-4849-a906-cd9acb03d887', async () => {
    await regressionTest('768b07da-715b-4849-a906-cd9acb03d887');
  });

  it('Regression test for c47bcb40-f965-424a-b99f-1f89c5dcc628', async () => {
    await regressionTest('c47bcb40-f965-424a-b99f-1f89c5dcc628');
  });

  it('Regression test for 6443aa34-4b08-4b44-a53d-cb1e7285cadc', async () => {
    await regressionTest('6443aa34-4b08-4b44-a53d-cb1e7285cadc');
  });

  it('Regression test for 9d5cabdd-7642-4970-b01c-329324719e8c', async () => {
    await regressionTest('9d5cabdd-7642-4970-b01c-329324719e8c');
  });

  it('Regression test for 041900d4-dbc5-483b-b73a-51a0af9bd910', async () => {
    await regressionTest('041900d4-dbc5-483b-b73a-51a0af9bd910');
  });

  it('Regression test for 44a6420c-23b7-44f4-b10b-545bbe358599', async () => {
    await regressionTest('44a6420c-23b7-44f4-b10b-545bbe358599');
  });

  it('Regression test for 6f95047c-00c8-4862-8a93-264d0f477243', async () => {
    await regressionTest('6f95047c-00c8-4862-8a93-264d0f477243');
  });

  it('Regression test for 8e2a8a4b-a7d6-42e3-a762-238923a60df5', async () => {
    await regressionTest('8e2a8a4b-a7d6-42e3-a762-238923a60df5');
  });

  it('Regression test for caba4861-65c4-4a03-ac67-fa0dd61a5124', async () => {
    await regressionTest('caba4861-65c4-4a03-ac67-fa0dd61a5124');
  });

  it('Regression test for 8aa05017-6e89-4858-b71b-10c0e2c7ea95', async () => {
    await regressionTest('8aa05017-6e89-4858-b71b-10c0e2c7ea95');
  });

  it('Regression test for fa78c1f0-a3ff-467d-b493-9235614d084f', async () => {
    await regressionTest('fa78c1f0-a3ff-467d-b493-9235614d084f');
  });

  it('Regression test for 41a288e1-42ab-4cad-883b-a4c848e8146c', async () => {
    await regressionTest('41a288e1-42ab-4cad-883b-a4c848e8146c');
  });

  it('Regression test for 97be0b97-cc7d-43cd-93c0-18fd7549892d', async () => {
    await regressionTest('97be0b97-cc7d-43cd-93c0-18fd7549892d');
  });

  it('Regression test for 37d6fa3d-3627-4229-acae-0c1196a9bf39', async () => {
    await regressionTest('37d6fa3d-3627-4229-acae-0c1196a9bf39');
  });

  it('Regression test for a5a7eee7-c627-414d-85a6-08e9955c3a7c', async () => {
    await regressionTest('a5a7eee7-c627-414d-85a6-08e9955c3a7c');
  });

  it('Regression test for 538f4bdb-6639-4465-ab19-4de7517b270e', async () => {
    await regressionTest('538f4bdb-6639-4465-ab19-4de7517b270e');
  });

  it('Regression test for 19bf663b-1018-4f62-9ac4-9b0a2e0f2a8b', async () => {
    await regressionTest('19bf663b-1018-4f62-9ac4-9b0a2e0f2a8b');
  });

  it('Regression test for f6c52599-8733-42a7-8b87-a0feffb5d853', async () => {
    await regressionTest('f6c52599-8733-42a7-8b87-a0feffb5d853');
  });

  it('Regression test for d2bf1ca7-32a1-47b7-b2c1-c3bb11a92bfd', async () => {
    await regressionTest('d2bf1ca7-32a1-47b7-b2c1-c3bb11a92bfd');
  });

  it('Regression test for 63b8efad-b314-44ec-a637-ed2bc920e2ac', async () => {
    await regressionTest('63b8efad-b314-44ec-a637-ed2bc920e2ac');
  });

  it('Regression test for e8e540bf-45c3-453f-8fc6-5df32e7e90d6', async () => {
    await regressionTest('e8e540bf-45c3-453f-8fc6-5df32e7e90d6');
  });

  it('Regression test for a33418b9-2952-441b-aa35-ba158fd14004', async () => {
    await regressionTest('a33418b9-2952-441b-aa35-ba158fd14004');
  });

  it('Regression test for f2ff6e25-7a49-4b26-a282-8011600e2188', async () => {
    await regressionTest('f2ff6e25-7a49-4b26-a282-8011600e2188');
  });

  it('Regression test for 99624abf-08f2-41ce-b52a-8658f246bb38', async () => {
    await regressionTest('99624abf-08f2-41ce-b52a-8658f246bb38');
  });

  it('Regression test for f56f2eb7-6438-41e3-833f-be0cbeeacd1a', async () => {
    await regressionTest('f56f2eb7-6438-41e3-833f-be0cbeeacd1a');
  });
});
