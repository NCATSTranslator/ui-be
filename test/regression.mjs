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
  const config = await cfg.bootstrapConfig('./configurations/production.json')
  await loadBiolink(config.biolink);
  await loadChebi();
  loadTrapi(config.trapi);
}

async function regressionTest(testFile) {
  await loadConfig();
  const input = cmn.readJson(`test/data/regression/in/${testFile}`);
  const expected = cmn.readJson(`test/data/regression/out/${testFile}`);
  const maxHops = 3;
  const translatorAdapter = new TranslatorServicexFEAdapter();
  const actual = await translatorAdapter.queryResultsToFE(await input, maxHops);
  tsmy.testSummary(actual.data, await expected);
}


await regressionTest('0949297c-59da-4670-bcbb-475b2a675ac5.json');
await regressionTest('09a69836-7d17-4944-8f8b-c96ae166421a.json');
await regressionTest('0a0bd5ab-a562-4279-b931-d0581f6e2ae7.json');
await regressionTest('0a8f6f97-9978-4d82-96f4-b03a7320be76.json');
await regressionTest('0abaa191-3a5c-4947-86ef-951703361593.json');
await regressionTest('122c68b0-d4f0-466c-9dc0-a92eddccb2f4.json');
await regressionTest('17fcffc5-7274-4a97-85b4-5304f4a127dd.json');
await regressionTest('1d10e168-be8b-4049-ae4e-cbfecd494b56.json');
await regressionTest('1f092040-32ff-43a6-a794-bf50061ea54e.json');
await regressionTest('24fddd22-6a29-4405-80bb-9e1ba8cb822b.json');
await regressionTest('28468ed3-857d-4b52-9699-5702e19d3c3a.json');
await regressionTest('2848f999-ec6d-4184-ae25-7213f995bc0f.json');
await regressionTest('2c511cb6-30c4-4d43-abba-d39779a86428.json');
await regressionTest('309e87ab-a393-4093-a8db-99e73dae0460.json');
await regressionTest('40ba5411-488c-499f-beed-8e430c6cf468.json');
await regressionTest('457d0fcb-bbcd-4019-9713-147a0805c384.json');
await regressionTest('45fdedef-d028-4525-8867-cff35141e515.json');
await regressionTest('46f8950f-eedd-468e-bfeb-0c27a8321877.json');
await regressionTest('4d44252b-397a-436b-979c-25d35eddbb2c.json');
await regressionTest('56e4b8b6-bc72-4a29-8675-f60dfb414147.json');
await regressionTest('5b4e5dbd-4e20-4ed4-a7f2-2d773ab67914.json');
await regressionTest('5dbe1798-eb9f-45e5-94d2-207b56e371d7.json');
await regressionTest('5e71ef14-1945-47d0-8f9a-3f42a13f48eb.json');
await regressionTest('7d06f999-63b0-4b8d-977f-cf6e41e9f715.json');
await regressionTest('8925c5fe-dbe0-40fc-a197-f61cfb35f3ac.json');
await regressionTest('95bcb3ed-c2af-435f-88c1-b62050176f93.json');
await regressionTest('9aa9ad3b-aecf-48f0-a443-68117578e534.json');
await regressionTest('9f28299d-29a3-4b56-9125-a9ee19f5ff67.json');
await regressionTest('a7b1414a-f520-4493-b21a-72d51205ee10.json');
await regressionTest('c7c67896-66d4-499c-9086-598e6c551009.json');
await regressionTest('d4ada9b9-f319-490b-af7d-d1205450f7d5.json');
await regressionTest('e775209e-7176-4ed5-832f-121c90e3978a.json');
await regressionTest('ead65080-c513-4486-879f-3159e5aaea7c.json');
await regressionTest('fabf2e87-bd9e-4cab-836a-ed1ab4d01b2e.json');
await regressionTest('feee7e08-2c1f-4326-a6c6-4db35de12d2c.json');
await regressionTest('ff583af2-3c33-478c-88f9-353c291db304.json');
console.log('All tests passed');
