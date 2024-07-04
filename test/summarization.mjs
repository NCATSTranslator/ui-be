'use strict'

import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import { loadTrapi } from '../lib/trapi.mjs';
import * as sum from '../lib/summarization.mjs';

async function blackBoxTest(name, f) {
  const input = cmn.readJson(`test/data/summarization/in/${name}.json`);
  const output = cmn.readJson(`test/data/summarization/out/${name}.json`);
  return [f.apply(null, await input), await output];
}

// We have to do this because the 'before' hook does not seem to work
async function loadConfig() {
  const config = await cfg.bootstrapConfig('test/data/summarization/config.json')
  await loadBiolink(config.biolink);
  await loadChebi();
  await loadTrapi(config.trapi);
}

function reduceSummaryNoise(summary) {
  summary.meta = null;
  summary.errors = null;
}

async function answersToSummaryTest(pathToData) {
  await loadConfig();
  const [actual, expected] = await blackBoxTest(pathToData, sum.answersToSummary);
  assert.deepEqual(reduceSummaryNoise(actual), reduceSummaryNoise(expected));
}


describe('answersToSummary', async () => {
  it('Should return an empty summary for an empty answers array', async () => {
    await answersToSummaryTest('empty-summary');
  });

  // Regression tests
  it('Heart Disorder regression test', async () => {
    await answersToSummaryTest('mondo-0005267');
  });
});
