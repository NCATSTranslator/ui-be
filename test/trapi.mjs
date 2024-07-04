'use strict'

import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import * as trapi from '../lib/trapi.mjs';

async function blackBoxTest(name, f) {
  const input = cmn.readJson(`test/data/trapi/in/${name}.json`);
  const output = cmn.readJson(`test/data/trapi/out/${name}.json`);
  return [f.apply(null, await input), await output];
}

// We have to do this because the 'before' hook does not seem to work
async function loadConfig() {
  const config = await cfg.bootstrapConfig('test/data/trapi/config.json')
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

async function creativeAnswersToSummaryTest(pathToData) {
  loadConfig();
  const [actual, expected] = await blackBoxTest(pathToData, trapi.creativeAnswersToSummary);
  assert.deepEqual(reduceSummaryNoise(actual), reduceSummaryNoise(expected));
}


describe('creativeAnswersToSummary', async () => {
  it('Should return an empty summary for an empty answers array', async () => {
    creativeAnswersToSummaryTest('empty-summary');
  });

  // Regression tests
  it('Heart Disorder regression test', async () => {
    creativeAnswersToSummaryTest('mondo-0005267');
  });
});
