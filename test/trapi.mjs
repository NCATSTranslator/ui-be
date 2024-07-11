'use strict'

import { describe, it, before } from 'node:test';
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import * as trapi from '../lib/trapi.mjs';

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

async function creativeAnswersToSummaryTest(testFile) {
  await loadConfig();
  const input = cmn.readJson(`test/data/trapi/in/${testFile}.json`);
  const expected = cmn.readJson(`test/data/trapi/out/${testFile}.json`);
  const maxHops = 3;
  const actual = await trapi.creativeAnswersToSummary("", await input, maxHops);
  assert.deepEqual(reduceSummaryNoise(actual), reduceSummaryNoise(await expected));
}


describe('creativeAnswersToSummary', async () => {
  it('Should return an empty summary for an empty answers array', async () => {
    await creativeAnswersToSummaryTest('empty-summary');
  });

  // Regression tests
  it('Bethlem Myopathy regression test', async () => {
    await creativeAnswersToSummaryTest('mondo-0008029');
  });
});
