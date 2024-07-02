'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import * as trapi from '../lib/trapi.mjs';
import * as cmn from '../lib/common.mjs';

async function blackBoxTest(description, name, f) {
  const input = cmn.readJson(`test/data/trapi/in/${name}.json`);
  const output = cmn.readJson(`test/data/trapi/out/${name}.json`);
  assert.deepEqual(await f.apply(null, await input), await output);
}

describe('creativeAnswersToSummary', () => {
  it('Should return an empty summary for an empty answers array', async () => {
    await blackBoxTest('empty-summary', trapi.creativeAnswersToSummary);
  });
});
