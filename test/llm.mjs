import * as cmn from '../lib/common.mjs';
import {testDeep} from './lib/common.mjs';
import {genTopNResultsContext} from '../lib/llm.mjs';

async function test(testFile) {
  const [ns, summary] = await cmn.readJson(`test/data/llm/in/${testFile}`);
  const expected = await cmn.readJson(`test/data/llm/out/${testFile}`);
  for (let i = 0; i < ns.length; i++) {
    const actual = genTopNResultsContext(ns[i], summary);
    testDeep(actual, expected[i]);
  }
}

await test('01.json');
