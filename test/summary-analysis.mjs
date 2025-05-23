import * as cmn from '../lib/common.mjs';
import * as bl from '../lib/biolink-model.mjs';
import * as test from './lib/common.mjs';
import { analysis_to_summary_analysis } from '../lib/summarization/summary-analysis.mjs';

async function test_summary_analysis(root_path) {
  await test.functionalTest(analysis_to_summary_analysis,
                            await cmn.readJson(`${root_path}/analysis_to_summary_analysis.json`),
                            bl.loadBiolink)
}

const root_path = './test/data/summary-analysis';
await test_summary_analysis(root_path);
