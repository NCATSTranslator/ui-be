import * as cmn from '../lib/common.mjs';
import * as bl from '../lib/biolink-model.mjs';
import * as test from './lib/common.mjs';
import * as sa from '../lib/summarization/summary-analysis.mjs';

async function test_summary_analysis(root_path) {
  await test.functionalTest(sa.analysis_to_summary_analysis,
                            await cmn.readJson(`${root_path}/analysis_to_summary_analysis.json`),
                            bl.loadBiolink);
  await test.functionalTest(sa.gen_analysis_summary_paths,
                            await cmn.readJson(`${root_path}/gen_analysis_summary_paths.json`),
                            null,
                            async (args) => {
                              const key = args[0];
                              args[0] = await test.runCase(sa.analysis_to_summary_analysis,
                                await cmn.readJson(`${root_path}/analysis_to_summary_analysis.json`),
                                key,
                                bl.loadBiolink);
                              return args;
                            });
}

const root_path = './test/data/summary-analysis';
await test_summary_analysis(root_path);
