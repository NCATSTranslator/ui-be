import * as cmn from '../lib/common.mjs';
import * as bl from '../lib/biolink-model.mjs';
import * as test from './lib/common.mjs';
import * as sa from '../lib/summarization/summary-analysis.mjs';

async function test_summary_analysis(root_path) {
  await test.functional_test({
    test_func: sa.analysis_to_summary_analysis,
    test_cases: await cmn.readJson(`${root_path}/analysis_to_summary_analysis.json`),
    config_loader: bl.loadBiolink
  });
  await test.functional_test({
    test_func: sa.gen_analysis_paths,
    test_cases: await cmn.readJson(`${root_path}/gen_analysis_paths.json`),
    args_loader: async (args) => {
      const key = args[0];
      args[0] = await test.run_case(sa.analysis_to_summary_analysis,
        await cmn.readJson(`${root_path}/analysis_to_summary_analysis.json`),
        key,
        bl.loadBiolink);
      return args;
    }
  });
}

const root_path = './test/data/summary-analysis';
await test_summary_analysis(root_path);
