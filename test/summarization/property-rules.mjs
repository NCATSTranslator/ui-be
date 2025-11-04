import * as test from '../lib/common.mjs';
import * as cmn from '../../lib/common.mjs';
import * as summary_rules from '../../lib/summarization/property-rules.mjs';

async function _test_summarization_property_rules(root_path) {
  await test.functional_test({
    test_func: summary_rules.make_rule_group_publications_by_knowledge_level,
    test_cases: await cmn.readJson(`${root_path}/make_rule_group_publications_by_knowledge_level.json`),
    post_func: test.apply_rule
  });
  await test.functional_test({
    test_func: summary_rules.make_rule_collect_publication_supporting_text,
    test_cases: await cmn.readJson(`${root_path}/make_rule_collect_publication_supporting_text.json`),
    post_func: test.apply_rule
  });
  await test.functional_test({
    test_func: summary_rules.make_rule_collect_semmed_sentences,
    test_cases: await cmn.readJson(`${root_path}/make_rule_collect_semmed_sentences`),
    post_func: test.apply_rule
  });
}

const root_path = './test/data/summarization';
await _test_summarization_property_rules(root_path);
