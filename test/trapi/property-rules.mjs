import * as trapi_rules from '../../lib/trapi/property-rules.mjs';
import * as cmn from '../../lib/common.mjs';
import * as test from '../lib/common.mjs';

async function _test_trapi_property_rules(root_path, env) {
  await test.functional_test({
      test_func: trapi_rules.make_rule_transform_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_transform_property.json`),
      args_loader: test.gen_function_loader(env),
      post_func: test.apply_rule
  });
  await test.functional_test({
      test_func: trapi_rules.make_rule_get_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_get_property.json`),
      args_loader: test.gen_function_loader(env),
      post_func: test.apply_rule
  });
  await test.functional_test({
      test_func: trapi_rules.make_rule_transform_and_aggregate_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_transform_and_aggregate_property.json`),
      args_loader: test.gen_function_loader(env),
      post_func: test.apply_rule
  });
  await test.functional_test({
      test_func: trapi_rules.make_rule_aggregate_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_aggregate_property.json`),
      post_func: test.apply_rule
  });
  await test.functional_test({
      test_func: trapi_rules.make_rule_transform_and_rename_attribute_value,
      test_cases: await cmn.readJson(`${root_path}/make_rule_transform_and_rename_attribute_value.json`),
      args_loader: test.gen_function_loader(env),
      post_func: test.apply_rule
  });
  await test.functional_test({
      test_func: trapi_rules.make_rule_map_attributes,
      test_cases: await cmn.readJson(`${root_path}/make_rule_map_attributes.json`),
      args_loader: test.gen_function_loader(env),
      post_func: test.apply_rule
  });
}

const root_path = './test/data/trapi';
const rule_env = Object.freeze({
  get: (source, key) => cmn.jsonGet(source, key),
  get_double: (source, key) => 2 * cmn.jsonGet(source, key),
  identity: val => val,
  double: val => 2 * val
});
await _test_trapi_property_rules(root_path, rule_env);
