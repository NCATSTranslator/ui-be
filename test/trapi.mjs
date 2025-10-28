import * as cmn from '../lib/common.mjs';
import * as test from './lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';
import * as bl from '../lib/biolink-model.mjs';

async function testTrapi(root_path) {
  await test.functional_test({
      test_func: trapi.client_request_to_trapi_query,
      test_cases: await cmn.readJson(`${root_path}/clientReqToTrapiQuery.json`),
      config_loader: trapi.load_trapi
  });
  await test.functional_test({
      test_func: trapi.get_results,
      test_cases: await cmn.readJson(`${root_path}/getResults.json`)
  });
  await test.functional_test({
      test_func: trapi.get_auxiliary_graphs,
      test_cases: await cmn.readJson(`${root_path}/getAuxGraphs.json`)
  });
  await test.functional_test({
      test_func: trapi.get_auxiliary_graph,
      test_cases: await cmn.readJson(`${root_path}/getAuxGraph.json`)
  });
  await test.functional_test({
      test_func: trapi.get_auxiliary_graph_edges,
      test_cases: await cmn.readJson(`${root_path}/getAuxGraphEdges.json`)
  });
  await test.functional_test({
      test_func: trapi.get_edge_bindings,
      test_cases: await cmn.readJson(`${root_path}/getEdgeBindings.json`)
  });
  await test.functional_test({
      test_func: trapi.get_node_binding,
      test_cases: await cmn.readJson(`${root_path}/getNodeBinding.json`)
  });
  await test.functional_test({
      test_func: trapi.get_kgraph,
      test_cases: await cmn.readJson(`${root_path}/getKgraph.json`)
  });
  await test.functional_test({
      test_func: trapi.get_kedge,
      test_cases: await cmn.readJson(`${root_path}/getKedge.json`)
  });
  await test.functional_test({
      test_func: trapi.get_knode,
      test_cases: await cmn.readJson(`${root_path}/getKnode.json`)
  });
  await test.functional_test({
      test_func: trapi.has_knode,
      test_cases: await cmn.readJson(`${root_path}/hasKnode.json`)
  });
  await test.functional_test({
      test_func: trapi.get_attrs,
      test_cases: await cmn.readJson(`${root_path}/getAttrs.json`)
  });
  await test.functional_test({
      test_func: trapi.get_attr_id,
      test_cases: await cmn.readJson(`${root_path}/getAttrId.json`)
  });
  await test.functional_test({
      test_func: trapi.get_attr_val,
      test_cases: await cmn.readJson(`${root_path}/getAttrVal.json`)
  });
  await test.functional_test({
      test_func: trapi.get_primary_source,
      test_cases: await cmn.readJson(`${root_path}/getPrimarySrc.json`)
  });
  await test.functional_test({
      test_func: trapi.get_subject,
      test_cases: await cmn.readJson(`${root_path}/getSub.json`)
  });
  await test.functional_test({
      test_func: trapi.get_object,
      test_cases: await cmn.readJson(`${root_path}/getObj.json`)
  });
  await test.functional_test({
      test_func: trapi.get_predicate,
      test_cases: await cmn.readJson(`${root_path}/getPred.json`)
  });
  await test.functional_test({
      test_func: trapi.get_support_graphs,
      test_cases: await cmn.readJson(`${root_path}/getSupGraphs.json`)
  });
  await test.functional_test({
      test_func: trapi.get_qualifiers,
      test_cases: await cmn.readJson(`${root_path}/getQualifiers.json`)
  });
  await test.functional_test({
      test_func: trapi.get_qualifier_id,
      test_cases: await cmn.readJson(`${root_path}/getQualifierId.json`)
  });
  await test.functional_test({
      test_func: trapi.get_qualifier_val,
      test_cases: await cmn.readJson(`${root_path}/getQualifierVal.json`)
  });
  await test.functional_test({
      test_func: trapi.get_knowledge_level,
      test_cases: await cmn.readJson(`${root_path}/getKlevel.json`)
  });
  await test.functional_test({
      test_func: trapi.get_agent_type,
      test_cases: await cmn.readJson(`${root_path}/getAgentType.json`)
  });
  await test.functional_test({
      test_func: trapi.message_to_query_type,
      test_cases: await cmn.readJson(`${root_path}/messageToQueryTemplate.json`)
  });
  await test.functional_test({
      test_func: trapi.message_to_endpoints,
      test_cases: await cmn.readJson(`${root_path}/messageToEndpoints.json`),
      config_loader: trapi.load_trapi
  });
  await test.functional_test({
      test_func: trapi.is_chemical_disease_query,
      test_cases: await cmn.readJson(`${root_path}/isChemicalDiseaseQuery.json`)
  });
  await test.functional_test({
      test_func: trapi.is_gene_chemical_query,
      test_cases: await cmn.readJson(`${root_path}/isGeneChemicalQuery.json`)
  });
  await test.functional_test({
      test_func: trapi.is_pathfinder_query,
      test_cases: await cmn.readJson(`${root_path}/isPathfinderQuery.json`)
  });
  await test.functional_test({
      test_func: trapi.is_valid_query,
      test_cases: await cmn.readJson(`${root_path}/isValidQuery.json`)
  });
  await test.functional_test({
      test_func: trapi.make_rule_transform_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_transform_property.json`),
      args_loader: _function_loader,
      post_func: _apply_rule
  });
  await test.functional_test({
      test_func: trapi.make_rule_get_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_get_property.json`),
      args_loader: _function_loader,
      post_func: _apply_rule
  });
  await test.functional_test({
      test_func: trapi.make_rule_transform_and_aggregate_property,
      test_cases: await cmn.readJson(`${root_path}/make_rule_and_aggregate_property.json`),
      args_loader: _function_loader,
      post_func: _apply_rule
  });
}

async function _apply_rule({actual, case_context}) {
  let {source, target, rule_context} = case_context;
  const rule = actual;
  if (!cmn.isArray(source)) {
    source = [source];
  }
  for (const src of source) {
    const transform = rule(src, rule_context);
    target = transform(target);
  }
  return target;
}

function _function_loader(args) {
  const rule_env = Object.freeze({
    identity: (source, key) => cmn.jsonGet(source, key),
    double: (source, key) => 2 * cmn.jsonGet(source, key)
  });
  args = args[0];
  if (typeof args.transform === 'string') {
    args.transform = rule_env[args.transform];
  }
  return [args];
}

const root_path = './test/data/trapi';
await testTrapi(root_path);
