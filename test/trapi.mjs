import * as cmn from '../lib/common.mjs';
import * as test from './lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';
import * as bl from '../lib/biolink-model.mjs';

async function testTrapi(rootPath) {
  await test.functionalTest(trapi.client_request_to_trapi_query,
                            await cmn.readJson(`${rootPath}/clientReqToTrapiQuery.json`),
                            trapi.load_trapi);
  await test.functionalTest(trapi.get_results,
                            await cmn.readJson(`${rootPath}/getResults.json`));
  await test.functionalTest(trapi.get_auxiliary_graphs,
                            await cmn.readJson(`${rootPath}/getAuxGraphs.json`));
  await test.functionalTest(trapi.get_auxiliary_graph,
                            await cmn.readJson(`${rootPath}/getAuxGraph.json`));
  await test.functionalTest(trapi.get_auxiliary_graph_edges,
                            await cmn.readJson(`${rootPath}/getAuxGraphEdges.json`));
  await test.functionalTest(trapi.get_edge_bindings,
                            await cmn.readJson(`${rootPath}/getEdgeBindings.json`));
  await test.functionalTest(trapi.get_node_binding,
                            await cmn.readJson(`${rootPath}/getNodeBinding.json`));
  await test.functionalTest(trapi.get_kgraph,
                            await cmn.readJson(`${rootPath}/getKgraph.json`));
  await test.functionalTest(trapi.get_kedge,
                            await cmn.readJson(`${rootPath}/getKedge.json`));
  await test.functionalTest(trapi.get_knode,
                            await cmn.readJson(`${rootPath}/getKnode.json`));
  await test.functionalTest(trapi.has_knode,
                            await cmn.readJson(`${rootPath}/hasKnode.json`));
  await test.functionalTest(trapi.get_attrs,
                            await cmn.readJson(`${rootPath}/getAttrs.json`));
  await test.functionalTest(trapi.get_attr_id,
                            await cmn.readJson(`${rootPath}/getAttrId.json`));
  await test.functionalTest(trapi.get_attr_val,
                            await cmn.readJson(`${rootPath}/getAttrVal.json`));
  await test.functionalTest(trapi.get_primary_source,
                            await cmn.readJson(`${rootPath}/getPrimarySrc.json`));
  await test.functionalTest(trapi.get_subject,
                            await cmn.readJson(`${rootPath}/getSub.json`));
  await test.functionalTest(trapi.get_object,
                            await cmn.readJson(`${rootPath}/getObj.json`));
  await test.functionalTest(trapi.get_predicate,
                            await cmn.readJson(`${rootPath}/getPred.json`));
  await test.functionalTest(trapi.get_support_graphs,
                            await cmn.readJson(`${rootPath}/getSupGraphs.json`));
  await test.functionalTest(trapi.get_qualifiers,
                            await cmn.readJson(`${rootPath}/getQualifiers.json`));
  await test.functionalTest(trapi.get_qualifier_id,
                            await cmn.readJson(`${rootPath}/getQualifierId.json`));
  await test.functionalTest(trapi.get_qualifier_val,
                            await cmn.readJson(`${rootPath}/getQualifierVal.json`));
  await test.functionalTest(trapi.get_knowledge_level,
                            await cmn.readJson(`${rootPath}/getKlevel.json`));
  await test.functionalTest(trapi.get_agent_type,
                            await cmn.readJson(`${rootPath}/getAgentType.json`));
  await test.functionalTest(trapi.message_to_query_type,
                            await cmn.readJson(`${rootPath}/messageToQueryTemplate.json`));
  await test.functionalTest(trapi.message_to_endpoints,
                            await cmn.readJson(`${rootPath}/messageToEndpoints.json`),
                            trapi.load_trapi);
  await test.functionalTest(trapi.is_chemical_disease_query,
                            await cmn.readJson(`${rootPath}/isChemicalDiseaseQuery.json`));
  await test.functionalTest(trapi.is_gene_chemical_query,
                            await cmn.readJson(`${rootPath}/isGeneChemicalQuery.json`));
  await test.functionalTest(trapi.is_pathfinder_query,
                            await cmn.readJson(`${rootPath}/isPathfinderQuery.json`));
  await test.functionalTest(trapi.is_valid_query,
                            await cmn.readJson(`${rootPath}/isValidQuery.json`));
  await test.classTest(trapi.AttributeIterator,
                       await cmn.readJson(`${rootPath}/AttributeIterator.json`));

}

const rootPath = './test/data/trapi';
await testTrapi(rootPath);
