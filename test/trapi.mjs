import * as cmn from '../lib/common.mjs';
import * as test from './lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';
import * as bl from '../lib/biolink-model.mjs';

async function testTrapi(rootPath) {
  await test.functionalTest(trapi.clientReqToTrapiQuery,
                            await cmn.readJson(`${rootPath}/clientReqToTrapiQuery.json`),
                            trapi.loadTrapi);
  await test.functionalTest(trapi.nodeIdsToTrapiMessage,
                            await cmn.readJson(`${rootPath}/nodeIdsToTrapiMessage.json`),
                            bl.loadBiolink);
  await test.functionalTest(trapi.getQueryGraph,
                            await cmn.readJson(`${rootPath}/getQueryGraph.json`));
  await test.functionalTest(trapi.getResults,
                            await cmn.readJson(`${rootPath}/getResults.json`));
  await test.functionalTest(trapi.getAuxGraphs,
                            await cmn.readJson(`${rootPath}/getAuxGraphs.json`));
  await test.functionalTest(trapi.getAuxGraph,
                            await cmn.readJson(`${rootPath}/getAuxGraph.json`));
  await test.functionalTest(trapi.getAuxGraphEdges,
                            await cmn.readJson(`${rootPath}/getAuxGraphEdges.json`));
  await test.functionalTest(trapi.getEdgeBindings,
                            await cmn.readJson(`${rootPath}/getEdgeBindings.json`));
  await test.functionalTest(trapi.getNodeBinding,
                            await cmn.readJson(`${rootPath}/getNodeBinding.json`));
  await test.functionalTest(trapi.getKgraph,
                            await cmn.readJson(`${rootPath}/getKgraph.json`));
  await test.functionalTest(trapi.getKedge,
                            await cmn.readJson(`${rootPath}/getKedge.json`));
  await test.functionalTest(trapi.getKnode,
                            await cmn.readJson(`${rootPath}/getKnode.json`));
  await test.functionalTest(trapi.hasKnode,
                            await cmn.readJson(`${rootPath}/hasKnode.json`));
  await test.classTest(trapi.AttributeIterator,
                       await cmn.readJson(`${rootPath}/AttributeIterator.json`));
  await test.functionalTest(trapi.getAttrs,
                            await cmn.readJson(`${rootPath}/getAttrs.json`));
  await test.functionalTest(trapi.getAttrId,
                            await cmn.readJson(`${rootPath}/getAttrId.json`));
  await test.functionalTest(trapi.getAttrVal,
                            await cmn.readJson(`${rootPath}/getAttrVal.json`));
  await test.functionalTest(trapi.noAttrs,
                            await cmn.readJson(`${rootPath}/noAttrs.json`));
  await test.functionalTest(trapi.getPrimarySrc,
                            await cmn.readJson(`${rootPath}/getPrimarySrc.json`));
  await test.functionalTest(trapi.getSub,
                            await cmn.readJson(`${rootPath}/getSub.json`));
  await test.functionalTest(trapi.getObj,
                            await cmn.readJson(`${rootPath}/getObj.json`));
  await test.functionalTest(trapi.getPred,
                            await cmn.readJson(`${rootPath}/getPred.json`));
  await test.functionalTest(trapi.getSupGraphs,
                            await cmn.readJson(`${rootPath}/getSupGraphs.json`));
  await test.functionalTest(trapi.getQualifiers,
                            await cmn.readJson(`${rootPath}/getQualifiers.json`));
  await test.functionalTest(trapi.getQualifierId,
                            await cmn.readJson(`${rootPath}/getQualifierId.json`));
  await test.functionalTest(trapi.getQualifierVal,
                            await cmn.readJson(`${rootPath}/getQualifierVal.json`));
  await test.functionalTest(trapi.getKlevel,
                            await cmn.readJson(`${rootPath}/getKlevel.json`));
  await test.functionalTest(trapi.getAgentType,
                            await cmn.readJson(`${rootPath}/getAgentType.json`));
  await test.functionalTest(trapi.messageToQueryTemplate,
                            await cmn.readJson(`${rootPath}/messageToQueryTemplate.json`));
  await test.functionalTest(trapi.messageToEndpoints,
                            await cmn.readJson(`${rootPath}/messageToEndpoints.json`),
                            trapi.loadTrapi);
  await test.functionalTest(trapi.isChemicalGeneQuery,
                            await cmn.readJson(`${rootPath}/isChemicalGeneQuery.json`));
  await test.functionalTest(trapi.isChemicalDiseaseQuery,
                            await cmn.readJson(`${rootPath}/isChemicalDiseaseQuery.json`));
  await test.functionalTest(trapi.isGeneChemicalQuery,
                            await cmn.readJson(`${rootPath}/isGeneChemicalQuery.json`));
  await test.functionalTest(trapi.isPathfinderQuery,
                            await cmn.readJson(`${rootPath}/isPathfinderQuery.json`));
  await test.functionalTest(trapi.isValidQuery,
                            await cmn.readJson(`${rootPath}/isValidQuery.json`));

}

const rootPath = './test/data/trapi';
await testTrapi(rootPath);
