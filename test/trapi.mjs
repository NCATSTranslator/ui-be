import * as cmn from '../lib/common.mjs';
import * as test from './lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';
import * as bl from '../lib/biolink-model.mjs';

async function testTrapi(rootPath) {
  await test.runTest(trapi.clientReqToTrapiQuery,
                     await cmn.readJson(`${rootPath}/clientReqToTrapiQuery.json`),
                     trapi.loadTrapi);
  await test.runTest(trapi.nodeIdsToTrapiMessage,
                     await cmn.readJson(`${rootPath}/nodeIdsToTrapiMessage.json`),
                     bl.loadBiolink);
  await test.runTest(trapi.getQueryGraph,
                     await cmn.readJson(`${rootPath}/getQueryGraph.json`));
  await test.runTest(trapi.getResults,
                     await cmn.readJson(`${rootPath}/getResults.json`));
  await test.runTest(trapi.getAuxGraphs,
                     await cmn.readJson(`${rootPath}/getAuxGraphs.json`));
  await test.runTest(trapi.getAuxGraph,
                     await cmn.readJson(`${rootPath}/getAuxGraph.json`));
  await test.runTest(trapi.getAuxGraphEdges,
                     await cmn.readJson(`${rootPath}/getAuxGraphEdges.json`));
  await test.runTest(trapi.getEdgeBindings,
                     await cmn.readJson(`${rootPath}/getEdgeBindings.json`));
  await test.runTest(trapi.getNodeBinding,
                     await cmn.readJson(`${rootPath}/getNodeBinding.json`));
  await test.runTest(trapi.getKgraph,
                     await cmn.readJson(`${rootPath}/getKgraph.json`));
  await test.runTest(trapi.getKedge,
                     await cmn.readJson(`${rootPath}/getKedge.json`));
  await test.runTest(trapi.getKnode,
                     await cmn.readJson(`${rootPath}/getKnode.json`));
  await test.runTest(trapi.hasKnode,
                     await cmn.readJson(`${rootPath}/hasKnode.json`));
}

const rootPath = './test/data/trapi';
await testTrapi(rootPath);
