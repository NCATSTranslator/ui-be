import * as llm from '../../lib/llm.mjs';
import * as cmn from '../../lib/common.mjs';
import {QUERY_TYPE, UnknownQueryError} from '../../lib/trapi.mjs';

function getRandIntInRange(min, max) {
  return Math.floor(Math.random() * (max - min)) + min;
}

function getNRandIntsInRange(n, min, max) {
  const ints = new Set();
  while (ints.size !== n) {
    ints.add(getRandIntInRange(min, max));
  }

  return [...ints];
}

function genQueryStr(summary) {
  const queryType = summary.meta.query_type;
  const result = summary.results[0];
  const subgraph = summary.paths[result.paths[0]].subgraph;
  const predicate = summary.edges[subgraph[1]].predicate;
  const objectName = summary.nodes[result.object].names[0];
  switch(queryType) {
    case QUERY_TYPE.CHEMICAL_GENE:
      return `What chemical ${predicate} ${objectName}?`;
    case QUERY_TYPE.CHEMICAL_DISEASE:
      return `What chemical ${predicate} ${objectName}?`;
    case QUERY_TYPE.GENE_CHEMICAL:
      return `What gene ${predicate} ${objectName}?`;
    case PATHFINDER:
      const subjectName = summary.nodes[result.subject].names[0];
      return `How is ${subjectName} related to ${objectName}?`;
    default:
      throw UnknownQueryError(queryType);
  }
}

const dataPath = process.argv[2];
const testCases = [];
const casesToGenerate = 100;
const summary = await cmn.readJson(dataPath);
const results = summary.results;
const resultCount = results.length;
for (let tc = 0; tc < casesToGenerate; tc++) {
  const contextCount = Math.min(getRandIntInRange(10, 50), resultCount);
  const resultKeys = getNRandIntsInRange(contextCount, 0, resultCount);
  testCases.push({
    query: genQueryStr(summary),
    results: resultKeys.map(k => llm.genResultContext(results[k], summary))
  });
}
console.log(JSON.stringify(testCases));
