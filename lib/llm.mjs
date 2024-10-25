import * as cmn from './common.mjs';

export function genTopNResultsContext(n, summary) {
  const results = summary.results;
  n = Math.min(n, results.length);
  const resultContexts = [];
  for (let i = 0; i < n; i++) {
    resultContexts.push(genResultContext(results[i], summary));
  }
  return resultContexts;
}

function genResultContext(result, summary) {
  const name = result.drug_name;
  const seenPids = new Set();
  const pathStrings = new Set();
  const pidsLeft = [...result.paths];
  while (!cmn.isArrayEmpty(pidsLeft)) {
    const pid = pidsLeft.pop();
    seenPids.add(pid);
    const path = summary.paths[pid];
    const pathString = genPathString(path, summary);
    if (!pathString) {
      console.error(`Unexpected missing path in summary: ${pid}`);
      continue;
    }
    pathStrings.add(pathString);
    const subgraph = path.subgraph;
    for (let i = 1; i < subgraph.length; i+=2) {
      const edge = summary.edges[subgraph[i]];
      if (!cmn.isArrayEmpty(edge.support)) {
        for (let spid of edge.support) {
          if (!seenPids.has(spid)) {
            pidsLeft.push(spid);
          }
        }
      }
    }
  }
  return {
    name: name,
    paths: [...pathStrings]
  };
}

function genPathString(path, summary) {
  if (path === undefined) {
    return false;
  }
  const pathNames = path.subgraph.map((id, i) => {
    if (i%2 === 0) {
      return getNodeName(id, summary);
    }
    return `[${getPredicateName(id, summary)}]`;
  });
  return pathNames.join('-');
}

function getNodeName(nid, summary) {
  return summary.nodes[nid].names[0];
}

function getPredicateName(eid, summary) {
  return summary.edges[eid].predicate;
}
