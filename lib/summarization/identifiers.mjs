export {
  genNid,
  genEid,
  genPid
}

import { default as hash } from 'hash-sum';
import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as pred from './predicates.mjs';

function genNid(rnode, kgraph) {
  return rnode;
}

function genEid(redge, kgraph, doInvert = false) {
  const kedge = trapi.getKedge(redge, kgraph);
  const sub = trapi.getSub(kedge);
  const predicate = pred.genQualifiedPred(kedge, doInvert);
  const obj = trapi.getObj(kedge);
  const edge_type = trapi.getEdgeType(kedge);
  if (doInvert) {
    return genPid([obj, predicate, sub, edge_type]);
  }

  return genPid([sub, predicate, obj, edge_type]);
}

function genPid(path) {
  return hash(path);
}

