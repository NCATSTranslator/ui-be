export {
  genNid,
  gen_eid,
  genPid
}

import { default as hash } from 'hash-sum';
import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as pred from './predicates.mjs';

function genNid(rnode, kgraph) {
  return rnode;
}

function gen_eid(redge, kgraph, do_invert = false, is_edge_root) {
  const kedge = trapi.getKedge(redge, kgraph);
  const sub = trapi.getSub(kedge);
  const predicate = pred.genQualifiedPred(kedge, do_invert);
  const obj = trapi.getObj(kedge);
  const edge_type = trapi.getEdgeType(kedge);
  if (do_invert) {
    return genPid([obj, predicate, sub, edge_type, is_edge_root]);
  }

  return genPid([sub, predicate, obj, edge_type, is_edge_root]);
}

function genPid(path) {
  return hash(path);
}

