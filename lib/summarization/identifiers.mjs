export {
  gen_nid,
  gen_eid,
  gen_pid
}

import { default as hash } from 'hash-sum';
import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as pred from './predicates.mjs';

function gen_nid(rnode, kgraph) {
  return rnode;
}

function gen_eid(redge, kgraph, do_invert = false, is_edge_root) {
  const kedge = trapi.getKedge(redge, kgraph);
  const sub = trapi.getSub(kedge);
  const predicate = pred.genQualifiedPred(kedge, do_invert);
  const obj = trapi.getObj(kedge);
  const edge_type = trapi.getEdgeType(kedge);
  if (do_invert) {
    return gen_pid([obj, predicate, sub, edge_type, is_edge_root]);
  }

  return gen_pid([sub, predicate, obj, edge_type, is_edge_root]);
}

function gen_pid(path) {
  return hash(path);
}

