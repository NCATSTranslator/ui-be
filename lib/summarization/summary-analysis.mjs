'use strict'
export {
  analysis_to_summary_analysis
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';

function analysis_to_summary_analysis(analysis, kgraph, aux_graphs) {
  const summary_analysis = new SummaryAnalysis(analysis, kgraph, aux_graphs);
  return _finalize_summary_analysis(summary_analysis, kgraph);
}

function _finalize_summary_analysis(summary_analysis, kgraph) {
  for (const nb of summary_analysis.node_ids) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  summary_analysis = _trim_summary_analysis_edges(summary_analysis, ((eid) => {
    const kedge = trapi.getKedge(eid, kgraph);
    return !bl.isBiolinkPred(trapi.getPred(kedge));
  }));
  if (summary_analysis.edge_count() === 0) return false;
  // TODO: do edge support graph replacement here. NEEDED for Pathfinder
  //_collapse_edges(analysis_graph, rules);
  return summary_analysis;
}

class SummaryAnalysis {
  static ROOT = 'root';
  constructor(analysis, kgraph, aux_graphs) {
    this.root = new GraphNode(SummaryAnalysis.ROOT, this._abinds_to_kgbinds(trapi.getEdgeBindings(analysis)));
    this.node_ids = null;
    this._graphs = new Map();
    this._edges = new Map();
    this._add_graph_unsafe(this.root);
    const unprocessed_edges = [];
    for (const eid of this.root.edge_ids) {
      unprocessed_edges.push(new EdgeNode(eid));
    }
    const unprocessed_graphs = [];
    const node_bindings = new Set();
    // Edges and graphs will only ever be processed once
    while (!cmn.isArrayEmpty(unprocessed_graphs) || !cmn.isArrayEmpty(unprocessed_edges)) {
      while (!cmn.isArrayEmpty(unprocessed_edges)) {
        const edge = unprocessed_edges.pop();
        const kedge = trapi.getKedge(edge.id, kgraph);
        if (!kedge) throw new trapi.EdgeBindingNotFoundError(edge.id);
        node_bindings.add(trapi.getSub(kedge));
        node_bindings.add(trapi.getObj(kedge));
        const edge_sup_graphs = trapi.getSupGraphs(kedge);
        edge.set_graph_ids(edge_sup_graphs);
        for (const gid of edge.graph_ids) {
          if (!this._has_graph(gid)) {
            unprocessed_graphs.push(new GraphNode(gid));
          }
        };
        this._add_edge_unsafe(edge);
      }
      while (!cmn.isArrayEmpty(unprocessed_graphs)) {
        const graph = unprocessed_graphs.pop();
        const aux_graph = trapi.getAuxGraph(graph.id, aux_graphs);
        if (!aux_graph) throw new trapi.AuxGraphNotFoundError(graph.id);
        const aux_ebs = trapi.getAuxGraphEdges(aux_graph);
        graph.set_edge_ids(aux_ebs);
        for (const eb of aux_ebs) {
          if (!this._has_edge(eb)) {
            unprocessed_edges.push(new EdgeNode(eb));
          }
        }
        this._add_graph_unsafe(graph);
      }
    }
    this.node_ids = [...node_bindings];
  }

  // TODO: implement collapse edge behavior
//        if (!cmn.isArrayEmpty(edge_sup_graphs) && bl.sanitizeBiolinkItem(trapi.getPred(kedge)) === 'related to') {
//          // What we are doing here is replacing an edge with the graphs that support it. This is done by:
//          const hosts = edge_topology.clone_hosts(eb);
//          // 1. Ensuring the removed edge is not embedded anywhere
//          edge_topology.clear_hosts(eb);
//          // 2. Ensuring all children edges of the removed edge are embedded where the removed edge was embedded
//          // This has to be done in a roundabout manner via edge_hosts because we haven't processed the support graphs yet
//          edge_sup_graphs.forEach((gid) => {
//            if (!processed_sup_graphs.has(gid)) {
//              unprocessed_sup_graphs.push([gid, hosts]);
//            }
//          });
//        } else {
  //
  edge_count() { return this._edges.size }
  edge_ids() { return this._edges.keys() }
  remove_edge_unsafe(eid) {
    this._edges.delete(eid);
    return this;
  }

  _has_graph(gid) { return this._graphs.has(gid); }
  _add_graph_unsafe(graph) {
    this._graphs.set(graph.id, graph);
    return this;
  }
  _has_edge(eid) { return this._edges.has(eid); }
  _add_edge_unsafe(edge) {
    this._edges.set(edge.id, edge);
  }
  _abinds_to_kgbinds(abinds) {
    return Object.values(abinds).reduce((kgbinds, ab) => {
      return kgbinds.concat(ab.map(map_ab_kgb => {
        return cmn.jsonGet(map_ab_kgb, 'id');
      }));
    },
    []);
  }
}

function _trim_summary_analysis_edges(summary_analysis, do_trim) {
  for (const eid of summary_analysis.edge_ids()) {
    if (do_trim(eid)) {
      summary_analysis.remove_edge_unsafe(eid);
    }
  }
  return summary_analysis;
}

class GraphNode {
  constructor(id, edge_ids = []) {
    this.id = id;
    this.edge_ids = edge_ids;
  }

  set_edge_ids(edge_ids) {
    this.edge_ids = [...edge_ids];
  }
}

class EdgeNode {
  constructor(id) {
    this.id = id;
    this.graph_ids = [];
  }

  set_graph_ids(graph_ids) {
    this.graph_ids = [...graph_ids];
  }
}

//function _make_fn_nb_to_outedges(analysis_graph, kgraph) {
//  const map_eb_sopair = __make_map_eb_sopair(analysis_graph, kgraph);
//  const map_nb_outedges = new Map();
//  for (const eb of analysis_graph.edge_bindings) {
//    const { sub, obj } = map_eb_sopair.get(eb);
//    __add_nb_outedge_mapping(map_nb_outedges, sub, __make_outedge(eb, obj));
//    __add_nb_outedge_mapping(map_nb_outedges, obj, __make_outedge(eb, sub));
//  }
//  return (node_binding) => { return map_nb_outedges.get(node_binding); };
//
//  function __make_map_eb_sopair(analysis_graph, kgraph) {
//    const map_eb_sopair = new Map();
//    for (const eb of analysis_graph.edge_bindings) {
//      const kedge = trapi.getKedge(eb, kgraph);
//      map_eb_sopair.set(eb, cmn.make_pair('sub', trapi.getSub(kedge),
//                                          'obj', trapi.getObj(kedge)));
//    }
//    return map_eb_sopair;
//  }
//  function __make_outedge(eb, node) {
//    return cmn.make_pair('eb',  eb, 'target', node);
//  }
//  function __add_nb_outedge_mapping(map, nb, outedge) {
//    let outedges = map.get(nb);
//    if (outedges === undefined) {
//      outedges = [];
//      map.set(nb, outedges);
//    }
//    outedges.push(outedge)
//  }
//}

