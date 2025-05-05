'use strict'
export {
  analysis_to_analysis_topology
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { EdgeTopology } from './EdgeTopology.mjs';

function analysis_to_analysis_topology(analysis, kgraph, aux_graphs) {
   const deconstructed_analysis = _deconstruct_analysis(analysis, kgraph, aux_graphs);
  return _make_analysis_topology(...deconstructed_analysis, kgraph);
}

function _deconstruct_analysis(analysis, kgraph, aux_graphs) {
  const edge_topology = new EdgeTopology();
  const unprocessed_edge_bindings = _abinds_to_kgbinds(trapi.getEdgeBindings(analysis)).map((eb) => {
    edge_topology.make_edge(eb, [EdgeTopology.root_host()]);
    return eb;
  });

  const unprocessed_sup_graphs = [];
  const node_bindings = new Set();
  const processed_edge_bindings = new Set();
  //const processed_sup_graphs = new Set();
  const map_support_eb = new Map();
  // Invariant: edges and subgraphs will only ever be processed once.
  while (!cmn.isArrayEmpty(unprocessed_edge_bindings) || !cmn.isArrayEmpty(unprocessed_sup_graphs)) {
    while (!cmn.isArrayEmpty(unprocessed_edge_bindings)) {
      const eb = unprocessed_edge_bindings.pop();
      if (processed_edge_bindings.has(eb)) continue;
      const kedge = trapi.getKedge(eb, kgraph);
      if (!kedge) throw new trapi.EdgeBindingNotFoundError(eb);
      node_bindings.add(trapi.getSub(kedge));
      node_bindings.add(trapi.getObj(kedge));
      const edge_sup_graphs = trapi.getSupGraphs(kedge);
      edge_topology.set_support(eb, edge_sup_graphs);
      // TODO: replace this check with a configurable version
      if (!cmn.isArrayEmpty(edge_sup_graphs) && bl.sanitizeBiolinkItem(trapi.getPred(kedge)) === 'related to') {
        // What we are doing here is replacing an edge with the graphs that support it. This is done by:
        const hosts = edge_topology.clone_hosts(eb);
        // 1. Ensuring the removed edge is not embedded anywhere
        edge_topology.clear_hosts(eb);
        // 2. Ensuring all children edges of the removed edge are embedded where the removed edge was embedded
        // This has to be done in a roundabout manner via edge_hosts because we haven't processed the support graphs yet
        edge_sup_graphs.forEach((gid) => {
          if (!map_support_eb.has(gid)) {
            unprocessed_sup_graphs.push([gid, hosts]);
          }
        });
      } else {
        edge_sup_graphs.forEach((gid) => {
          if (!map_support_eb.has(gid)) {
            const hosts = [gid];
            unprocessed_sup_graphs.push([gid, hosts]);
          }
        });
      }
      processed_edge_bindings.add(eb);
    };

    while (!cmn.isArrayEmpty(unprocessed_sup_graphs)) {
      const [gid, hosts] = unprocessed_sup_graphs.pop();
      if (map_support_eb.has(gid)) continue;
      const aux_graph = trapi.getAuxGraph(gid, aux_graphs);
      if (!aux_graph) throw new AuxGraphNotFoundError(gid);
      const aux_graph_edge_bindings = trapi.getAuxGraphEdges(aux_graph);
      aux_graph_edge_bindings.forEach((eb) => {
        if (!processed_edge_bindings.has(eb)) {
          edge_topology.make_edge(eb, hosts);
          unprocessed_edge_bindings.push(eb);
        } else {
          // We do not want to process the same edge twice, but we need to include this
          // graph as a graph where this edge occurs.
          edge_topology.add_hosts(eb, hosts);
        }
      });

      map_support_eb.set(gid, cmn.cloneArray(aux_graph_edge_bindings));
    }
  }

  return [node_bindings, edge_topology, map_support_eb];
}

function _make_analysis_topology(node_bindings, edge_topology, map_support_eb, kgraph) {
  for (const nb of node_bindings) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  const valid_edge_ids = edge_topology.edge_ids().filter(eb => {
    const kedge = trapi.getKedge(eb, kgraph);
    // TODO: It would be proper to also delete the edge from edge_topology. The implementation is very complicated because of the cascading effect it could have.
    return bl.isBiolinkPred(trapi.getPred(kedge));
  });
  if (cmn.isArrayEmpty(valid_edge_ids)) return false;
  return new AnalysisTopology(node_bindings, edge_topology, map_support_eb, kgraph);
}

class AnalysisTopology {
  constructor(node_ids, edge_topology, map_support_eid, kgraph) {
    this.node_ids = [...node_ids];
    this.edge_topology = edge_topology;
    this._map_support_eid = map_support_eid;
    this._map_nid_outedges = new Map();
    const edge_ids = this.edge_topology.edge_ids();
    const map_eid_sopair = _make_map_eid_sopair(edge_ids, kgraph);
    for (const eid of edge_ids) {
      const { sub, obj } = map_eid_sopair.get(eid);
      _add_nid_outedge_mapping(this._map_nid_outedges, sub, _make_outedge(eid, obj));
      _add_nid_outedge_mapping(this._map_nid_outedges, obj, _make_outedge(eid, sub));
    }
  }

  nid_to_outedges(nid) {
    return this._map_nid_outedges.get(nid);
  }
}

function _make_map_eid_sopair(edge_bindings, kgraph) {
  const map_eid_sopair = new Map();
  for (const eid of edge_bindings) {
    const kedge = trapi.getKedge(eid, kgraph);
    map_eid_sopair.set(eid, cmn.make_pair('sub', trapi.getSub(kedge),
                                          'obj', trapi.getObj(kedge)));
  }
  return map_eid_sopair;
}

function _make_outedge(eid, node) {
  return cmn.make_pair('eid',  eid, 'target', node);
}

function _add_nid_outedge_mapping(map, nid, outedge) {
  let outedges = map.get(nid);
  if (outedges === undefined) {
    outedges = [];
    map.set(nid, outedges);
  }
  outedges.push(outedge)
}

/**
 * Flatten the Query Graph edge bindings from an Analysis into a list of Knowledge Graph edge bindings
 *
 * @mapping
 * Analysis Edge Bindings ===> [Knowledge Graph Edge Binding]
 * {
 *   <qg_edge_binding> : [
 *     { id: <kg_edge_binding> },
 *     ...
 *   ]
 *   ...
 * }
 * ===>
 * [ <kg_edge_binding>, ... ]
 *
 * @param {object} a_bindings - The bindings to flatten.
 *
 * @returns {string[]} - The list of binding IDs.
 */
function _abinds_to_kgbinds(abinds) {
  return Object.values(abinds).reduce((kgbinds, ab) => {
    return kgbinds.concat(ab.map(map_ab_kgb => {
      return cmn.jsonGet(map_ab_kgb, 'id');
    }));
  },
  []);
}
