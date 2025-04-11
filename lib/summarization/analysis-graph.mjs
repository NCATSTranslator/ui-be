'use strict'
export {
  analysis_to_agraph,
  gen_paths,
  finalize_paths
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { EdgeTopology } from './EdgeTopology.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';

function analysis_to_agraph(analysis, kgraph, aux_graphs) {
  const agraph = _deconstruct_analysis(analysis, kgraph, aux_graphs);
  return _validate_agraph(agraph, kgraph);
}

function gen_paths(agraph, kgraph, start, ends, max_path_len) {
  const map_nb_outedges = _make_map_nb_outedges(agraph, kgraph);
  const complete_paths = [];
  const remaining_paths = [[start]];
  while (!cmn.isArrayEmpty(remaining_paths)) {
    const path = remaining_paths.pop();
    const head = path[path.length-1];
    const outedges = map_nb_outedges.get(head);
    const is_terminal_len = (path.length === max_path_len-2);
    if (is_terminal_len) {
      for (let i = 0; i < outedges.length; i++) {
        const outedge = outedges[i];
        const is_end_node = ends.includes(outedge.target);
        const is_cycle = path.includes(outedge.target);
        if (is_end_node && !is_cycle) {
          path.push(outedge.eb, outedge.target);
          complete_paths.push(path);
        }
      }
    } else {
      for (let i = 0; i < outedges.length; i++) {
        const outedge = outedges[i];
        const is_cycle = path.includes(outedge.target);
        if (!is_cycle) {
          const is_end_node = ends.includes(outedge.target);
          if (is_end_node) {
            complete_paths.push([...path, outedge.eb, outedge.target]);
          }
          remaining_paths.push([...path, outedge.eb, outedge.target]);
        } }
    }
  }
  return complete_paths;
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

function _deconstruct_analysis(analysis, kgraph, aux_graphs) {
  const edge_topology = new EdgeTopology();
  const unprocessed_edge_bindings = _abinds_to_kgbinds(trapi.getEdgeBindings(analysis)).map((eb) => {
    edge_topology.make_edge(eb, [_CONSTANTS.ROOT_GRAPH_KEY]);
    return eb;
  });

  const unprocessed_sup_graphs = [];
  const node_bindings = new Set();
  const processed_edge_bindings = new Set();
  const processed_sup_graphs = new Set();
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
          if (!processed_sup_graphs.has(gid)) {
            unprocessed_sup_graphs.push([gid, hosts]);
          }
        });
      } else {
        edge_sup_graphs.forEach((gid) => {
          if (!processed_sup_graphs.has(gid)) {
            const hosts = [gid];
            unprocessed_sup_graphs.push([gid, hosts]);
          }
        });
      }
      processed_edge_bindings.add(eb);
    };

    while (!cmn.isArrayEmpty(unprocessed_sup_graphs)) {
      const [gid, hosts] = unprocessed_sup_graphs.pop();
      if (processed_sup_graphs.has(gid)) continue;
      const aux_graph = trapi.getAuxGraph(gid, aux_graphs);
      if (!aux_graph) throw new AuxGraphNotFoundError(gid);
      trapi.getAuxGraphEdges(aux_graph).forEach((eb) => {
        if (!processed_edge_bindings.has(eb)) {
          edge_topology.make_edge(eb, hosts);
          unprocessed_edge_bindings.push(eb);
        } else {
          // We do not want to process the same edge twice, but we need to include this
          // graph as a graph where this edge occurs.
          edge_topology.add_hosts(eb, hosts);
        }
      });

      processed_sup_graphs.add(gid);
    }
  }
  const agraph = {
    'node_bindings': [...node_bindings],
    'edge_topology': edge_topology
  };
  return agraph;
}

function _validate_agraph(agraph, kgraph) {
  for (const nb of agraph.node_bindings) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  agraph.edge_bindings = agraph.edge_topology.edge_ids().filter(eb => {
    const kedge = trapi.getKedge(eb, kgraph);
    // TODO: It would be proper to also delete the edge from edge_topology. The implementation is very complicated because of the cascading effect it could have.
    return bl.isBiolinkPred(trapi.getPred(kedge));
  });
  if (cmn.isArrayEmpty(agraph.edge_bindings)) return false;
  return agraph;
}

function _make_map_nb_outedges(agraph, kgraph) {
  const map_eb_sopair = __make_map_eb_sopair(agraph, kgraph);
  const map_nb_outedges = new Map();
  for (const eb of agraph.edge_bindings) {
    const { sub, obj } = map_eb_sopair.get(eb);
    __add_nb_outedge_mapping(map_nb_outedges, sub, __make_outedge(eb, obj));
    __add_nb_outedge_mapping(map_nb_outedges, obj, __make_outedge(eb, sub));
  }
  return map_nb_outedges;

  function __make_map_eb_sopair(agraph, kgraph) {
    const map_eb_sopair = new Map();
    for (const eb of agraph.edge_bindings) {
      const kedge = trapi.getKedge(eb, kgraph);
      map_eb_sopair.set(eb, cmn.make_pair('sub', trapi.getSub(kedge),
                                          'obj', trapi.getObj(kedge)));
    }
    return map_eb_sopair;
  }
  function __make_outedge(eb, node) {
    return cmn.make_pair('eb',  eb, 'target', node);
  }
  function __add_nb_outedge_mapping(map, nb, outedge) {
    let outedges = map.get(nb);
    if (outedges === undefined) {
      outedges = [];
      map.set(nb, outedges);
    }
    outedges.push(outedge)
  }
}

function finalize_paths(agraph_paths, analysis_topology, kgraph) {
  function N(n) { return id.genNid(n, kgraph); }
  function E(e, o) { return id.genEid(e, kgraph, pred.isPredInverted(e, o, kgraph)); }
  const summary_topology = new EdgeTopology();
  const normalized_paths = agraph_paths.map(path => {
    let normalized_path = [];
    if ((path.length - 1) < 0) return normalized_path;
    for (let i = 0; i < (path.length - 1); i+=2) {
      const nb = path[i];
      const eb = path[i+1];
      const eid = E(eb, nb);
      if (!summary_topology.has_edge(eid)) {
        summary_topology.make_edge(eid,
                                   analysis_topology.clone_hosts(eb),
                                   analysis_topology.clone_support(eb));
      } else {
        summary_topology.add_hosts(eid, analysis_topology.clone_hosts(eb));
        summary_topology.add_support(eid, analysis_topology.clone_support(eb));
      }
      const nid = N(nb);
      normalized_path.push(nid, eid);
    }

    normalized_path.push(N(path[path.length - 1]));
    return normalized_path;
  });
  const path_hosts = new Map();
  // Find which graphs host which paths
  // A path is hosted in a graph if all of the path's edges are hosted in the graph
  for (const path of normalized_paths) {
    const path_eids = [];
    for (let i = 1; i < path.length; i+=2) {
      path_eids.push(path[i]);
    }
    const hosts = summary_topology.find_path_hosts(path_eids);
    if (!cmn.isArrayEmpty(hosts)) {
      path_hosts.set(id.genPid(path), hosts)
    }
  }

  const edge_bases = {}
  // Find which paths support which edges
  // A path supports an edge if the host of the path is the same as the support for the edge
  for (const eid of summary_topology.edge_ids()) {
    const edge_support_paths = [];
    for (const [pid, hosts] of path_hosts) {
      for (const path_host of hosts) {
        if (summary_topology.has_support(eid, path_host)) {
          edge_support_paths.push(pid);
        }
      }
    }
    if (!edge_bases[eid]) {
      edge_bases[eid] = new SummaryEdge();
    }
    const sedge = edge_bases[eid];
    sedge.extendSupPaths(edge_support_paths);
    sedge.isRootPath = summary_topology.has_host(eid, _CONSTANTS.ROOT_GRAPH_KEY);
    sedge.type = sedge.hasSupport() ? trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT : trapi.CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
  }

  return [normalized_paths, edge_bases];
}

const _CONSTANTS = Object.freeze({
  ROOT_GRAPH_KEY: 'root'
});
