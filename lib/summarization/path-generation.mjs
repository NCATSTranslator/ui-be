'use strict'

export {
  gen_paths,
  finalize_paths
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as pred from './predicates.mjs';
import * as id from './identifiers.mjs';
import { EdgeTopology } from './EdgeTopology.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';

function gen_paths(agraph, kgraph, start, ends, max_path_len) {
  const complete_paths = [];
  const remaining_paths = [[start]];
  while (!cmn.isArrayEmpty(remaining_paths)) {
    const path = remaining_paths.pop();
    const head = path[path.length-1];
    const outedges = agraph.nb_to_outedges(head);
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

function finalize_paths(agraph_paths, analysis_topology, kgraph) {
  const summary_topology = new EdgeTopology();
  const edge_metadata = new Map();
  const normalized_paths = agraph_paths.map(path => {
    let normalized_path = [];
    if ((path.length - 1) < 0) return normalized_path;
    for (let i = 0; i < (path.length - 1); i+=2) {
      const nb = path[i];
      const eb = path[i+1];
      const is_edge_inverted = pred.isPredInverted(eb, nb, kgraph);
      const eid = id.genEid(eb, kgraph, is_edge_inverted);
      if (!edge_metadata.has(eid)) {
        edge_metadata.set(eid, {
          edge_bindings: [],
          inverted_id: null
        });
      }
      edge_metadata.get(eid).edge_bindings.push(eb);
      if (is_edge_inverted) {
        edge_metadata.get(eid).inverted_id = id.genEid(eb, kgraph);
      }
      if (!summary_topology.has_edge(eid)) {
        summary_topology.make_edge(eid,
                                   analysis_topology.clone_hosts(eb),
                                   analysis_topology.clone_support(eb));
      } else {
        summary_topology.add_hosts(eid, analysis_topology.clone_hosts(eb));
        summary_topology.add_support(eid, analysis_topology.clone_support(eb));
      }
      const nid = id.genNid(nb, kgraph);
      normalized_path.push(nid, eid);
    }

    normalized_path.push(id.genNid(path[path.length - 1]));
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

  const summary_edges = {}
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
    if (!summary_edges[eid]) {
      summary_edges[eid] = new SummaryEdge();
    }
    const sedge = summary_edges[eid];
    sedge.extendSupPaths(edge_support_paths);
    sedge.isRootPath = summary_topology.is_edge_root(eid);
    sedge.type = sedge.hasSupport() ? trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT : trapi.CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
    sedge.metadata = edge_metadata.get(eid);
  }

  return [normalized_paths, summary_edges];
}

