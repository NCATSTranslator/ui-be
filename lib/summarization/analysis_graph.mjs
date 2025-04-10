'use strict'
export {
  analysis_to_agraph,
  gen_paths,
  EdgeBindingNotFoundError
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';

function analysis_to_agraph(analysis, kgraph, aux_graphs) {
  const agraph = deconstruct_analysis(analysis, kgraph, aux_graphs);
  return validate_agraph(agraph, kgraph);
}

function gen_paths(agraph, kgraph, start, ends, max_path_len) {
  const map_nb_outedges = make_map_nb_outedges(agraph, kgraph);
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

class EdgeBindingNotFoundError extends Error {
  constructor(edgeBinding) {
    super(`Edge binding not found for ${JSON.stringify(edgeBinding)}`);
  }
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
function abinds_to_kgbinds(abinds) {
  return Object.values(abinds).reduce((kgbinds, ab) => {
    return kgbinds.concat(ab.map(map_ab_kgb => {
      return cmn.jsonGet(map_ab_kgb, 'id');
    }));
  },
  []);
}

function deconstruct_analysis(analysis, kgraph, aux_graphs) {
  const edge_topology = new Map(); // This doubles as the set of processed edge bindings
  const unprocessed_edge_bindings = abinds_to_kgbinds(trapi.getEdgeBindings(analysis)).map((eb) => {
    edge_topology[eb] = { embedded_in: ['root'] };
    return eb;
  });

  const unprocessed_sup_graphs = [];
  const node_bindings = new Set();
  const processed_sup_graphs = new Set();
  // Invariant: edges and subgraphs will only ever be processed once.
  while (!cmn.isArrayEmpty(unprocessed_edge_bindings) || !cmn.isArrayEmpty(unprocessed_sup_graphs)) {
    while (!cmn.isArrayEmpty(unprocessed_edge_bindings)) {
      const eb = unprocessed_edge_bindings.pop();
      if (edge_topology[eb].supporting_graphs !== undefined) continue; // Skip the edge binding if we've processed it already
      const kedge = trapi.getKedge(eb, kgraph);
      if (!kedge) throw new EdgeBindingNotFoundError(eb);
      node_bindings.add(trapi.getSub(kedge));
      node_bindings.add(trapi.getObj(kedge));
      const edge_sup_graphs = trapi.getSupGraphs(kedge);
      edge_topology[eb].supporting_graphs = edge_sup_graphs;
      // TODO: replace this check with a configurable version
      if (!cmn.isArrayEmpty(edge_sup_graphs) && bl.sanitizeBiolinkItem(trapi.getPred(kedge)) === 'related to') {
        // What we are doing here is replacing an edge with the graphs that support it. This is done by:
        const edge_hosts = [...edge_topology[eb].embedded_in];
        // 1. Ensuring the removed edge is not embedded anywhere
        edge_topology[eb].embedded_in= [];
        // 2. Ensuring all children edges of the removed edge are embedded where the removed edge was embedded
        // This has to be done in a roundabout manner via edge_hosts because we haven't processed the support graphs yet
        edge_sup_graphs.forEach((gid) => {
          if (!processed_sup_graphs.has(gid)) {
            unprocessed_sup_graphs.push([gid, edge_hosts]);
          }
        });
      } else {
        edge_sup_graphs.forEach((gid) => {
          if (!processed_sup_graphs.has(gid)) {
            const edge_hosts = [gid]; // The edge host for the edges of this support graph is just the support graph itself
            unprocessed_sup_graphs.push([gid, edge_hosts]);
          }
        });
      }
    };

    while (!cmn.isArrayEmpty(unprocessed_sup_graphs)) {
      const [gid, edge_hosts] = unprocessed_sup_graphs.pop();
      if (processed_sup_graphs.has(gid)) continue;
      const aux_graph = trapi.getAuxGraph(gid, aux_graphs);
      if (!aux_graph) throw new AuxGraphNotFoundError(gid);
      trapi.getAuxGraphEdges(aux_graph).forEach((eb) => {
        if (edge_topology[eb] === undefined) {
          edge_topology[eb] = { embedded_in: edge_hosts };
          unprocessed_edge_bindings.push(eb);
        } else {
          // We do not want to process the same edge twice, but we need to include this
          // graph as a graph where this edge occurs.
          edge_topology[eb].embedded_in.concat(edge_hosts);
        }
      });

      processed_sup_graphs.add(gid);
    }
  }
  return {
    'node_bindings': [...node_bindings],
    'edge_topology': edge_topology
  };
}

function validate_agraph(agraph, kgraph) {
  for (const nb of agraph.node_bindings) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  agraph.edge_bindings = Object.keys(agraph.edge_topology).filter(eb => {
    const kedge = trapi.getKedge(eb, kgraph);
    // TODO: It would be proper to also delete the edge from edge_topology. The implementation is very complicated because of the cascading effect it could have.
    return bl.isBiolinkPred(trapi.getPred(kedge));
  });
  if (cmn.isArrayEmpty(agraph.edge_bindings)) return false;
  return agraph;
}

function make_map_nb_outedges(agraph, kgraph) {
  const map_eb_sopair = make_map_eb_sopair(agraph, kgraph);
  const map_nb_outedges = new Map();
  for (const eb of agraph.edge_bindings) {
    const { sub, obj } = map_eb_sopair.get(eb);
    add_nb_outedge_mapping(map_nb_outedges, sub, make_outedge(eb, obj));
    add_nb_outedge_mapping(map_nb_outedges, obj, make_outedge(eb, sub));
  }
  return map_nb_outedges;

  function make_map_eb_sopair(agraph, kgraph) {
    const map_eb_sopair = new Map();
    for (const eb of agraph.edge_bindings) {
      const kedge = trapi.getKedge(eb, kgraph);
      map_eb_sopair.set(eb, cmn.make_pair('sub', trapi.getSub(kedge),
                                          'obj', trapi.getObj(kedge)));
    }
    return map_eb_sopair;
  }
  function make_outedge(eb, node) {
    return cmn.make_pair('eb',  eb, 'target', node);
  }
  function add_nb_outedge_mapping(map, nb, outedge) {
    let outedges = map.get(nb);
    if (outedges === undefined) {
      outedges = [];
      map.set(nb, outedges);
    }
    outedges.push(outedge)
  }
}
