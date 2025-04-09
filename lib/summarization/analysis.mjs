'use strict'
export {
  analysis_to_agraph,
  EdgeBindingNotFoundError
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';

function analysis_to_agraph(analysis, kgraph, aux_graphs) {
  const raw_graph = deconstruct_analysis(analysis, kgraph, aux_graphs);
  return gen_rgraph(
    raw_graph.node_bindings,
    raw_graph.edge_topology,
    kgraph
  );
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
function map_abinds_kgbinds(abinds) {
  return Object.values(abinds).reduce((kgbinds, ab) => {
    return kgbinds.concat(ab.map(map_ab_kgb => {
      return cmn.jsonGet(map_ab_kgb, 'id');
    }));
  },
  []);
}

function deconstruct_analysis(analysis, kgraph, aux_graphs) {
  const edge_topology = new Map(); // This doubles as the set of processed edge bindings
  const unprocessed_edge_bindings = map_abinds_kgbinds(trapi.getEdgeBindings(analysis)).map((eb) => {
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

function gen_rgraph(node_bindings, edge_topology, kgraph) {
  for (const nb of node_bindings) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  const rgraph = {};
  rgraph.nodes = node_bindings;
  const edge_bindings = Object.keys(edge_topology);
  if (!edge_bindings) return false;
  rgraph.edges = edge_bindings.filter(eb => {
    const kedge = trapi.getKedge(eb, kgraph);
    // TODO: It would be proper to also delete the edge from edge_topology. The implementation is very complicated because of the cascading effect it could have.
    return bl.isBiolinkPred(trapi.getPred(kedge));
  });
  rgraph.edge_topology = edge_topology;
  return rgraph;
}

class EdgeBindingNotFoundError extends Error {
  constructor(edgeBinding) {
    super(`Edge binding not found for ${JSON.stringify(edgeBinding)}`);
  }
}
