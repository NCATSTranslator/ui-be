'use strict'
export {
  analysis_to_agraph
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { EdgeTopology } from './EdgeTopology.mjs';

function analysis_to_agraph(analysis, kgraph, aux_graphs) {
  const agraph = _deconstruct_analysis(analysis, kgraph, aux_graphs);
  return _validate_agraph(agraph, kgraph);
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
    edge_topology.make_edge(eb, [EdgeTopology.root_host()]);
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
    node_bindings: [...node_bindings],
    edge_topology: edge_topology,
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
  agraph.nb_to_outedges = _make_fn_nb_to_outedges(agraph, kgraph);
  return agraph;
}

function _make_fn_nb_to_outedges(agraph, kgraph) {
  const map_eb_sopair = __make_map_eb_sopair(agraph, kgraph);
  const map_nb_outedges = new Map();
  for (const eb of agraph.edge_bindings) {
    const { sub, obj } = map_eb_sopair.get(eb);
    __add_nb_outedge_mapping(map_nb_outedges, sub, __make_outedge(eb, obj));
    __add_nb_outedge_mapping(map_nb_outedges, obj, __make_outedge(eb, sub));
  }
  return (node_binding) => { return map_nb_outedges.get(node_binding); };

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
