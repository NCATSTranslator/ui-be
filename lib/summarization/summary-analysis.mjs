'use strict'
export {
  analysis_to_summary_analysis,
  gen_analysis_paths,
  summary_analysis_to_summary_paths_and_edges
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi/core.mjs';
import * as bl from '../biolink-model.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';

function analysis_to_summary_analysis(analysis, kgraph, aux_graphs) {
  const summary_analysis = new SummaryAnalysis(analysis, kgraph, aux_graphs);
  return _finalize_summary_analysis(summary_analysis, kgraph);
}

function gen_analysis_paths(summary_analysis, kgraph, start, ends, max_path_len) {
  const paths = new Map();
  const constraints = {
    start: start,
    ends: ends,
    max_path_len: max_path_len,
    graph: summary_analysis.root
  };
  _gen_graph_paths(summary_analysis, kgraph, paths, constraints);
  return paths;

  function _gen_graph_paths(summary_analysis, kgraph, paths, constraints) {
    const graph = constraints.graph;
    if (paths.has(graph.id)) return;
    const complete_paths = [];
    paths.set(graph.id, complete_paths);
    const remaining_paths = [[constraints.start]];
    const ends = constraints.ends;
    const map_nb_outedges = _gen_map_nb_outedges(graph.edge_ids(), kgraph);
    while (!cmn.is_array_empty(remaining_paths)) {
      const path = remaining_paths.pop();
      const head = path[path.length-1];
      const outedges = map_nb_outedges.get(head);
      const is_terminal_len = (path.length === constraints.max_path_len-2);
      if (is_terminal_len) {
        for (let i = 0; i < outedges.length; i++) {
          const outedge = outedges[i];
          const is_end_node = ends.includes(outedge.target);
          const is_cycle = path.includes(outedge.target);
          if (is_end_node && !is_cycle) {
            path.push(outedge.eb, outedge.target);
            for (let j = 1; j < path.length; j+=2) {
              const support_gids = summary_analysis.support_graph_ids(path[j]);
              for (let k = 0; k < support_gids.length; k++) {
                const sub_constraints = {
                  start: path[j-1],
                  ends: [path[j+1]],
                  max_path_len: max_path_len,
                  graph: summary_analysis.get_graph(support_gids[k])
                };
                _gen_graph_paths(summary_analysis, kgraph, paths, sub_constraints);
              }
            }
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
              const complete_path = [...path, outedge.eb, outedge.target];
              for (let j = 1; j < complete_path.length; j+=2) {
                const start = complete_path[j-1];
                const end = complete_path[j+1];
                const support_gids = summary_analysis.support_graph_ids(complete_path[j]);
                for (let k = 0; k < support_gids.length; k++) {
                  const sub_constraints = {
                    start: complete_path[j-1],
                    ends: [complete_path[j+1]],
                    max_path_len: max_path_len,
                    graph: summary_analysis.get_graph(support_gids[k])
                  };
                  _gen_graph_paths(summary_analysis, kgraph, paths, sub_constraints);
                }
              }
              complete_paths.push([...path, outedge.eb, outedge.target]);
            }
            remaining_paths.push([...path, outedge.eb, outedge.target]);
          }
        }
      }
    }
  }
}

function summary_analysis_to_summary_paths_and_edges(summary_analysis, analysis_paths, kgraph) {
  const summary_paths = {};
  const edge_metadata = new Map();
  for (const [gid, paths] of analysis_paths.entries()) {
    for (const path of paths) {
      const summary_path = [];
      for (let i = 0; i < (path.length -1); i+=2) {
        const nb = path[i];
        const eb = path[i+1];
        const is_edge_inverted = pred.isPredInverted(eb, nb, kgraph);
        const is_edge_root = gid === SummaryAnalysis.ROOT;
        const eid = id.gen_eid(eb, kgraph, is_edge_inverted, is_edge_root);
        if (!edge_metadata.has(eid)) {
          edge_metadata.set(eid, {
            edge_bindings: [],
            inverted_id: null,
            is_root: is_edge_root
          });
        }
        edge_metadata.get(eid).edge_bindings.push(eb);
        if (is_edge_inverted) {
          edge_metadata.get(eid).inverted_id = id.gen_eid(eb, kgraph, false, is_edge_root);
        }
        const nid = id.gen_nid(nb, kgraph);
        summary_path.push(nid, eid);
      }
      summary_path.push(id.gen_nid(path[path.length-1]));
      if (!summary_paths[gid]) {
        summary_paths[gid] = [];
      }
      summary_paths[gid].push([...summary_path]);
    }
  }
  const summary_edges = {};
  for (const [gid, paths] of Object.entries(summary_paths)) {
    for (const path of paths) {
      for (let i = 1; i < path.length; i+=2) {
        const eid = path[i];
        if (summary_edges[eid] === undefined) {
          summary_edges[eid] = new SummaryEdge();
        }
        const summary_edge = summary_edges[eid];
        const metadata = edge_metadata.get(eid);
        metadata.edge_bindings = cmn.distinctArray(metadata.edge_bindings);
        for (const eb of metadata.edge_bindings) {
          for (const gid of summary_analysis.support_graph_ids(eb)) {
            summary_edge.extendSupPaths(summary_paths[gid].map(id.gen_pid));
          }
          summary_edge.isRootPath = metadata.is_root;
          summary_edge.type = summary_edge.hasSupport() ? trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT : trapi.CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
          summary_edge.metadata = metadata;
        }
      }
    }
  }
  return [Object.values(summary_paths).flat(), summary_edges];
}

function _finalize_summary_analysis(summary_analysis, kgraph) {
  for (const nb of summary_analysis.node_ids) {
    if (!trapi.has_knode(nb, kgraph)) return false;
  }
  summary_analysis = _trim_summary_analysis_edges(summary_analysis, ((eid) => {
    const kedge = trapi.get_kedge(eid, kgraph);
    return !bl.isBiolinkPred(trapi.get_predicate(kedge));
  }));
  if (summary_analysis.edge_count() === 0) return false;
  _collapse_edges(summary_analysis, ((eid) => {
    const kedge = trapi.get_kedge(eid, kgraph);
    const predicate = trapi.get_predicate(kedge);
    return predicate === 'biolink:related_to';
  }));
  return summary_analysis;
}

class SummaryAnalysis {
  static ROOT = 'root';
  constructor(analysis, kgraph, aux_graphs) {
    let root_bindings = this._abinds_to_kgbinds(trapi.get_edge_bindings(analysis));
    if (cmn.is_array_empty(root_bindings)) {
      root_bindings = this._abinds_to_kgbinds(trapi.get_path_bindings(analysis));
      root_bindings = trapi.get_auxiliary_graph_edges(trapi.get_auxiliary_graph(root_bindings[0], aux_graphs));
    }
    this.root = new GraphNode(SummaryAnalysis.ROOT, root_bindings);
    this.node_ids = null;
    this._graphs = new Map();
    this._edges = new Map();
    this._add_graph_unsafe(this.root);
    const unprocessed_edges = [];
    for (const eid of this.root.edge_ids()) {
      unprocessed_edges.push(new EdgeNode(eid));
    }
    const unprocessed_graphs = [];
    const node_bindings = new Set();
    // Edges and graphs will only ever be processed once
    while (!cmn.is_array_empty(unprocessed_graphs) || !cmn.is_array_empty(unprocessed_edges)) {
      while (!cmn.is_array_empty(unprocessed_edges)) {
        const edge = unprocessed_edges.pop();
        const kedge = trapi.get_kedge(edge.id, kgraph);
        if (!kedge) throw new trapi.EdgeBindingNotFoundError(edge.id);
        node_bindings.add(trapi.get_subject(kedge));
        node_bindings.add(trapi.get_object(kedge));
        const edge_sup_graphs = trapi.get_support_graphs(kedge);
        edge.set_graph_ids(edge_sup_graphs);
        for (const gid of edge.graph_ids()) {
          if (!this._has_graph(gid)) {
            unprocessed_graphs.push(new GraphNode(gid));
          }
        };
        this._add_edge_unsafe(edge);
      }
      while (!cmn.is_array_empty(unprocessed_graphs)) {
        const graph = unprocessed_graphs.pop();
        const aux_graph = trapi.get_auxiliary_graph(graph.id, aux_graphs);
        if (!aux_graph) throw new trapi.AuxGraphNotFoundError(graph.id);
        const aux_ebs = trapi.get_auxiliary_graph_edges(aux_graph);
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
  edge_count() { return this._edges.size }
  edge_ids() { return this._edges.keys() }
  remove_edge_unsafe(eid) {
    this._edges.delete(eid);
    return this;
  }
  support_graph_ids(eid) { return this._edges.get(eid).graph_ids(); }
  graph_edge_ids(gid) { return this._graphs.get(gid).edge_ids(); }
  get_graph(gid) { return this._graphs.get(gid); }
  collapse_edge(host_id, eid) {
    const host_graph = this._graphs.get(host_id);
    const edges_moved = [];
    const support_graph_ids = this.support_graph_ids(eid);
    if (cmn.is_array_empty(support_graph_ids)) return false;
    for (const gid of support_graph_ids) {
      const graph = this._graphs.get(gid);
      edges_moved.push(...graph.edge_ids());
    }
    host_graph.extend_edge_ids(edges_moved);
    host_graph.remove_edge_id(eid);
    return edges_moved;
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

function _gen_map_nb_outedges(ebs, kgraph) {
  const map_nb_outedges = new Map();
  for (const eb of ebs) {
    const kedge = trapi.get_kedge(eb, kgraph);
    const subject = trapi.get_subject(kedge);
    const object = trapi.get_object(kedge);
    __add_nb_outedge_mapping(map_nb_outedges, subject, __make_outedge(eb, object));
    __add_nb_outedge_mapping(map_nb_outedges, object, __make_outedge(eb, subject));
  }
  return map_nb_outedges;

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

function _trim_summary_analysis_edges(summary_analysis, do_trim) {
  for (const eid of summary_analysis.edge_ids()) {
    if (do_trim(eid)) {
      summary_analysis.remove_edge_unsafe(eid);
    }
  }
  return summary_analysis;
}

function _collapse_edges(summary_analysis, do_collapse) {
  const graphs_left = [summary_analysis.root.id];
  const graphs_seen = new Set();
  while (!cmn.is_array_empty(graphs_left)) {
    const host_id = graphs_left.pop();
    if (graphs_seen.has(host_id)) continue;
    graphs_seen.add(host_id);
    const edges_left = summary_analysis.graph_edge_ids(host_id);
    const edges_seen = new Set();
    while (!cmn.is_array_empty(edges_left)) {
      const eid = edges_left.pop();
      if (edges_seen.has(eid)) continue;
      edges_seen.add(eid);
      if (do_collapse(eid)) {
        const edges_moved = summary_analysis.collapse_edge(host_id, eid);
        if (edges_moved) {
          edges_left.push(...edges_moved);
        }
      } else {
        graphs_left.push(...summary_analysis.support_graph_ids(eid));
      }
    }
  }
}

class GraphNode {
  constructor(id, edge_ids = []) {
    this.id = id;
    this._edge_ids = edge_ids;
  }

  edge_ids() { return [...this._edge_ids]; }
  set_edge_ids(edge_ids) { this._edge_ids = [...edge_ids]; }
  extend_edge_ids(edge_ids) { this._edge_ids.push(...edge_ids); }
  remove_edge_id(edge_id) { this._edge_ids = this._edge_ids.filter(eid => eid !== edge_id); }
}

class EdgeNode {
  constructor(id) {
    this.id = id;
    this._graph_ids = [];
  }

  graph_ids() { return [...this._graph_ids]; }
  set_graph_ids(graph_ids) { this._graph_ids = [...graph_ids]; }
}

