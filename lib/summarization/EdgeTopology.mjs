'use strict'
export { EdgeTopology }

class EdgeTopology {
  static root_host() { return _CONSTANTS.ROOT_HOST; };

  constructor() {
    this._topology = new Map();
  }

  edge_ids() {
    return [...this._topology.keys()];
  }

  has_edge(edge_id) {
    return this._topology.has(edge_id);
  }

  make_edge(edge_id, hosts = [], support = []) {
    this._set_edge(edge_id, {
      edge_hosts: [...hosts],
      support_graphs: [...support]
    });
  }

  add_hosts(edge_id, hosts) {
    this._hosts(edge_id).push(...hosts);
  }

  clear_hosts(edge_id) {
    return this._hosts(edge_id) = [];
  }

  clone_hosts(edge_id) {
    return [...new Set(this._hosts(edge_id))];
  }

  has_support(edge_id, support_graph) {
    return this._support(edge_id).includes(support_graph);
  }

  set_support(edge_id, support_graphs) {
    this._get_edge(edge_id).support_graphs = [...support_graphs];
  }

  add_support(edge_id, support_graphs) {
    this._support(edge_id).push(...support_graphs);
  }

  clone_support(edge_id) {
    return [...new Set(this._support(edge_id))];
  }

  has_host(edge_id, host_id) {
    return this._hosts(edge_id).includes(host_id);
  }

  find_path_hosts(path_edge_ids) {
    let path_hosts = this._hosts(path_edge_ids[0]);
    for (let i = 1; i < path_edge_ids.length; i++) {
      const edge_hosts = this._hosts(path_edge_ids[i]);
      path_hosts.filter((host) => { return edge_hosts.includes(host); });
    }
    return [...new Set(path_hosts)];
  }

  _get_edge(edge_id) {
    return this._topology.get(edge_id);
  }

  _set_edge(edge_id, edge) {
    return this._topology.set(edge_id, edge);
  }

  _hosts(edge_id) {
    return this._get_edge(edge_id).edge_hosts;
  }

  _support(edge_id) {
    return this._get_edge(edge_id).support_graphs;
  }
}
