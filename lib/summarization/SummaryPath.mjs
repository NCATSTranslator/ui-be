export { SummaryPath }

import * as id from "./identifiers.mjs";

class SummaryPath {
  constructor(subgraph) {
    this.id = id.gen_pid(subgraph);
    this.score = 0.0;
    this.subgraph = subgraph;
    this.aras = [];
    this.tags = {};
  }

  extendAgents(agents) {
    this.aras.push(...agents);
  }

  get agents() { return this.aras; }
  get length() { return this.subgraph.length; }
  get nodeCount() {
    if (this.subgraph.length === 0) return 0;
    return Math.floor(this.length/2)+1;
  }

  get edgeCount() { return (this.length-1)/2; }
  get graph() { return this.subgraph; }
  get start() { return this.subgraph[0]; }
  get end() { return this.subgraph[this.length-1]; }

  nid(i) { return this.subgraph[i*2]; }
  forNids(func) { this._forIds(func, 0, this.length); }
  forInternalNids(func) { this._forIds(func, 2, this.length-1); }
  eid(i) { return this.subgraph[(i*2)+1]; }
  forEids(func) { this._forIds(func, 1, this.length); }

  _forIds(func, start, end) {
    for (let i = start; i < end; i+=2) {
      func(this.subgraph[i]);
    }
  }
}
