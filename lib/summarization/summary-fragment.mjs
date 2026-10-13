'use strict';
export {
  answers_to_summary_fragments,
  answer_to_fragments,
  result_to_fragment,
  analysis_to_fragment,
  gen_knode_transforms,
  gen_kedge_transforms,
  make_error_fragment,
  SummaryFragment,
  SummaryFragmentEdges,
  MissingInforesError,
  NodeBindingNotFoundError,
  _make_fragment_config,
  _get_result_endpoints,
  _flatten_binding,
  _get_result_scoring_components,
  _make_knode_transform,
  _make_kedge_transform,
  _get_knowledge_level
}

import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';
import * as trapi from '../trapi/core.mjs';
import * as sa from './summary-analysis.mjs';
import * as id from './identifiers.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';

function answers_to_summary_fragments(answers, knode_rules, kedge_rules, feature_config) {
  const config = _make_fragment_config(knode_rules, kedge_rules, feature_config);
  const errors = {};
  const fragments = answers.flatMap(answer => answer_to_fragments(answer, config, errors));
  return [fragments, errors];
}

function answer_to_fragments(answer, config, errors) {
  const results = trapi.get_results(answer);
  if (!results) return [];

  const kgraph = trapi.get_kgraph(answer);
  const aux_graphs = trapi.get_auxiliary_graphs(answer);
  const [start_key, end_key] = trapi.message_to_endpoints(answer);
  const eid_cache = new Map();
  const fragments = [];
  for (const result of results) {
    const fragment = result_to_fragment(
      result, kgraph, aux_graphs, start_key, end_key, config, errors, eid_cache);
    if (!fragment.is_empty()) {
      fragments.push(fragment);
    }
  }
  return fragments;
}

function result_to_fragment(result, kgraph, aux_graphs, start_key, end_key, config, errors, eid_cache) {
  try {
    const [start, ends] = _get_result_endpoints(result, start_key, end_key, kgraph);
    const analyses = trapi.get_analyses(result);
    const fragment = analyses.reduce(
      (merged, analysis) => {
        return merged.merge(analysis_to_fragment(
          analysis, kgraph, aux_graphs, start, ends, config, errors, eid_cache));
      },
      new SummaryFragment());

    if (!fragment.is_empty()) {
      const rid = id.gen_nid(start, kgraph);
      fragment.push_score(rid, _get_result_scoring_components(result));
    }

    return fragment;
  } catch (err) {
    console.error(err);
    if (err instanceof NodeBindingNotFoundError) {
      return make_error_fragment('unknown', err.message);
    }

    return make_error_fragment('unknown', 'Unknown error while building result summary fragment');
  }
}

function analysis_to_fragment(analysis, kgraph, aux_graphs, start, ends, config, errors, eid_cache) {
  const agent = trapi.get_resource_id(analysis);
  if (!agent) {
    return make_error_fragment('unknown', 'Expected analysis to have resource_id');
  }

  try {
    const summary_analysis = sa.analysis_to_summary_analysis(analysis, kgraph, aux_graphs);
    if (!summary_analysis) {
      return make_error_fragment(agent, 'Summary analysis construction failed');
    }
    const analysis_paths = sa.gen_analysis_paths(summary_analysis, kgraph, start, ends, config.max_path_len);
    const [summary_paths, summary_edges] = sa.summary_analysis_to_summary_paths_and_edges(
      summary_analysis, analysis_paths, kgraph, config.replace_indirect_edges, eid_cache);
    const analysis_context = {
      agent: agent,
      errors: errors
    };
    const knode_transforms = gen_knode_transforms(
      summary_analysis.node_ids, kgraph, config.knode_rules, analysis_context);
    const kedge_transforms = gen_kedge_transforms(
      summary_edges, kgraph, config.kedge_rules, analysis_context);
    return new SummaryFragment(
      [agent],
      summary_paths,
      knode_transforms,
      new SummaryFragmentEdges(summary_edges, kedge_transforms)
    );
  } catch (err) {
    console.error(err);
    if (err instanceof trapi.EdgeBindingNotFoundError
        || err instanceof sa.MissingSupportPathsError) {
      return make_error_fragment(agent, err.message);
    }

    return make_error_fragment(agent, 'Unknown error while building Analysis Graph');
  }
}

function gen_knode_transforms(node_ids, kgraph, knode_rules, context) {
  return node_ids.map(nid => {
    const knode = trapi.get_knode(nid, kgraph);
    return _make_knode_transform(nid, knode, knode_rules, context);
  });
}

function gen_kedge_transforms(summary_edges, kgraph, kedge_rules, context) {
  const kedge_transforms = [];
  for (const [eid, edge] of Object.entries(summary_edges)) {
    for (const eb of edge.metadata.edge_bindings) {
      const kedge = trapi.get_kedge(eb, kgraph);
      kedge_transforms.push(_make_kedge_transform(eid, kedge, kedge_rules, context));
    }
  }
  return kedge_transforms;
}

function make_error_fragment(agent, error) {
  const fragment = new SummaryFragment();
  fragment.push_agent(agent);
  fragment.push_error(agent, error);
  return fragment;
}

class SummaryFragment {
  constructor(agents, paths, nodes, edges, scores, errors) {
    this.agents = agents || [];
    this.paths = paths || [];
    this.nodes = nodes || [];
    this.edges = edges || new SummaryFragmentEdges();
    this.scores = scores || {};
    this.errors = errors || {};
  }

  is_empty() {
    return cmn.is_array_empty(this.paths)
      && cmn.is_array_empty(this.nodes)
      && this.edges.is_empty();
  }

  push_score(rid, scoring_components) {
    const result_scores = cmn.json_set_default_and_get(this.scores, rid, []);
    result_scores.push(scoring_components);
  }

  push_agent(agent) {
    this.agents.push(agent);
  }

  push_error(agent, error) {
    const agent_errors = cmn.json_set_default_and_get(this.errors, agent, []);
    agent_errors.push(error);
  }

  merge(fragment) {
    this.agents.push(...fragment.agents);
    this.paths.push(...fragment.paths);
    this.nodes.push(...fragment.nodes);
    this.edges.merge(fragment.edges);
    this._merge_keyed_lists(this.scores, fragment.scores);
    this._merge_keyed_lists(this.errors, fragment.errors);
    return this;
  }

  _merge_keyed_lists(target, source) {
    Object.keys(source).forEach((k) => {
      const current = cmn.json_set_default_and_get(target, k, []);
      current.push(...source[k]);
    });
  }
}

class SummaryFragmentEdges {
  constructor(base, updates) {
    this.base = base || {};
    this.updates = updates || [];
  }

  is_empty() {
    return cmn.is_object_empty(this.base) && cmn.is_array_empty(this.updates);
  }

  merge(fragment_edges) {
    Object.keys(fragment_edges.base).forEach((eid) => {
      const current_edge = cmn.json_set_default_and_get(this.base, eid, new SummaryEdge(eid));
      current_edge.merge(fragment_edges.base[eid]);
    });

    this.updates.push(...fragment_edges.updates);
    return this;
  }
}

class MissingInforesError extends Error {
  constructor(infores) {
    super(`Missing infores: ${infores}`);
    this.infores = infores;
  }
}

class NodeBindingNotFoundError extends Error {
  constructor(node_binding) {
    super(`Node binding not found for ${JSON.stringify(node_binding)}`);
  }
}

function _make_fragment_config(knode_rules, kedge_rules, feature_config) {
  return {
    knode_rules: knode_rules,
    kedge_rules: kedge_rules,
    max_path_len: (2 * feature_config.max_hops) + 1,
    replace_indirect_edges: feature_config.replace_indirect_edges
  };
}

function _get_result_endpoints(result, start_key, end_key, kgraph) {
  const start = _flatten_binding(result, start_key, kgraph)[0];
  const ends = _flatten_binding(result, end_key, kgraph);
  return [start, ends];
}

function _flatten_binding(result, key, kgraph) {
  const node_bindings = trapi.get_node_bindings(result, key);
  if (!cmn.is_array(node_bindings) || cmn.is_array_empty(node_bindings)) {
    throw new NodeBindingNotFoundError(key);
  }
  return node_bindings.map((entry) => {
    const endpoint = trapi.get_binding_id(entry);
    if (!trapi.has_knode(endpoint, kgraph)) {
      throw new NodeBindingNotFoundError(endpoint);
    }
    return endpoint;
  });
}

function _get_result_scoring_components(result) {
  const scoring_components = trapi.get_ordering_components(result);
  const normalized_score = trapi.get_normalized_score(result);
  cmn.json_set(scoring_components, 'normalized_score', normalized_score);
  return scoring_components;
}

function _make_knode_transform(nid, knode, knode_rules, context) {
  return {
    id: nid,
    transforms: knode_rules(knode, context)
  };
}

function _make_kedge_transform(eid, kedge, kedge_rules, context) {
  const edge_context = cmn.deep_copy(context);
  const source = trapi.get_primary_source(kedge);
  const provenance = bl.infores_to_provenance(source.infores);
  if (cmn.is_missing(provenance)) throw new MissingInforesError(source.infores);
  edge_context.infores = source.infores;
  edge_context.knowledge_level = _get_knowledge_level(kedge, provenance);
  return {
    id: eid,
    transforms: kedge_rules(kedge, edge_context)
  };
}

function _get_knowledge_level(kedge, provenance) {
  const agent_type = trapi.get_agent_type(kedge);
  if (agent_type === 'text_mining_agent') {
    return 'ml';
  }

  const kl = trapi.get_knowledge_level(kedge);
  if (kl === 'knowledge_assertion') {
    return 'trusted';
  } else if (kl === 'not_provided') {
    return 'unknown';
  } else if (kl !== null) {
    return 'inferred';
  }

  return provenance.knowledge_level;
}
