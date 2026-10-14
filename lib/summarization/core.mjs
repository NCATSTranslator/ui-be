'use strict';

export {
  answers_to_summary,
  _merge_fragments
}

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';
import * as trapi from '../trapi/core.mjs';
import * as summary_rules from './property-rules.mjs';
import * as fraglib from './summary-fragment.mjs';
import * as edgelib from './edge-finalization.mjs';
import * as nodelib from './node-finalization.mjs';
import * as pathlib from './path-finalization.mjs';
import * as reslib from './result-generation.mjs';
import * as id from './identifiers.mjs';
import * as taglib from '../taglib.mjs';
import { SummaryNode } from './SummaryNode.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';
import { Summary, SummaryMetadata } from './Summary.mjs';

/**
 * Responsible for converting a set of TRAPI answers into a summarized form that the FE application can use.
 *
 * @param {string}   qid - The query ID for the given answer set.
 * @param {object[]} answers - The set of TRAPI answers to summarize.
 * @param {number}   max_hops - The maximum number of hops to consider when summarizing the answers.
 *
 * @returns {object} - The summarized form of the answers.
 */
function answers_to_summary(qid, answers, feature_config) {
  if (!cmn.is_array(answers) || cmn.is_array_empty(answers)) {
    logger.warn(`No results for query with PK ${qid}`);
    const default_metadata = new SummaryMetadata(qid, []);
    return new Summary(default_metadata);
  }

  const knode_summarization_rules = summary_rules.make_knode_rules();
  const kedge_summarization_rules = summary_rules.make_kedge_rules();

  const query_type = _answers_to_query_type(answers);
  const [summary_fragments, errors] = fraglib.answers_to_summary_fragments(answers,
      knode_summarization_rules, kedge_summarization_rules, feature_config);
  return _summary_fragments_to_summary(qid, summary_fragments, query_type, errors);
}

/*
 * Determine the query template type based on a set of answers.
 *
 * @param {object} answers - The answers to determine the query type from.
 *
 * @returns {number} - The query type.
 */
function _answers_to_query_type(answers) {
  // TODO: A more robust solution might be possible but all answers should have the same query type.
  return trapi.message_to_query_type(answers[0]); // This assumes a fully merged TRAPI message
}

function _merge_fragments(fragments, query_type, errors) {
  const nodes = {};
  const edges = {};
  const scores = {};
  for (const fragment of fragments) {
    _merge_graph_elements(nodes, fragment.nodes, fragment.agents, (nid) => new SummaryNode(nid));
    _merge_edges(edges, fragment.edges, fragment.agents);
    _merge_keyed_lists(scores, fragment.scores);
    _merge_keyed_lists(errors, fragment.errors);
  }

  for (const node of Object.values(nodes)) {
    node.types.sort(bl.biolink_class_cmp_fn);
  }

  const results = {};
  const paths = {};
  for (const fragment of fragments) {
    _merge_results(results, _paths_to_results(fragment.paths, nodes, query_type));
    _merge_paths(paths, fragment.paths, fragment.agents);
  }

  return {
    results: Object.values(results).map(cmn.obj_remove_duplicates),
    paths: paths,
    nodes: nodes,
    edges: edges,
    scores: scores,
    errors: errors
  };
}

function _merge_graph_elements(elements, updates, agents, make_default) {
  for (const update of updates) {
    const element = cmn.json_set_default_and_get(elements, update.id, make_default(update.id));
    for (const transform of update.transforms) {
      transform(element);
      element.aras.push(...agents);
    }
  }
}

function _merge_edges(edges, fragment_edges, agents) {
  for (const [eid, base] of Object.entries(fragment_edges.base)) {
    const edge = cmn.json_set_default_and_get(edges, eid, new SummaryEdge(eid));
    edge.merge(base);
  }
  _merge_graph_elements(edges, fragment_edges.updates, agents, (eid) => new SummaryEdge(eid));
}

function _merge_keyed_lists(target, source) {
  for (const [key, values] of Object.entries(source)) {
    const current = cmn.json_set_default_and_get(target, key, []);
    current.push(...values);
  }
}

function _paths_to_results(paths, nodes, query_type) {
  return paths.map((path) => {
    const rid = trapi.is_pathfinder_query(query_type)
      ? id.gen_pid(reslib.gen_metapath(path.subgraph, nodes))
      : path.start;
    return cmn.make_pair('start', rid, 'pid', path.id);
  });
}

function _merge_results(results, new_results) {
  for (const result of new_results) {
    const existing = cmn.json_set_default_and_get(results, result.start, {});
    const result_paths = cmn.json_set_default_and_get(existing, 'paths', []);
    result_paths.push(result.pid);
  }
}

function _merge_paths(paths, new_paths, agents) {
  for (const path of new_paths) {
    const existing = cmn.json_get(paths, path.id, false);
    if (existing) {
      existing.extendAgents(agents);
      continue;
    }
    path.extendAgents(agents);
    cmn.json_set(paths, path.id, path);
  }
}

function _summary_fragments_to_summary(qid, summary_fragments, query_type, errors) {
  const { results, paths, nodes, edges, scores } = _merge_fragments(summary_fragments, query_type, errors);

  const { publications: pubs, trials, provenance } = edgelib.finalize_edges(edges, errors);

  const meta = new SummaryMetadata(qid, cmn.distinct_array(summary_fragments.flatMap(fragment => fragment.agents)));

  nodelib.finalize_nodes(nodes);

  pathlib.finalize_paths(paths, nodes, edges, query_type);

  Object.keys(edges).forEach((eid) => {
    const edge = cmn.json_get(edges, eid);
    edge.tags = taglib.tags_to_raw_obj(edge);
  });

  const generated = reslib.generate_results(results, paths, nodes, edges, scores, errors, query_type);

  Object.values(nodes).forEach((node) => {
    node.tags = taglib.tags_to_raw_obj(node);
  });

  return new Summary(meta, generated.results, paths, nodes, edges, pubs, trials, provenance, generated.tags, errors);
}
