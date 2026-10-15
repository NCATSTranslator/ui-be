export {
  generate_results,
  gen_metapath
}

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as trapi from '../trapi/core.mjs';
import * as taglib from '../taglib.mjs';
import * as id from './identifiers.mjs';
import * as summary_tags from './tags.mjs';

function generate_results(results, paths, nodes, edges, scores, errors, query_type) {
  for (const result of results) {
    result.paths = result.paths.filter(pid => paths[pid] !== undefined);
  }
  _prune_unreferenced(results, paths, edges, nodes);

  const used_tags = {};
  const expanded_results = {};
  for (const result of results) {
    const pids = result.paths;
    const root_pids = pids.filter(pid => _is_root_path(_get_summary_path(pid, paths), edges));
    if (cmn.is_array_empty(root_pids)) {
      _record_no_root_paths(pids, paths, errors);
      continue;
    }

    const summary_path = _get_summary_path(root_pids[0], paths);
    const name = _gen_result_name(summary_path, nodes, query_type);
    const start = summary_path.start;
    const end = summary_path.end;
    const tags = _collect_result_tags(pids, paths, start, used_tags);
    const result_id = _gen_result_id(summary_path, nodes, query_type);
    const expanded_result = expanded_results[result_id];
    if (cmn.is_missing(expanded_result)) {
      expanded_results[result_id] = {
        id: result_id,
        subject: start,
        drug_name: name,
        paths: root_pids,
        object: end,
        scores: scores[start],
        tags: tags
      };
    } else {
      expanded_result.paths.push(...root_pids);
      Object.assign(expanded_result.tags, tags);
    }
  }

  const path_compare = _query_type_to_path_compare(query_type);
  for (const result of Object.values(expanded_results)) {
    _pid_sort(result.paths, paths, path_compare);
  }

  return {
    results: Object.values(expanded_results),
    tags: used_tags,
    paths: paths,
    nodes: nodes,
    edges: edges,
    errors: errors
  };
}

function gen_metapath(subgraph, nodes) {
  const metapath = [];
  for (let i = 0; i < subgraph.length; i+=2) {
    metapath.push(nodes[subgraph[i]].get_specific_type());
  }
  return metapath;
}

function _get_summary_path(pid, paths) {
  return cmn.json_get(paths, pid);
}

function _prune_unreferenced(results, paths, edges, nodes) {
  const seen_paths = new Set();
  const seen_edges = new Set();
  const seen_nodes = new Set();
  for (const result of results) {
    const result_paths = [];
    for (const pid of result.paths) {
      const path = _get_summary_path(pid, paths);
      if (path.length !== 0) {
        result_paths.push(pid);
        seen_paths.add(pid);
      }
      path.forEids(eid => seen_edges.add(eid));
      path.forNids(nid => seen_nodes.add(nid));
    }
    result.paths = result_paths;
  }

  _delete_unseen(paths, seen_paths);
  _delete_unseen(edges, seen_edges);
  _delete_unseen(nodes, seen_nodes);
}

function _delete_unseen(section, seen_ids) {
  for (const key of Object.keys(section)) {
    if (!seen_ids.has(key)) {
      delete section[key];
    }
  }
}

function _is_root_path(path, edges) {
  let is_root = true;
  path.forEids((eid) => {
    is_root = is_root && edges[eid].is_root;
  });
  return is_root;
}

function _record_no_root_paths(pids, paths, errors) {
  const agents = new Set();
  for (const pid of pids) {
    for (const agent of paths[pid].aras) {
      agents.add(agent);
    }
  }
  const error_message = "No root paths found";
  logger.warn(`${[...agents].join(', ')}: ${error_message}`);
  for (const agent of agents) {
    const agent_errors = cmn.json_set_default_and_get(errors, agent, []);
    agent_errors.push(error_message);
  }
}

function _collect_result_tags(pids, paths, start, used_tags) {
  const tags = {};
  for (const pid of pids) {
    const path = _get_summary_path(pid, paths);
    for (const tag of taglib.get_tags(path)) {
      if (summary_tags.is_result_tag(tag) && path.start !== start) continue;
      used_tags[tag.id] = tag.description;
      tags[tag.id] = null;
    }
  }
  return tags;
}

function _gen_result_name(summary_path, nodes, query_type) {
  if (trapi.is_pathfinder_query(query_type)) {
    const metapath = gen_metapath(summary_path.subgraph, nodes);
    metapath[0] = nodes[summary_path.start].name();
    metapath[metapath.length-1] = nodes[summary_path.end].name();
    return metapath.join('/');
  }
  return nodes[summary_path.start].name();
}

function _gen_result_id(summary_path, nodes, query_type) {
  if (trapi.is_pathfinder_query(query_type)) {
    return id.gen_pid(gen_metapath(summary_path.subgraph, nodes));
  }
  return id.gen_pid([summary_path.start, summary_path.end]);
}

function _query_type_to_path_compare(query_type) {
  if (trapi.is_pathfinder_query(query_type)) return _path_score_compare;
  return _path_alpha_len_compare;
}

function _path_alpha_len_compare(summary_path_a, summary_path_b) {
  const len_a = summary_path_a.length;
  const len_b = summary_path_b.length;
  if (len_a === len_b) {
    for (let i = 0; i < summary_path_a.nodeCount; i++) {
      if (summary_path_a.nid(i) < summary_path_b.nid(i)) return -1;
      if (summary_path_a.nid(i) > summary_path_b.nid(i)) return 1;
    }
    return 0;
  }
  if (len_a < len_b) return -1;
  return 1;
}

function _path_score_compare(summary_path_a, summary_path_b) {
  if (summary_path_a.score >= summary_path_b.score) return -1;
  return 1;
}

function _pid_sort(pids, paths, path_compare) {
  if (pids.length < 2) return pids;
  return pids.sort((pid_a, pid_b) => {
    const comparison = path_compare(_get_summary_path(pid_a, paths), _get_summary_path(pid_b, paths));
    if (comparison === 0) {
      if (pid_a < pid_b) return -1;
      if (pid_b < pid_a) return 1;
      return 0;
    }
    return comparison;
  });
}
