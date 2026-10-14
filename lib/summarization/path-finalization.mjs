export {
  finalize_paths
}

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';
import * as bta from '../biothings-annotation.mjs';
import * as trapi from '../trapi/core.mjs';
import * as taglib from '../taglib.mjs';
import * as summary_tags from './tags.mjs';
import { MissingInforesError } from './summary-fragment.mjs';

function finalize_paths(paths, nodes, edges, query_type) {
  for (const pid of Object.keys(paths)) {
    const path = paths[pid];
    if (_has_dangling_reference(pid, path, nodes, edges)) {
      delete paths[pid];
      continue;
    }
    _decorate_chemical_start(path, nodes, query_type);
  }

  for (const path of Object.values(paths)) {
    _tag_path(path, nodes, edges, query_type);
  }
  return paths;
}

function _has_dangling_reference(pid, path, nodes, edges) {
  let has_dangling_ref = false;
  path.forNids((nid) => {
    if (nodes[nid] === undefined) {
      logger.warn(`${nid} not found in path ${pid}`);
      has_dangling_ref = true;
    }
  });
  path.forEids((eid) => {
    if (edges[eid] === undefined) {
      logger.warn(`${eid} not found in path ${pid}`);
      has_dangling_ref = true;
    }
  });
  return has_dangling_ref;
}

function _decorate_chemical_start(path, nodes, query_type) {
  const path_start = nodes[path.start];
  if (!bta.is_chemical(path_start)) return;
  summary_tags.decorate_clinical_tag(path_start);
  summary_tags.decorate_role_tags(path_start);
  summary_tags.decorate_otc_tag(path_start);
  if (trapi.is_chemical_disease_query(query_type)) {
    summary_tags.decorate_indication_tag(path, nodes);
  }
}

function _tag_path(path, nodes, edges, query_type) {
  path.forEids((eid) => {
    summary_tags.promote_edge_tags_to_path(edges[eid], path);
  });
  path.removeDuplicates();
  summary_tags.promote_start_node_tags_to_path(nodes[path.start], path);
  path.forInternalNids((nid) => {
    const node = nodes[nid];
    summary_tags.promote_internal_node_tags_to_path(node, path);
    taglib.set_tag(path, summary_tags.make_node_type_tag(node.get_specific_type()));
  });
  summary_tags.decorate_max_phase_tag(path, nodes, query_type);
  for (const infores of path.agents) {
    const provenance = bl.infores_to_provenance(infores);
    if (cmn.is_missing(provenance)) throw new MissingInforesError(infores);
    taglib.set_tag(path, summary_tags.make_ara_tag(infores, provenance.name));
  }
  taglib.set_tag(path, summary_tags.make_path_length_tag(path.edgeCount));
  path.tags = taglib.tags_to_raw_obj(path);
}
