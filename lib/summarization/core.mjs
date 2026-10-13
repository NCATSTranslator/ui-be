'use strict';

export { answers_to_summary }

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';
import * as bta from '../biothings-annotation.mjs';
import * as trapi from '../trapi/core.mjs';
import * as summary_rules from './property-rules.mjs';
import * as fraglib from './summary-fragment.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { SummaryNode } from './SummaryNode.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';
import * as taglib from '../taglib.mjs';
import * as summary_tags from './tags.mjs';

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
  const summary = _summary_fragments_to_summary(qid, summary_fragments,
      _get_answers_kgraph(answers), query_type, feature_config, errors);
  return summary;
}

class Summary {
  constructor(meta, results, paths, nodes, edges, pubs, trials, provenance, tags, errors) {
    this.meta = meta || {};
    this.results = results || [];
    this.paths = paths || {};
    this.nodes = nodes || {};
    this.edges = edges || {};
    this.trials = trials || {};
    this.provenance = provenance ?? {};
    this.tags = tags || {};
    this.errors = errors || {};
    this.publications = pubs ?? {};
    Object.keys(this.publications).forEach(pid => {
      this.publications[pid] = this.publications[pid].to_obj();
    });
  }

  set_timestamp(timestamp) {
    this.meta.timestamp = timestamp;
    for (const node of Object.values(this.nodes)) {
      node.source_time = timestamp;
    }
    for (const edge of Object.values(this.edges)) {
      edge.source_time = timestamp;
    }
  }
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

function _query_type_to_path_compare(query_type) {
  if (trapi.is_pathfinder_query(query_type)) return _path_score_compare;
  return _path_alpha_len_compare;
}

class SummaryMetadata {
  constructor(qid, agents) {
    if (qid === undefined || !cmn.is_string(qid)) {
      throw new TypeError(`Expected argument qid to be of type string, got: ${qid}`);
    }

    if (agents === undefined || !cmn.is_array(agents)) {
      throw new TypeError(`Expected argument agents to be type array, got: ${agents}`);
    }

    this.qid = qid;
    this.aras = agents;
    this.timestamp = null
  }
}

class SummaryPublication {
  constructor(type, url) {
    this.type = type;
    this.url = url;
    this.infores = new Set();
  }

  register_infores(infores) {
    this.infores.add(infores);
  }

  get_infores() {
    return [...this.infores];
  }

  to_obj() {
    return {
      type: this.type,
      url: this.url,
      infores: this.get_infores()
    }
  }
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

function _get_answers_kgraph(answers) {
  return trapi.get_kgraph(answers[0]); // This assumes a fully merged TRAPI message
}

/**
 * Generates a function to extract attributes or properties from a TRAPI object given a set of rules.
 *
 * @param {object[]} rules - The set of rules to use for extracting attributes.
 *
 * @returns {function} - A rule that will apply a list of rules to a source object and produce a list of transformers.
 */
function edgeToString(edge) {
  return `${edge.subject}-${edge.predicate}-${edge.object}`;
}

function updateErrorsFromEdge(edge, errors, edgeErrorReasons) {
  const edgeAras = edge.aras;
  let edgeErrors = null;
  if (edgeAras.length !== 1) {
    edgeErrors = cmn.json_set_default_and_get(errors, 'unknown', []);
  } else {
    edgeErrors = cmn.json_set_default_and_get(errors, edgeAras[0], []);
  }

  edgeErrors.push(...edgeErrorReasons);
}

function reasonsForEdgeErrors(edge) {
  const reasons = [];
  if (!edge.subject || !edge.object || !edge.predicate) {
    reasons.push(`Invalid edge found: ${edgeToString(edge)}`);
  }

  if (!edge.provenance || edge.provenance.length === 0) {
    reasons.push(`No provenance for edge: ${edgeToString(edge)}`);
  }

  return reasons;
}

function _get_summary_path(pid, paths) {
  return cmn.json_get(paths, pid);
}

function _pid_sort(pids, paths, path_compare) {
  if (pids.length < 2) return pids;
  return pids.sort(__pid_compare);

  function __pid_compare(pid_a, pid_b) {
    const summary_path_a = _get_summary_path(pid_a, paths);
    const summary_path_b = _get_summary_path(pid_b, paths);
    const comparison = path_compare(summary_path_a, summary_path_b);
    if (comparison === 0) {
      if (pid_a < pid_b) return -1;
      if (pid_b < pid_a) return 1;
      return 0;
    }
    return comparison;
  }
}

function isRootPath(pid, paths, edges) {
  const summary_path = _get_summary_path(pid, paths);
  let isRoot = true;
  summary_path.forEids((eid) => {
    isRoot = isRoot && edges[eid].is_root;
  });

  return isRoot;
}

function _get_root_pids(pids, paths, edges) {
  const root_pids = pids.filter(pid => isRootPath(pid, paths, edges));
  return root_pids;
}

function cleanup(results, paths, edges, nodes) {
  function clean(section, seenIds) {
    for (let id of Object.keys(section)) {
      if (!seenIds.has(id)) {
        delete section[id];
      }
    }
  }

  const seenPaths = new Set();
  const seenEdges = new Set();
  const seenNodes = new Set();
  for (let res of results) {
    const resPaths = [];
    for (let pid of res.paths) {
      const path = _get_summary_path(pid, paths);
      if (path.length !== 0) {
        resPaths.push(pid);
        seenPaths.add(pid);
      }

      path.forEids(eid => seenEdges.add(eid));
      path.forNids(nid => seenNodes.add(nid));
    }

    res.paths = resPaths;
  }

  clean(paths, seenPaths);
  clean(edges, seenEdges);
  clean(nodes, seenNodes);
}

function _gen_metapath(path, nodes) {
  const metapath = [];
  for (let i = 0; i < path.length; i+=2) {
    const nid = path[i];
    const node = nodes[nid];
    metapath.push(node.get_specific_type());
  }
  return metapath;
}

function _collect_trials_by_disease(nodes) {
  const trials_by_disease = new Map();
  for (const node of Object.values(nodes)) {
    const trials = node.annotations?.chemical?.clinical_trials?.value;
    if (cmn.is_missing(trials)) continue;
    for (const trial of trials) {
      for (const disease_id of trial.disease_ids) {
        if (!nodes.hasOwnProperty(disease_id)) continue;
        let trial_ids = trials_by_disease.get(disease_id);
        if (trial_ids === undefined) {
          trial_ids = new Set();
          trials_by_disease.set(disease_id, trial_ids);
        }
        trial_ids.add(trial.id);
      }
    }
  }
  return trials_by_disease;
}

function _narrow_trials_to_ids(clinical_trials) {
  if (cmn.is_missing(clinical_trials)) return;
  clinical_trials.value = clinical_trials.value.map(trial => trial.id);
}

function _summary_fragments_to_summary(qid, smryFgmts, kgraph, query_type, feature_config, errors) {
  function fgmtPathsToResults(fgmtPaths, nodes, query_type) {
    const results = [];
    fgmtPaths.forEach((path) => {
      let rid = path.start;
      if (trapi.is_pathfinder_query(query_type)) {
        rid = id.gen_pid(_gen_metapath(path.subgraph, nodes));
      }
      results.push(cmn.make_pair('start', rid, 'pid', path.id));
    });

    return results;
  }

  function extendSmryResults(results, newResults) {
    newResults.forEach((result) => {
      let existingResult = cmn.json_set_default_and_get(results, result.start, {});
      let paths = cmn.json_set_default_and_get(existingResult, 'paths', [])
      paths.push(result.pid);
    });
  }

  function extendSmryPaths(paths, newPaths, agents) {
    newPaths.forEach((path) => {
      const summary_path = cmn.json_get(paths, path.id, false);
      if (summary_path) {
        summary_path.extendAgents(agents);
        return;
      }
      path.extendAgents(agents);
      cmn.json_set(paths, path.id, path);
    });
  }

  function _extend_summary_graph_elements(objs, updates, agents, defaultValue) {
    updates.forEach((update) => {
      const obj = cmn.json_set_default_and_get(objs, update.id, defaultValue(update.id));
      update.transforms.forEach((transform) => {
        transform(obj);
        obj.aras.push(...agents);
      });
    });
  }

  function _extend_summary_nodes(nodes, nodeUpdates, agents) {
    _extend_summary_graph_elements(nodes, nodeUpdates, agents, (id) => new SummaryNode(id));
  }

  function extendSmryEdges(edges, edgeFgmts, agents) {
    Object.keys(edgeFgmts.base).forEach((eid) => {
      const edge = cmn.json_set_default_and_get(edges, eid, new SummaryEdge(eid));
      edge.merge(edgeFgmts.base[eid]);
    });

    _extend_summary_graph_elements(edges, edgeFgmts.updates, agents, (id) => new SummaryEdge(id));
  }

  function extendSmryScores(scores, newScores) {
    Object.keys(newScores).forEach((rid) => {
      const currentScores = cmn.json_set_default_and_get(scores, rid, []);
      currentScores.push(...newScores[rid]);
    });
  }

  function extendSmryErrors(errors, newErrors) {
    Object.keys(newErrors).forEach((agtId) => {
      const currentErrors = cmn.json_set_default_and_get(errors, agtId, []);
      currentErrors.push(...newErrors[agtId]);
    });
  }

  function extendSmryPubs(smryPubs, edge) {
    const pubs = cmn.json_get(edge, 'publications', {});
    Object.keys(pubs).forEach((ks) => {
      const pubData = cmn.json_get(pubs, ks, []);
      pubData.forEach((pub) => {
        const pid = pub.id;
        let publication = cmn.json_get(smryPubs, pid, null);
        if (null === publication) {
          const [type, url] = ev.id_to_type_and_url(pid);
          publication = new SummaryPublication(type, url);
          cmn.json_set(smryPubs, pid, publication);
        }
        publication.register_infores(pub.infores);
      });
    });
  }

  function extendSmryTrials(smryTrials, edge) {
    const trials = cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, {});
    Object.keys(trials).forEach((ctid) => {
      const trial = trials[ctid];
      trial.id = ctid;
      const [_, url] = ev.id_to_type_and_url(ctid);
      trial.url = url;
      smryTrials[ctid] = trial;
    });
  }

  function extendSmryProvenance(summaryProvenance, edge) {
    edge.provenance.forEach(source => {
      const infores = source.infores;
      const provenance = bl.infores_to_provenance(infores);
      if (!provenance || !cmn.is_missing(summaryProvenance[infores])) return;
      summaryProvenance[infores] = provenance;
    });
  }

  function filter_redundant_provenance(edge) {
    const infores_with_records = new Set();
    for (let i = 0; i < edge.provenance.length; i++) {
      const source = edge.provenance[i];
      if (source.records && source.records.length > 0) {
        infores_with_records.add(source.infores);
      }
    }
    // Also prioritize source records over default URLs
    edge.provenance = edge.provenance.filter(source => {
      const has_records = source.records && source.records.length > 0;
      return has_records || !infores_with_records.has(source.infores);
    });
  }

  function extractAndFinalizeEdges(edges) {
    const pubs = {};
    const trials = {};
    const provenance = {};
    Object.values(edges).forEach((edge) => {
      extendSmryPubs(pubs, edge);
      extendSmryTrials(trials, edge);
      extendSmryProvenance(provenance, edge);
      edge.knowledge_level = provenance[edge.provenance[0].infores].knowledge_level;
      filter_redundant_provenance(edge);
      const edgePubs = cmn.json_get(edge, 'publications', {})
      const supText = cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT, {});
      Object.keys(edgePubs).forEach((kl) => {
        edgePubs[kl] = edgePubs[kl].map((pub) => {
          return {
            id: pub.id,
            support: supText[pub.id] ?? null,
            infores: pub.infores
          };
        });
      });
      delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT];
      edge.trials = Object.keys(cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, []));
      delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS];
      const unqualified_predicate = pred.get_most_specific_predicate(edge);
      const canonical_predicate = pred.gen_canonical_qualified_predicate(edge);
      taglib.set_tag(edge, summary_tags.make_predicate_tag(canonical_predicate));
      cmn.json_set(edge, 'predicate', pred.gen_qualified_predicate(edge, edge.is_inverted()));
      cmn.json_set(edge, 'predicate_url', bl.pred_to_url(unqualified_predicate));
      cmn.json_set(edge, 'description', bl.get_predicate_description(unqualified_predicate));
      if (edge.is_inverted()) {
        const tmp = edge.subject;
        edge.subject = edge.object;
        edge.object = tmp;
      }
      delete edge['qualifiers'];
    });

    return [edges, pubs, trials, provenance];
  }

  function __gen_results_and_tags_from_results(results, paths, nodes,
      edges, scores, errors, query_type) {
    const used_tags = {};
    const expanded_results_map = {};
    for (const result of results) {
      const pids = result.paths;
      const root_pids = _get_root_pids(pids, paths, edges);
      // Bail if there are no root paths
      if (cmn.is_array_empty(root_pids)) {
        let aras = new Set();
        for (const pid of pids) {
          const path = paths[pid];
          for (const ara of path.aras) {
            aras.add(ara);
          }
        }
        aras = [...aras];
        const error_message = "No root paths found";
        logger.warn(`${aras.join(', ')}: ${error_message}`);
        for (const ara of aras) {
          const ara_errors = cmn.json_set_default_and_get(errors, ara, []);
          ara_errors.push(error_message);
        }
        continue;
      }

      const summary_path = _get_summary_path(root_pids[0], paths);
      const name = __gen_name(nodes, summary_path, query_type);
      const start = summary_path.start;
      const end = summary_path.end;
      const tags = {};
      pids.forEach((pid) => {
        const path = _get_summary_path(pid, paths);
        for (const tag of taglib.get_tags(path)) {
          if (summary_tags.is_result_tag(tag) && path.start !== start) continue;
          used_tags[tag.id] = tag.description;
          tags[tag.id] = null;
        };
      });

      const result_id = __gen_id(nodes, summary_path, query_type);
      const expanded_result = expanded_results_map[result_id];
      if (cmn.is_missing(expanded_result)) {
        expanded_results_map[result_id] = {
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
    const expanded_results = Object.values(expanded_results_map);
    for (let i = 0; i < expanded_results.length; i++) {
      const result = expanded_results[i];
      _pid_sort(result.paths, paths, _query_type_to_path_compare(query_type));
    }
    return [expanded_results, used_tags];

    function __gen_name(nodes, summary_path, query_type) {
      if (trapi.is_pathfinder_query(query_type)) {
        const metapath = _gen_metapath(summary_path.subgraph, nodes)
        metapath[0] = nodes[summary_path.start].name();
        metapath[metapath.length-1] = nodes[summary_path.end].name();
        return metapath.join('/');
      }
      return nodes[summary_path.start].name();
    }

    function __gen_id(nodes, summary_path, query_type) {
      if (trapi.is_pathfinder_query(query_type)) {
        const metapath = _gen_metapath(summary_path.subgraph, nodes);
        return id.gen_pid(metapath);
      }
      return id.gen_pid([summary_path.start, summary_path.end]);
    }

  }

  let results = {};
  let paths = {};
  let nodes = {};
  let edges = {};
  let pubs = {};
  let trials = {};
  let scores = {};
  let summaryProvenance = {};
  let tags = [];
  smryFgmts.forEach((sf) => {
    const agents = sf.agents;
    _extend_summary_nodes(nodes, sf.nodes, agents);
    extendSmryEdges(edges, sf.edges, agents);
    extendSmryScores(scores, sf.scores);
    extendSmryErrors(errors, sf.errors);
  });

  Object.values(nodes).forEach(node => {
    node.types.sort(bl.biolink_class_cmp_fn);
  });

  smryFgmts.forEach((sf) => {
    const agents = sf.agents;
    const newResults = fgmtPathsToResults(sf.paths, nodes, query_type);
    extendSmryResults(results, newResults);
    extendSmryPaths(paths, sf.paths, agents);
  });

  results = Object.values(results).map(cmn.obj_remove_duplicates)
  function pushIfEmpty(arr, val) {
    if (cmn.is_array_empty(arr)) {
      arr.push(val);
    }
  };

  // Edge post-processing
  Object.keys(edges).forEach((eid) => {
    const edge = edges[eid];
    // Remove any edges that have a missing subject, object, predicate, or provenance
    const edgeErrorReasons = reasonsForEdgeErrors(edge);
    if (edgeErrorReasons.length !== 0) {
      console.error(`Found invalid edge ${eid}. Reasons: ${JSON.stringify(edgeErrorReasons)}`);
      updateErrorsFromEdge(edge, errors, edgeErrorReasons);
      delete edges[eid];
      return;
    }

    // Remove any duplicates on all edge attributes
    cmn.obj_remove_duplicates(edge);

    // Remove duplicates from publications and generate pub/misc tags
    const pubs = cmn.json_get(edge, 'publications', {});
    Object.keys(pubs).forEach((kl) => {
      const klPubs = cmn.json_get(pubs, kl, []);
      const seenIds = new Set();
      cmn.json_set(pubs, kl, klPubs.filter((pub) => {
        const shouldInclude = !seenIds.has(pub.id);
        if (ev.is_publication(pub.id)) {
          taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.PUBLICATIONS)
        }
        seenIds.add(pub.id);
        return shouldInclude;
      }));
    });

    edge.provenance.forEach(source => {
      if (!bl.is_valid_infores(source.infores)) {
        edgeErrorReasons.push(`Found invalid infores: ${source.infores} on edge ${edgeToString(edge)}`);
      }
    });

    if (edgeErrorReasons.length !== 0) {
      updateErrorsFromEdge(edge, errors, edgeErrorReasons);
      delete edges[eid];
      return
    }

    // Generate other edge evidence tags
    if (!cmn.is_array_empty(Object.keys(edge.supporting_trials))) {
      taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.CLINICAL);
    }
    // TODO: Refine meaning of an ontological edge
    if (ev.is_ontological(edge.predicate)) {
      taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.ONTOLOGICAL);
    }
    taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.OTHER);
  });

  [edges, pubs, trials, summaryProvenance] = extractAndFinalizeEdges(edges);

  const meta = new SummaryMetadata(qid, cmn.distinct_array(smryFgmts.map((sf) => {
    return sf.agents;
  }).flat()));

  // Node post-processing
  const trials_by_disease = _collect_trials_by_disease(nodes);
  Object.keys(nodes).forEach((nid) => {
    const node = nodes[nid];
    node.curies.push(nid);
    // Remove any duplicates on all node attributes
    cmn.obj_remove_duplicates(node);
    node.types.sort(bl.biolink_class_cmp_fn);
    _conflate_gene_and_protein(node);

    // Provide a CURIE as a default value if the node has no name
    const node_names = node.names;
    pushIfEmpty(node_names, nid);

    const provenance = bl.curie_to_normalized_url(nid, node.curies);
    if (!provenance) {
      logger.warn(`WARNING: No provenance for CURIE: ${nid}`);
    } else {
      node.provenance.push(provenance);
    }

    if (bta.is_chemical(node)) {
      _narrow_trials_to_ids(node.annotations.chemical.clinical_trials);
    }
    if (bta.is_disease(node)) {
      const curie_entries = node.annotations?.disease?.curies?.value ?? [];
      node.curies.push(...curie_entries.map(entry => entry.curie));
      const trial_ids = trials_by_disease.get(nid);
      if (trial_ids !== undefined) {
        node.annotations.disease.clinical_trials = bta.make_clinical_trials_section([...trial_ids]);
      }
    }
    if (bta.is_gene(node)) {
      const tdls = node.annotations.gene.tdl?.value ?? [];
      for (let i = 0; i < tdls.length; i++) {
        const tdl = tdls[i];
        taglib.set_tag(node, summary_tags.make_tdl_tag(tdl));
      }
    }
    node.curies = cmn.distinct_array(node.curies);
    bta.inject_curies(node);
  });

  Object.keys(paths).forEach((pid) => {
    const path = _get_summary_path(pid, paths);
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
    if (has_dangling_ref) {
      delete paths[pid];
      return;
    }
    const path_start = nodes[path.start];
    if (bta.is_chemical(path_start)) {
      summary_tags.decorate_clinical_tag(path_start);
      summary_tags.decorate_role_tags(path_start);
      summary_tags.decorate_otc_tag(path_start);
      // Generate tags for chemical indications
      if (trapi.is_chemical_disease_query(query_type)) {
        summary_tags.decorate_indication_tag(path, nodes);
      }
    }
  });

  // Path post-processing
  Object.keys(paths).forEach((pid) => {
    const path = _get_summary_path(pid, paths);
    // Promote edge tags to path tags
    path.forEids((eid) => {
      summary_tags.promote_edge_tags_to_path(edges[eid], path);
    });
    path.removeDuplicates();
    // Paths inherit node tags
    summary_tags.promote_start_node_tags_to_path(nodes[path.start], path);
    path.forInternalNids((nid) => {
      const node = nodes[nid];
      summary_tags.promote_internal_node_tags_to_path(node, path);
      taglib.set_tag(path, summary_tags.make_node_type_tag(node.get_specific_type()));
    });
    // Generate a special tag for the answer node
    summary_tags.decorate_max_phase_tag(path, nodes, query_type);
    // Generate tags based on the aras for this path
    const agent_infores = path.agents;
    agent_infores.forEach((infores) => {
      const provenance = bl.infores_to_provenance(infores);
      if (cmn.is_missing(provenance)) throw new fraglib.MissingInforesError(infores);
      taglib.set_tag(path, summary_tags.make_ara_tag(infores, provenance.name));
    });
    // Generate tags for number of connections
    taglib.set_tag(path, summary_tags.make_path_length_tag(path.edgeCount));
    path.tags = taglib.tags_to_raw_obj(path);
  });

  Object.keys(edges).forEach((eid) => {
    const edge = cmn.json_get(edges, eid);
    edge.tags = taglib.tags_to_raw_obj(edge);
  });

  results.forEach((res) => {
    res.paths = res.paths.filter(pid => paths[pid] !== undefined);
  });

  cleanup(results, paths, edges, nodes);
  [results, tags] = __gen_results_and_tags_from_results(results, paths, nodes,
      edges, scores, errors, query_type);

  Object.values(nodes).forEach((node) => {
    node.tags = taglib.tags_to_raw_obj(node);
  });

  return new Summary(meta, results, paths, nodes, edges, pubs, trials, summaryProvenance, tags, errors);
}

function _conflate_gene_and_protein(node) {
  const node_type = node.get_specific_type();
  if (node_type === "Protein") {
    const gene_type = bl.tag_biolink("Gene");
    const gene_index = node.types.indexOf(gene_type);
    if (gene_index > -1) {
      node.types.splice(gene_index, 1);
      node.types.unshift(gene_type);
    }
  }
}

class AuxGraphNotFoundError extends Error {
  constructor(auxGraph) {
    super(`Auxiliary graph not found for ${auxGraph}`);
  }
}
