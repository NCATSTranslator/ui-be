'use strict';

export { answers_to_summary }

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';
import * as bta from '../biothings-annotation.mjs';
import * as trapi from '../trapi/core.mjs';
import * as trapi_rules from '../trapi/property-rules.mjs';
import * as summary_rules from './property-rules.mjs';
import * as sa from './summary-analysis.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';
import { SummaryNode } from './SummaryNode.mjs';
import * as taglib from '../taglib.mjs';

/**
 * Responsible for converting a set of TRAPI answers into a summarized form that the FE application can use.
 *
 * @param {string}   qid - The query ID for the given answer set.
 * @param {object[]} answers - The set of TRAPI answers to summarize.
 * @param {number}   max_hops - The maximum number of hops to consider when summarizing the answers.
 *
 * @returns {object} - The summarized form of the answers.
 */
function answers_to_summary(qid, answers, max_hops) {
  if (!cmn.is_array(answers) || cmn.is_array_empty(answers)) {
    logger.warn(`No results for query with PK ${qid}`);
    const default_metadata = new SummaryMetadata(qid, []);
    return new Summary(default_metadata);
  }

  const knode_summarization_rules = _make_extraction_rules(
    [
      trapi_rules.make_rule_aggregate_property({
        source_key: 'name',
        target_key: 'names'
      }),
      trapi_rules.make_rule_aggregate_property({
        source_key: 'categories',
        target_key: 'types'
      }),
      trapi_rules.make_rule_transform_attribute_value({
        attr_id: bl.tagBiolink('synonym'),
        transform: cmn.identity,
        update: (target, synonyms) => {
          const current = cmn.jsonSetDefaultAndGet(target, "synonyms", []);
          current.push(...synonyms);
          return target;
        },
        default_value: []
      }),
      trapi_rules.make_rule_map_attributes({
        attr_ids: [bl.tagBiolink('same_as')],
        target_key: 'curies',
        transform: attr => {
          const valid_xrefs = [];
          trapi.get_attr_val(attr).forEach(xref => {
            if (bl.isValidCurie(xref)) {
              valid_xrefs.push(xref)
            }
          });
          return valid_xrefs;
        }
      }),
      trapi_rules.make_rule_collect_attribute_values({
        attr_ids: [bl.tagBiolink('description')],
        target_key: 'descriptions',
      }),
      bta.make_rule_collect_chemical_annotations(),
      bta.make_rule_collect_gene_annotations(),
      bta.make_rule_collect_disease_annotations()
    ]);

  const kedge_summarization_rules = _make_extraction_rules(
    [
      trapi_rules.make_rule_transform_property({
        source_key: trapi.CONSTANTS.GRAPH.EDGE.PREDICATE,
        transform: (obj, key) => bl.sanitizeBiolinkItem(cmn.jsonGet(obj, key))
      }),
      trapi_rules.make_rule_transform_and_aggregate_property({
        source_key: trapi.CONSTANTS.GRAPH.SOURCES.KEY,
        target_key: 'provenance',
        transform: (obj, key) => trapi.get_primary_source(obj)
      }),
      trapi_rules.make_rule_transform_property({
        source_key: trapi.CONSTANTS.GRAPH.EDGE.QUALIFIER.KEY,
        transform: (obj, key) => cmn.jsonGet(obj, key, false)
      }),
      trapi_rules.make_rule_get_property(trapi.CONSTANTS.GRAPH.EDGE.SUBJECT),
      trapi_rules.make_rule_get_property(trapi.CONSTANTS.GRAPH.EDGE.OBJECT),
      summary_rules.make_rule_group_publications_by_knowledge_level(),
      summary_rules.make_rule_collect_publication_supporting_text(),
      summary_rules.make_rule_collect_semmed_sentences(),
      summary_rules.make_rule_collect_clinical_trial_metadata()
    ]);

  const query_type = _answers_to_query_type(answers);
  const [summary_fragments, errors] = _answers_to_summary_fragments(answers,
      knode_summarization_rules, kedge_summarization_rules, max_hops);
  const summary = _summary_fragments_to_summary(qid, summary_fragments,
      _get_answers_kgraph(answers), query_type, errors);
  return summary;
}

class Summary {
  constructor(meta, results, paths, nodes, edges, pubs, trials, tags, errors) {
    this.meta = meta || {};
    this.results = results || [];
    this.paths = paths || {};
    this.nodes = nodes || {};
    this.edges = edges || {};
    this.publications = pubs || {};
    this.trials = trials || {};
    this.tags = tags || {};
    this.errors = errors || {};
  }

  set_timestamp(timestamp) {
    this.meta.timestamp = timestamp;
  }
}

class SummaryFragment {
  constructor(agents, paths, nodes, edges, scores, errors) {
    this.agents = agents || [];
    this.paths = paths   || [];
    this.nodes = nodes   || [];
    this.edges = edges   || new SummaryFragmentEdges();
    this.scores = scores || {};
    this.errors = errors || {};
  }

  isEmpty() {
    return cmn.is_array_empty(this.paths) &&
           cmn.is_array_empty(this.nodes) &&
           this.edges.isEmpty()
  }

  pushScore(rid, scoringComponents) {
    const resultScores = cmn.jsonSetDefaultAndGet(this.scores, rid, []);
    resultScores.push(scoringComponents);
  }

  pushAgent(agent) {
    this.agents.push(agent);
  }

  pushError(agent, error) {
    const currentError = cmn.jsonSetDefaultAndGet(this.errors, agent, []);
    currentError.push(error);
  }

  merge(smryFgmt) {
    this.agents.push(...smryFgmt.agents);
    this.paths.push(...smryFgmt.paths);
    this.nodes.push(...smryFgmt.nodes);
    this.edges.merge(smryFgmt.edges);
    this._mergeFgmtObjects(this.scores, smryFgmt.scores);
    this._mergeFgmtObjects(this.errors, smryFgmt.errors);
    return this
  }

  _mergeFgmtObjects(obj1Prop, obj2Prop) {
    Object.keys(obj2Prop).forEach((k) => {
      const current = cmn.jsonSetDefaultAndGet(obj1Prop, k, []);
      current.push(...obj2Prop[k]);
    });
  }
}

function make_error_fragment(agent, error) {
  const smryFgmt = new SummaryFragment();
  smryFgmt.pushAgent(agent);
  smryFgmt.pushError(agent, error);
  return smryFgmt;
}

class SummaryFragmentEdges {
  constructor(base, updates) {
    this.base = base || {};
    this.updates = updates || [];
  }

  isEmpty() {
    return cmn.isObjectEmpty(this.base) && cmn.is_array_empty(this.updates);
  }

  merge(smryFgmtEdges) {
    Object.keys(smryFgmtEdges.base).forEach((eid) => {
      const currentEdge = cmn.jsonSetDefaultAndGet(this.base, eid, new SummaryEdge());
      currentEdge.merge(smryFgmtEdges.base[eid]);
    });

    this.updates.push(...smryFgmtEdges.updates);
    return this;
  }
}

class SummaryPath {
  constructor(subgraph, agents) {
    this.subgraph = subgraph;
    this.aras = agents;
    taglib.make_taggable(this);
  }

  extendAgents(agents) {
    this.aras.concat(agents);
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

function _summary_path_compare(summary_path_a, summary_path_b) {
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
  constructor(type, url, src) {
    this.type = type;
    this.url = url;
    this.source = src;
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

function inforesToName(infores) {
  return bl.inforesToProvenance(infores).name;
}

/**
 * Generates a function to extract attributes or properties from a TRAPI object given a set of rules.
 *
 * @param {object[]} rules - The set of rules to use for extracting attributes.
 *
 * @returns {function} - A rule that will apply a list of rules to a source object and produce a list of transformers.
 */
function _make_extraction_rules(rules) {
  return (src, cxt) => {
    return rules.map(rule => { return rule(src, cxt); });
  };
}

/**
 * Get the endpoints for a TRAPI result graph.
 *
 * @param {object} result - The TRAPI result.
 * @param {string} startKey - The key to use for the start node. There should only be a single start node.
 * @param {string} endKey - The key to use for the end nodes. There can be multiple end nodes.
 *
 * @returns {string[]} - The start and end nodes for the result graph.
 * @throws {NodeBindingNotFoundError} - If either of the start or end nodes are not found.
 */
function _get_result_endpoints(result, start_key, end_key) {
  // Flatten the node bindings for a specific key of a result into a list of IDs
  const rnode_start = __flatten_binding(result, start_key)[0];
  const rnode_ends = __flatten_binding(result, end_key); // There can be multiple endpoints
  return [rnode_start, rnode_ends];

  function __flatten_binding(result, key) {
    const node_bindings = trapi.get_node_bindings(result, key);
    if (!cmn.is_array(node_bindings) || cmn.is_array_empty(node_bindings)) {
      throw new NodeBindingNotFoundError(nodeBinding);
    }
    // TODO: move node binding getters to trapi/core.mjs
    return node_bindings.map((entry) => {
      const endpoint = cmn.jsonGet(entry, 'id');
      return endpoint;
    });
  }
}

/**
 * Gets a summarized edge tag based on the Knowledge Level and Agent Type of the edge. Defaults to using the Knowledge Level provided by the infores catalog if the tag cannot be determined by using the edge attributes.
 *
 * @param {object} kedge - The knowledge edge to summarize.
 * @param {object} provenance - The provenance of the edge.
 *
 * @returns {string} - The summarized edge tag.
 */
// TODO: Add constants for KL/AT and the summarized KL
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

function _get_tag_family(tag) {
  return tag.id.split('/')[1];
}

function _is_result_tag(tag) {
  return tag.id.startsWith('r/');
}

function _is_external_tag(tag) {
  const valid_families = ['cc', 'di', 'pc', 'pt', 'role', 'ara', 'otc', 'tdl'];
  const family = _get_tag_family(tag);
  return valid_families.includes(family);
}

function _is_fda_tag(tag) {
  return tag.id.startsWith('r/fda');
}

function _gen_max_phase_tag(node, query_type) {
  // Only generate this tag for non-gene/chemical queries
  if (!trapi.is_valid_query(query_type) || trapi.is_gene_chemical_query(query_type)) {
    return false;
  }
  const fda_tags = taglib.get_tags(node).filter(_is_fda_tag);
  let highest_fda_approval = 0;
  if (!cmn.is_array_empty(fda_tags)) {
    highest_fda_approval = Math.max(...fda_tags.map((tag) => {
      return parseInt(tag.id.split('/')[2]);
    }));
  }
  if (__is_drug(node, highest_fda_approval)) return _CONSTANTS.TAGS.RESULT.CHEMICAL_CLASS.DRUG;
  if (__is_clinical_phase(highest_fda_approval)) {
    return new taglib.Tag({
      id: `r/cc/phase${highest_fda_approval}`,
      name: `Phase ${highest_fda_approval} Drug`
    });
  }
  return _CONSTANTS.TAGS.RESULT.CHEMICAL_CLASS.OTHER;

  function __is_drug(node, fda_level) {
    return fda_level === 4 || node.get_specific_type() === 'Drug';
  }

  function __is_clinical_phase(fda_level) {
    return fda_level > 0 && fda_level < 4;
  }
}

function _make_knode_transform(nid, knode, knode_summarization_rules, summary_context) {
  return cmn.make_pair('id',         nid,
                       'transforms', knode_summarization_rules(knode, summary_context));
}

function _make_kedge_transform(eid, kedge, kedge_summarization_rules, summary_context) {
  const edge_context = cmn.deepCopy(summary_context)
  const provenance = bl.inforesToProvenance(trapi.get_primary_source(kedge));
  edge_context.provenance = provenance;
  edge_context.knowledge_level = _get_knowledge_level(kedge, provenance);
  return cmn.make_pair('id',         eid,
                       'transforms', kedge_summarization_rules(kedge, edge_context));
}

function edgeToString(edge) {
  return `${edge.subject}-${edge.predicate}-${edge.object}`;
}

function updateErrorsFromEdge(edge, errors, edgeErrorReasons) {
  const edgeAras = edge.aras;
  let edgeErrors = null;
  if (edgeAras.length !== 1) {
    edgeErrors = cmn.jsonSetDefaultAndGet(errors, 'unknown', []);
  } else {
    edgeErrors = cmn.jsonSetDefaultAndGet(errors, edgeAras[0], []);
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

function _answers_to_summary_fragments(
    answers,
    knode_summarization_rules,
    kedge_summarization_rules,
    max_hops) {
  function resultToSmryFgmt(result, kgraph, auxGraphs, startKey, endKey, errors) {
    function analysis_to_fragment(analysis, kgraph, auxiliary_graphs, start, ends) {
      const agent = cmn.jsonGet(analysis, 'resource_id', false);
      if (!agent) {
        return make_error_fragment('unknown', 'Expected analysis to have resource_id');
      }

      try {
        const max_path_len = (2 * max_hops) + 1;
        const summary_analysis = sa.analysis_to_summary_analysis(analysis, kgraph, auxiliary_graphs);
        const analysis_paths = sa.gen_analysis_paths(summary_analysis, kgraph, start, ends, max_path_len);
        const [summary_paths, summary_edges] = sa.summary_analysis_to_summary_paths_and_edges(
          summary_analysis, analysis_paths, kgraph);
        const analysis_context = {
          agent: agent,
          errors: errors
        };
        const knode_properties = summary_analysis.node_ids.map(nb => {
          const knode = trapi.get_knode(nb, kgraph);
          return _make_knode_transform(nb, knode, knode_summarization_rules, analysis_context);
        });
        const kedge_properties = [];
        for (const [eid, edge] of Object.entries(summary_edges)) {
          for (const eb of edge.metadata.edge_bindings) {
            const kedge = trapi.get_kedge(eb, kgraph);
            kedge_properties.push(_make_kedge_transform(eid, kedge, kedge_summarization_rules, analysis_context));
          }
        }
        return new SummaryFragment(
          [agent],
          summary_paths,
          knode_properties,
          new SummaryFragmentEdges(summary_edges, kedge_properties)
        );
      } catch (err) {
        console.error(err);
        if (err instanceof trapi.EdgeBindingNotFoundError) {
          return make_error_fragment(agent, e.message);
        }

        return make_error_fragment(agent, 'Unknown error while building Analysis Graph');
      }
    }

    try {
      const [rnodeStart, rnodeEnds] = _get_result_endpoints(result, startKey, endKey);
      // TODO: There SHOULD only be a single start point. We should probably throw an error when this is not the case.
      const analyses = getResultAnalyses(result);
      const resultSmryFgmt = analyses.reduce(
        (rsf, analysis) => {
          return rsf.merge(analysis_to_fragment(analysis, kgraph, auxGraphs, rnodeStart, rnodeEnds));
        },
        new SummaryFragment());

      if (!resultSmryFgmt.isEmpty()) {
        // Ordering components are a property of the result, so we have to add them after the result analyses are summarized.
        const rid = id.gen_nid(rnodeStart, kgraph); // The first node uniquely identifies a result.
        const scoringComponents = getResultScoringComponents(result);
        resultSmryFgmt.pushScore(rid, scoringComponents);
      }

      return resultSmryFgmt;
    } catch (err) {
      console.error(err);
      if (err instanceof NodeBindingNotFoundError) {
        return make_error_fragment('unknown', err.message);
      }

      return make_error_fragment('unknown', 'Unknown error while building result summary fragment');
    }
  }

  const smryFgmts = [];
  const errors = {};
  answers.forEach((answer) => {
    const results = trapi.get_results(answer);
    if (!results) {
      // TODO: Add warning
      return;
    }

    // TODO: What to do if these fail
    const kgraph = trapi.get_kgraph(answer);
    const auxGraphs = trapi.get_auxiliary_graphs(answer);
    const [startKey, endKey] = trapi.message_to_endpoints(answer);

    // TODO: Where is the error handling?
    results.forEach((result) => {
      const sf = resultToSmryFgmt(result, kgraph, auxGraphs, startKey, endKey, errors);
      // TODO: Empty summary fragments should throw an error (at least in some cases)
      if (!sf.isEmpty()) {
        smryFgmts.push(sf);
      }
    });
  });

  return [smryFgmts, errors];
}

function _get_summary_path(pid, paths) {
  return cmn.jsonGet(paths, pid);
}

function _pid_sort(pids, paths) {
  if (pids.length < 2) return pids;
  return pids.sort(__pid_compare);

  function __pid_compare(pid_a, pid_b) {
    const summary_path_a = _get_summary_path(pid_a, paths);
    const summary_path_b = _get_summary_path(pid_b, paths);
    const comparison = _summary_path_compare(summary_path_a, summary_path_b);
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
    isRoot = isRoot && edges[eid].isRootPath;
  });

  return isRoot;
}

function _get_root_pids(pids, paths, edges) {
  const root_pids = pids.filter(pid => isRootPath(pid, paths, edges));
  return root_pids;
}

function genSupChain(pids, paths, edges) {
  const seenPids = [];
  const remaining = _get_root_pids(pids, paths, edges);
  while (remaining.length !== 0) {
    const next = remaining.pop();
    if (seenPids.includes(next)) continue;
    seenPids.push(next);
    const summary_path = _get_summary_path(next, paths);
    summary_path.forEids((eid) => {
      const edgeSup = edges[eid].supPaths;
      remaining.push(...edgeSup.filter((spid) => !seenPids.includes(spid)));
    });
  }

  return seenPids;
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

function _summary_fragments_to_summary(qid, smryFgmts, kgraph, query_type, errors) {
  function fgmtPathsToResultsAndPaths(fgmtPaths, nodes, query_type) {
    const results = [];
    const paths = [];
    fgmtPaths.forEach((path) => {
      const pid = id.gen_pid(path);
      let rid = path[0];
      if (trapi.is_pathfinder_query(query_type)) {
        rid = id.gen_pid(_gen_metapath(path, nodes));
      }
      results.push(cmn.make_pair('start', rid, 'pid',   pid));
      paths.push(cmn.make_pair('pid',  pid, 'path', path));
    });

    return [results, paths];
  }

  function extendSmryResults(results, newResults) {
    newResults.forEach((result) => {
      let existingResult = cmn.jsonSetDefaultAndGet(results, result.start, {});
      let paths = cmn.jsonSetDefaultAndGet(existingResult, 'paths', [])
      paths.push(result.pid);
    });
  }

  function extendSmryPaths(paths, newPaths, agents) {
    newPaths.forEach((path) => {
      const summary_path = cmn.jsonGet(paths, path.pid, false);
      if (summary_path) {
        summary_path.extendAgents(agents);
        return;
      }

      cmn.jsonSet(paths, path.pid, new SummaryPath(path.path, agents));
    });
  }

  function _extend_summary_graph_elements(objs, updates, agents, defaultValue) {
    updates.forEach((update) => {
      const obj = cmn.jsonSetDefaultAndGet(objs, update.id, defaultValue());
      update.transforms.forEach((transform) => {
        transform(obj);
        obj.aras.push(...agents);
      });
    });
  }

  function _extend_summary_nodes(nodes, nodeUpdates, agents) {
    _extend_summary_graph_elements(nodes, nodeUpdates, agents, () => new SummaryNode());
  }

  function extendSmryEdges(edges, edgeFgmts, agents) {
    Object.keys(edgeFgmts.base).forEach((eid) => {
      const edge = cmn.jsonSetDefaultAndGet(edges, eid, new SummaryEdge());
      edge.merge(edgeFgmts.base[eid]);
    });

    _extend_summary_graph_elements(edges, edgeFgmts.updates, agents, () => new SummaryEdge());
  }

  function extendSmryScores(scores, newScores) {
    Object.keys(newScores).forEach((rid) => {
      const currentScores = cmn.jsonSetDefaultAndGet(scores, rid, []);
      currentScores.push(...newScores[rid]);
    });
  }

  function extendSmryErrors(errors, newErrors) {
    Object.keys(newErrors).forEach((agtId) => {
      const currentErrors = cmn.jsonSetDefaultAndGet(errors, agtId, []);
      currentErrors.push(...newErrors[agtId]);
    });
  }

  function extendSmryPubs(smryPubs, edge) {
    const pubs = cmn.jsonGet(edge, 'publications', {});
    Object.keys(pubs).forEach((ks) => {
      const pubData = cmn.jsonGet(pubs, ks, []);
      pubData.forEach((pub) => {
        const pid = pub.id;
        const [type, url] = ev.idToTypeAndUrl(pid);
        cmn.jsonSet(smryPubs, pid, new SummaryPublication(type, url, pub.src));
      });
    });
  }

  function extendSmryTrials(smryTrials, edge) {
    const trials = cmn.jsonGet(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, {});
    Object.keys(trials).forEach((ctid) => {
      const trial = trials[ctid];
      trial.id = ctid;
      const [_, url] = ev.idToTypeAndUrl(ctid);
      trial.url = url;
      smryTrials[ctid] = trial;
    });
  }

  function extractAndFinalizeEdges(edges) {
    const pubs = {};
    const trials = {};
    Object.values(edges).forEach((edge) => {
      extendSmryPubs(pubs, edge);
      extendSmryTrials(trials, edge);
      const edgePubs = cmn.jsonGet(edge, 'publications', {})
      const supText = cmn.jsonGet(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT, {});
      Object.keys(edgePubs).forEach((kl) => {
        edgePubs[kl] = edgePubs[kl].map((pub) => {
          return { id: pub.id, support: supText[pub.id] || null };
        });
      });
      delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT];
      edge.trials = Object.keys(cmn.jsonGet(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, []));
      delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS];
      const unqualified_predicate = pred.getMostSpecificPred(edge);
      cmn.jsonSet(edge, 'predicate', pred.genQualifiedPred(edge, edge.isInverted()));
      cmn.jsonSet(edge, 'predicate_url', bl.predToUrl(unqualified_predicate));
      cmn.jsonSet(edge, 'description', bl.get_predicate_description(unqualified_predicate));
      if (edge.isInverted()) {
        const tmp = edge.subject;
        edge.subject = edge.object;
        edge.object = tmp;
      }
      delete edge['qualifiers'];
    });

    return [edges, pubs, trials];
  }

  function __gen_results_and_tags_from_results(results, paths, nodes,
      edges, scores, errors, query_type) {
    const used_tags = {};
    const expanded_results = [];
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
          const ara_errors = cmn.jsonSetDefaultAndGet(errors, ara, []);
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
          if (_is_result_tag(tag) && path.start !== start) continue;
          used_tags[tag.id] = tag.description;
          tags[tag.id] = null;
        };
      });
      // Generate direct/indirect tags for results
      root_pids.forEach((pid) => {
        const summary_path = _get_summary_path(pid, paths);
        let is_path_indirect = false;
        summary_path.forEids((eid) => {
          const edge = edges[eid];
          is_path_indirect = is_path_indirect || edge.hasSupport();
        });
        let tag = null;
        if (is_path_indirect) {
          tag = _CONSTANTS.TAGS.PATH.TYPE.INDIRECT;
          __tag_path_as_indirect(pid, edges, paths);
        } else {
          tag = _CONSTANTS.TAGS.PATH.TYPE.DIRECT;
          taglib.set_tag(summary_path, tag);
        }
        used_tags[tag.id] = tag.description;
        tags[tag.id] = null;
      });

      expanded_results.push({
        'id': __gen_id(nodes, summary_path, query_type),
        'subject': start,
        'drug_name': name,
        'paths': _pid_sort(root_pids, paths),
        'object': end,
        'scores': scores[start],
        'tags': tags
      });
    }

    return [expanded_results, used_tags];

    function __tag_path_as_indirect(pid, edges, paths) {
      function helper(pid, edges, paths, seen) {
        seen.add(pid);
        const summary_path = _get_summary_path(pid, paths);
        summary_path.forEids((eid) => {
          const edge = edges[eid];
          if (edge.hasSupport()) {
            for (let spid of edge.supPaths) {
              if (!seen.has(spid)) {
                helper(spid, edges, paths, seen);
              }
            }
          }
        });
        taglib.set_tag(summary_path, _CONSTANTS.TAGS.PATH.TYPE.INDIRECT);
      }
      helper(pid, edges, paths, new Set());
    }

    function __gen_name(nodes, summary_path, query_type) {
      if (trapi.is_pathfinder_query(query_type)) {
        const metaPath = _gen_metapath(summary_path.subgraph, nodes)
        metaPath[0] = nodes[summary_path.start].name();
        metaPath[metaPath.length-1] = nodes[summary_path.end].name();
        return metaPath.join('/');
      }
      return nodes[summary_path.start].name();
    }

    function __gen_id(nodes, summary_path, query_type) {
      if (trapi.is_pathfinder_query(query_type)) {
        const metapath = _gen_metapath(summary_path.subgraph, nodes);
        return id.gen_pid(metaPath);
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
  let tags = [];
  smryFgmts.forEach((sf) => {
    const agents = sf.agents;
    _extend_summary_nodes(nodes, sf.nodes, agents);
    extendSmryEdges(edges, sf.edges, agents);
    extendSmryScores(scores, sf.scores);
    extendSmryErrors(errors, sf.errors);
  });

  Object.values(nodes).forEach(node => {
    node.types.sort(bl.biolinkClassCmpFn);
  });

  smryFgmts.forEach((sf) => {
    const agents = sf.agents;
    const [newResults, newPaths] = fgmtPathsToResultsAndPaths(sf.paths, nodes, query_type);
    extendSmryResults(results, newResults);
    extendSmryPaths(paths, newPaths, agents);
  });

  results = Object.values(results).map(cmn.objRemoveDuplicates)
  function pushIfEmpty(arr, val) {
    if (cmn.is_array_empty(arr)) {
      arr.push(val);
    }
  };

  // Edge post-processing
  Object.keys(edges).forEach((eid) => {
    const edge = edges[eid];
    // Remove any empty edges. TODO: Why are these even here?
    if (Object.keys(edge).length === 2 && edge.aras !== undefined && edge.supPaths !== undefined) {
      delete edges[eid];
      return;
    }

    // Remove any edges that have a missing subject, object, predicate, or provenance
    const edgeErrorReasons = reasonsForEdgeErrors(edge);
    if (edgeErrorReasons.length !== 0) {
      console.error(`Found invalid edge ${eid}. Reasons: ${JSON.stringify(edgeErrorReasons)}`);
      updateErrorsFromEdge(edge, errors, edgeErrorReasons);
      delete edges[eid];
      return;
    }

    // Remove any duplicates on all edge attributes
    cmn.objRemoveDuplicates(edge);

    // Remove duplicates from publications and generate pub/misc tags
    const pubs = cmn.jsonGet(edge, 'publications', {});
    Object.keys(pubs).forEach((kl) => {
      const klPubs = cmn.jsonGet(pubs, kl, []);
      const seenIds = new Set();
      cmn.jsonSet(pubs, kl, klPubs.filter((pub) => {
        const shouldInclude = !seenIds.has(pub.id);
        if (ev.is_publication(pub.id)) {
          taglib.set_tag(edge, _CONSTANTS.TAGS.PATH.EVIDENCE.PUBLICATIONS)
        }
        seenIds.add(pub.id);
        return shouldInclude;
      }));
    });

    // Convert all infores to provenance
    cmn.jsonUpdate(edge, 'provenance', (provenance) => {
      return provenance.map((p) => {
        const provenanceMapping = bl.inforesToProvenance(p);
        if (!provenanceMapping) {
          edgeErrorReasons.push(`Found invalid provenance ${p} on edge ${edgeToString(edge)}`);
        }
        return provenanceMapping;
      }).filter(cmn.identity);
    });

    if (edgeErrorReasons.length !== 0) {
      updateErrorsFromEdge(edge, errors, edgeErrorReasons);
      delete edges[eid];
      return
    }

    edge.knowledge_level = edge.provenance[0].knowledge_level;
    if (edge.hasSupport()) {
      edge.type = trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT;
    } else {
      edge.type = trapi.CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
    }
    // Generate other edge evidence tags
    if (!cmn.is_array_empty(Object.keys(edge.supporting_trials))) {
      taglib.set_tag(edge, _CONSTANTS.TAGS.PATH.EVIDENCE.CLINICAL);
    }
    // TODO: Refine meaning of an ontological edge
    if (ev.is_ontological(edge.predicate)) {
      taglib.set_tag(edge, _CONSTANTS.TAGS.PATH.EVIDENCE.ONTOLOGICAL);
    }
    taglib.set_tag(edge, _CONSTANTS.TAGS.PATH.EVIDENCE.OTHER);
  });

  [edges, pubs, trials] = extractAndFinalizeEdges(edges);

  const meta = new SummaryMetadata(qid, cmn.distinctArray(smryFgmts.map((sf) => {
    return sf.agents;
  }).flat()));

  // Node post-processing
  Object.keys(nodes).forEach((nid) => {
    const node = nodes[nid];
    node.curies.push(nid);
    // Remove any duplicates on all node attributes
    cmn.objRemoveDuplicates(node);
    node.types.sort(bl.biolinkClassCmpFn);

    // Provide a CURIE as a default value if the node has no name
    const node_names = node.names;
    pushIfEmpty(node_names, nid);

    const provenance = bl.curieToNormalizedUrl(nid, node.curies);
    if (!provenance) {
      logger.warn(`WARNING: No provenance for CURIE: ${nid}`);
    } else {
      node.set_provenance(provenance);
    }

    if (bta.is_disease(node)) {
      const curies = node.annotations.disease.curies;
      if (!cmn.is_missing(curies)) {
        node.curies.push(...curies);
      }
    }
    if (bta.is_gene(node)) {
      const tdl = node.annotations.gene.tdl;
      if (!cmn.is_missing(tdl)) {
        taglib.set_tag(node, new taglib.Tag({id: `r/tdl/${tdl.toLowerCase()}`, name: tdl}));
      }
    }
  });

  // Path post-processing
  Object.keys(paths).forEach((pid) => {
    const path = _get_summary_path(pid, paths);
    // Remove paths where there is an undefined node reference in the path
    path.forNids((nid) => {
      if (nodes[nid] === undefined) {
        delete paths[pid];
        return;
      }
    });
    // Remove paths where there is an undefined edge reference in the path
    // Promote edge tags to path tags
    path.forEids((eid) => {
      const edge = edges[eid];
      if (edge === undefined) {
        logger.warn(`${eid} not found`);
        delete paths[pid];
        return;
      }
      taglib.merge_tags({from_obj: edge, to_obj: path});
    });
    // Remove duplicates from every attribute on a path
    cmn.objRemoveDuplicates(path);
    // Generate tags for result nodes
    const path_start = nodes[path.start];
    if (bta.is_chemical(path_start)) {
      const annotations = path_start.annotations.chemical;
      const fda_approval = annotations.approval;
      if (!cmn.is_missing(fda_approval)) {
        if (fda_approval === 4) {
          taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.FDA.APPROVED);
        } else if (fda_approval === 0) {
          taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.FDA.NOT_APPROVED);
        } else {
          taglib.set_tag(path_start,
            new taglib.Tag({
              id: `r/fda/${fda_approval}`,
              name: `Clinical Trial Phase ${fda_approval}`
            }));
        }
      }
      if (!cmn.is_missing(annotations.roles)) {
        for (const role of annotations.roles) {
          taglib.set_tag(path_start,
            new taglib.Tag({id: `r/role/${role.id}`, name: cmn.titleize(role.name)}));
        }
      }
      const otc = annotations.otc_status;
      switch(otc) {
        case 2: taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.OTC.OTC); break;
        case 1: taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.OTC.PRESCRIPTION); break;
        case 0: taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.OTC.DISCONTINUED); break;
        case -2: taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.OTC.WITHDRAWN); break;
        case -1:
        default: taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.OTC.UNKNOWN);
      }
    }
    // Generate tags for chemical indications
    if (trapi.is_chemical_disease_query(query_type)) {
      // Consider the chemical indicated for the disease iff
      //   1. The chemical is marked as indicated for the disease
      //   2. The chemical has reached phase 4 approval from the FDA
      let indications = null;
      if (!cmn.is_missing(path_start.annotations)) {
        indications = path_start.annotations.chemical.indications ?? null;
      }
      let is_path_indicated = false;
      if (!cmn.is_missing(indications)) {
        const indications_set = new Set(indications);
        const path_end = nodes[path.end];
        let path_end_mesh_ids = path_end.curies;
        if (!cmn.is_missing(path_end.annotations)) {
          path_end_mesh_ids = path_end.curies.filter(curie => curie.startsWith("MESH:"));
        }
        for (let i = 0; i < path_end_mesh_ids.length; i++) {
          if (indications_set.has(path_end_mesh_ids[i])) {
            is_path_indicated = taglib.has_tag(path_start, _CONSTANTS.TAGS.RESULT.FDA.APPROVED);
            break;
          }
        }
      }
      if (is_path_indicated) {
        taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.INDICATED.YES);
      } else {
        taglib.set_tag(path_start, _CONSTANTS.TAGS.RESULT.INDICATED.NO);
      }
    }
    // Add tags for paths by processing nodes
    taglib.merge_tags({
      from_obj: nodes[path.start],
      to_obj: path,
      selector: (tag) => {return _is_result_tag(tag) && _is_external_tag(tag)}
    });
    path.forInternalNids((nid) => {
      const node = nodes[nid];
      taglib.merge_tags({
        from_obj: node,
        to_obj: path,
        selector: (tag) => {return !_is_result_tag(tag) && _is_external_tag(tag)}
      });
      const node_type = node.get_specific_type();
      const tag_node_type = new taglib.Tag({id: `p/pc/${node_type}`, name: node_type});
      taglib.set_tag(path, tag_node_type);
    });
    // Generate a special tag for the answer node
    const tag_max_phase = _gen_max_phase_tag(nodes[path.start], query_type);
    if (tag_max_phase) {
      taglib.set_tag(path, tag_max_phase);
    }
    // Generate tags based on the aras for this path
    const agent_infores = path.agents;
    agent_infores.forEach((infores) => {
      const tag_ara = new taglib.Tag({id: `r/ara/${infores}`, name: inforesToName(infores)});
      taglib.set_tag(path, tag_ara);
    });
    // Generate tags for number of connections
    const edge_count = path.edgeCount
    let tag_length_description = 'Connections';
    if (edge_count === 1) {
      tag_length_description = 'Connection';
    }
    const tag_length = new taglib.Tag({id: `p/pt/${edge_count}`, name: `${edge_count} ${tag_length_description}`});
    taglib.set_tag(path, tag_length);
  });

  Object.keys(edges).forEach((eid) => {
    const edge = cmn.jsonGet(edges, eid);
    edge.supPaths = edge.supPaths.filter(pid => paths[pid] !== undefined);
  });

  results.forEach((res) => {
    res.paths = res.paths.filter(pid => paths[pid] !== undefined);
    res.paths = genSupChain(res.paths, paths, edges);
  });

  cleanup(results, paths, edges, nodes);
  [results, tags] = __gen_results_and_tags_from_results(results, paths, nodes,
      edges, scores, errors, query_type);
  Object.keys(edges).forEach((eid) => {
    const edge = cmn.jsonGet(edges, eid);
    edge.supPaths = _pid_sort(edge.supPaths, paths);
  });

  return new Summary(meta, results, paths, nodes, edges, pubs, trials, tags, errors);
}

function getResultAnalyses(result) {
  return cmn.jsonGet(result, 'analyses');
}

function getResultScoringComponents(result) {
  const scoringComponents = cmn.jsonGet(
    result,
    'ordering_components',
    {confidence: 0, novelty: 0, clinical_evidence: 0}
  );

  const normalizedScore = cmn.jsonGet(result, 'normalized_score', 0);
  cmn.jsonSet(scoringComponents, 'normalized_score', normalizedScore);
  return scoringComponents;
}

function getResultNormalizedScore(result) {
  return cmn.jsonGet(result, 'normalized_score', 0);
}

class NodeBindingNotFoundError extends Error {
  constructor(edgeBinding) {
    super(`Node binding not found for ${JSON.stringify(edgeBinding)}`);
  }
}

class AuxGraphNotFoundError extends Error {
  constructor(auxGraph) {
    super(`Auxiliary graph not found for ${auxGraph}`);
  }
}

const _CONSTANTS = Object.freeze({
  TAGS: {
    PATH: {
      EVIDENCE: {
        CLINICAL: new taglib.Tag({id: 'p/ev/clinical', name: 'Clinical Trials'}),
        PUBLICATIONS: new taglib.Tag({id: 'p/ev/publications', name: 'Publications'}),
        ONTOLOGICAL: new taglib.Tag({id: 'p/ev/ontology', name: 'Accepted Ontology'}),
        OTHER: new taglib.Tag({id: 'p/ev/other', name: 'Other'})
      },
      TYPE: {
        DIRECT: new taglib.Tag({id: 'p/pt/lkup', name: 'Direct'}),
        INDIRECT: new taglib.Tag({id: 'p/pt/inf', name: 'Indirect'})
      }
    },
    RESULT: {
      CHEMICAL_CLASS: {
        DRUG: new taglib.Tag({id: 'r/cc/drug', name: 'Drug'}),
        OTHER: new taglib.Tag({id: 'r/cc/other', name: 'Other'})
      },
      FDA: {
        APPROVED: new taglib.Tag({id: `r/fda/4`, name: `FDA Approved`}),
        NOT_APPROVED : new taglib.Tag({id: 'r/fda/0', name: 'Not FDA Approved'})
      },
      INDICATED: {
        YES: new taglib.Tag({id: 'r/di/ind', name: 'In a clinical trial for indicated disease'}),
        NO: new taglib.Tag({id: 'r/di/not', name: 'Not in a clinical trial for indicated disease'})
      },
      OTC: {
        OTC: new taglib.Tag({id: "r/otc/t", name: "Over the counter"}),
        PRESCRIPTION: new taglib.Tag({id: "r/otc/f", name: "Prescription only"}),
        DISCONTINUED: new taglib.Tag({id: "r/otc/d", name: "Discontinued"}),
        WITHDRAWN: new taglib.Tag({id: "r/otc/w", name: "Withdrawn"}),
        UNKNOWN: new taglib.Tag({id: "r/otc/o", name: "Other"})
      }
    }
  }
});
