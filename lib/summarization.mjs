'use strict';

import { default as hash } from 'hash-sum';
import * as cmn from './common.mjs';
import * as ev from './evidence.mjs';
import * as bl from './biolink-model.mjs';
import * as bta from './biothings-annotation.mjs';
import * as trapi from './trapi.mjs';

const CONSTANTS = {
  PUBLICATIONS: 'publications',
  SUPPORTING_TEXT: 'supporting_text',
  SUPPORTING_TRIALS: 'supporting_trials',
  DIRECT_EDGE: 'direct',
  INDIRECT_EDGE: 'indirect',
  TAGS: 'tags'
};

/**
 * Responsible for converting a set of TRAPI answers into a summarized form that the FE application can use.
 *
 * @param {string}   qid - The query ID for the given answer set.
 * @param {object[]} answers - The set of TRAPI answers to summarize.
 * @param {number}   max_hops - The maximum number of hops to consider when summarizing the answers.
 *
 * @returns {object} - The summarized form of the answers.
 */
export function answersToSmry (qid, answers, max_hops) {
  if (answers.length < 1) {
    return {};
  }

  const nodeRules = makeExtractionRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAndTransformAttributes([bl.tagBiolink('xref')], 'curies', (xref) => {
        return bl.isValidCurie(xref) ? xref : [];
      }),
      aggregateAttributes([bl.tagBiolink('description')], 'descriptions'),
    ]);

  const edgeRules = makeExtractionRules(
    [
      transformPropertyRule(
        'predicate',
        (obj, key) => bl.sanitizeBiolinkItem(cmn.jsonGet(obj, key))),
      aggregateAndTransformProperty(
        trapi.CONSTANTS.GRAPH.SOURCES.KEY,
        ['provenance'],
        (obj, key) => trapi.getPrimarySrc(obj)),
      transformPropertyRule('qualifiers', (obj, key) => cmn.jsonGet(obj, key, false)),
      getPropertyRule('subject'),
      getPropertyRule('object'),
      getPubs(),
      getSupText(),
      getSupTrials()
    ]);

  const queryType = answersToQueryTemplate(answers);
  const [sfs, errors] = answersToSmryFgmts(answers, nodeRules, edgeRules, max_hops);
  const smry = smryFgmtsToSmry(
    qid,
    sfs,
    answersToKgraph(answers),
    queryType,
    errors);
  return smry;
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
    return cmn.isArrayEmpty(this.paths) &&
           cmn.isArrayEmpty(this.nodes) &&
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

function makeErrSmryFgmt(agent, error) {
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
    return cmn.isObjectEmpty(this.base) && cmn.isArrayEmpty(this.updates);
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

class SummaryNode {
  constructor(agents) {
    this.aras = agents || [];
    this.curies = [];
    this.descriptions = [];
    this.names = [];
    this.other_names = [];
    this.provenance = []
    this.tags = {}
    this.types = [];
  }

  name() { return (cmn.isArrayEmpty(this.names) ? this.curies[0] : this.names[0]); }
  get otherNames () { return this.other_names; }
  set otherNames (otherNames) { this.other_names = otherNames; }

  get type() {
    // TODO: We should inject 'Named Thing' as a type if its empty
    if (cmn.isArrayEmpty(this.types)) {
      return 'Named Thing'; // TODO: Should be a biolink constant
    }

    return bl.sanitizeBiolinkItem(this.types[0]);
  }

  extendAgents(agents) {
    this.aras.concat(agents)
  }
}

class SummaryEdge {
  constructor(agents, supPaths, isRootPath) {
    this.aras = agents || [];
    this.support = supPaths || [];
    this.is_root = isRootPath || false;
    this.knowledge_level = null;
    this.type = null;
    this.subject = null;
    this.object = null;
    this.predicate = null;
    this.predicate_url = null;
    this.provenance = [];
    this.publications = {};
    this.trials = [];
  }

  // TODO: Should this be a property of the path instead of the edge?
  get isRootPath() { return this.is_root; }
  set isRootPath(isRootPath) { this.is_root = isRootPath; }
  get supPaths() { return this.support; }
  set supPaths(supPaths) { this.support = supPaths; }
  get knowledgeLevel() { return this.knowledge_level; }
  set knowledgeLevel(kl) { this.knowledge_level = kl; }
  get predUrl() { return this.predicate_url; }
  set predUrl(predUrl) { this.predicate_url = predUrl; }

  hasSupport() { return !cmn.isArrayEmpty(this.supPaths); }
  extendAgents(agents) {
    this.aras.concat(agents)
  }

  extendSupPaths(paths) {
    this.supPaths.push(...paths);
  }

  merge(smryEdge) {
    this.extendAgents(smryEdge.aras);
    this.extendSupPaths(smryEdge.supPaths);
    this.isRootPath = this.isRootPath || smryEdge.isRootPath;
    this.knowledgeLevel = this.knowledgeLevel || smryEdge.knowledgeLevel;
    this.type = this.type || smryEdge.type;
    // Change the type of the edge if we get support graphs
    if (smryEdge.type === CONSTANTS.INDIRECT_EDGE) {
      this.type = smryEdge.type;
    }
    this.subject = this.subject || smryEdge.subject;
    this.predicate = this.predicate || smryEdge.predicate;
    this.object = this.object || smryEdge.object;
    this.predUrl = this.predUrl || smryEdge.predUrl;
    this.provenance.push(...smryEdge.provenance);
    this.trials.push(...smryEdge.trials);
    Object.keys(smryEdge.publications).forEach((kl) => {
      if (!this.publications[kl]) {
        this.publications[kl] = [];
      }

      this.publications[kl].push(...smryEdge.publications[kl]);
    });

    return this
  }
}

class SummaryPath {
  constructor(subgraph, agents) {
    this.subgraph = subgraph;
    this.aras = agents;
    this.tags = {};
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

function smryPathCompare(smryPath1, smryPath2) {
  const len1 = smryPath1.length;
  const len2 = smryPath2.length;
  if (len1 === len2) {
    for (let i = 0; i < smryPath1.nodeCount; i++) {
      if (smryPath1.nid(i) < smryPath2.nid(i)) return -1;
      else if (smryPath1.nid(i) > smryPath2.nid(i)) return 1;
    }

    return 0;
  }

  if (len1 < len2) return -1;
  return 1;
}

class SummaryMetadata {
  constructor(qid, agents) {
    if (qid === undefined || !cmn.isString(qid)) {
      throw new TypeError(`Expected argument qid to be of type string, got: ${qid}`);
    }

    if (agents === undefined || !cmn.isArray(agents)) {
      throw new TypeError(`Expected argument agents to be type array, got: ${agents}`);
    }

    this.qid = qid;
    this.aras = agents;
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
function answersToQueryTemplate(answers) {
  // TODO: A more robust solution might be possible but all answers should have the same query type.
  return trapi.messageToQueryTemplate(answers[0]); // This assumes a fully merged TRAPI message
}

function answersToKgraph(answers) {
  return trapi.getKgraph(answers[0]); // This assumes a fully merged TRAPI message
}

function inforesToName(infores) {
  return bl.inforesToProvenance(infores).name;
}

/* Constructs a rule on how to extract a property from a source object. There are 3 different stages to an extraction rule:
 * 1. Definition: This is what this function does.
 * 2. Application: The rule can by applied to source object with some context which will produce a transformer function.
 * 3. Transformation: Once the target object is known, the actual transformation can be applied to modify the target object.
 *
 * @param {string} key - The key to extract from an object.
 * @param {function} transform - The transformation to apply to the extracted value.
 * @param {function} update - How to update the accumulator with the extracted value.
 * @param {object} defaultValue - The default value to use if the extraction fails.
 *
 * @returns {function} - The extraction rule.
 */
function makeExtractionRule(key, transform, update, defaultValue) {
  return (src, cxt) => {
    return (target) => {
      try {
        const v = transform(src, key, cxt);
        return update(v, target);
      } catch (e) {
        const agentErrors = cmn.jsonSetDefaultAndGet(cxt.errors, cxt.agent, []);
        agentErrors.push(e.message);
        return update(defaultValue, target);
      }
    }
  }
}

/* Constructs a rule for extracting a property from a Graph Element, transforming it, and placing it in the accumuator using the same key.
 *
 * @param {string} key - The key to extract from a Graph Element and place into the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function transformPropertyRule(key, transform) {
  return makeExtractionRule(
    key,
    transform,
    (property, acc) => { return cmn.jsonSetFromKpath(acc, [key], property); },
    null);
}

/* Constructs a rule for extracting a property from a Graph Element and placing it in the accumulator using the same key.
 *
 * @param {string} key - The key to extract from a Graph Element and place into the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function getPropertyRule(key) {
  return transformPropertyRule(key, (obj, key) => cmn.jsonGet(obj, key));
}

/* Constructs a rule for extracting a property from a Graph Element, transforming it, and aggregating it in the accumulator using the same key.
 *
 * @param {string} key - The key to extract from a Graph Element and aggregate in the accumulator.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function aggregateAndTransformProperty(key, kpath, transform) {
  return makeExtractionRule(
    key,
    transform,
    (property, acc) => {
      const currentValue = cmn.jsonSetDefaultAndGet(acc, kpath, []);
      if (property !== null) {
        currentValue.push(...cmn.coerceArray(property));
      }

      return acc;
    },
    []);
}

/* Constructs a rule for extracting a property from a Graph Element and aggregating it in the accumulator.
 *
 * @param {string} key - The key to extract from a Graph Element and aggregate in the accumulator.
 * @param {string[]} kpath - The path to the property in the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function aggregateProperty(key, kpath) {
  return aggregateAndTransformProperty(key, kpath, (obj) => { return cmn.jsonGet(obj, key); });
}

/* Constructs a rule for renaming and transforming an attribute from a Graph Element and placing it into an accumulator.
 *
 * @param {string} attributeId - The Type ID of the attribute to rename and transform.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function renameAndTransformAttribute(attrId, kpath, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const attrVal = attrIter.findOneVal([attrId]);
      if (attrVal === null) return null;
      return transform(attrVal);
    },
    (attr, acc) => {
      const currentValue = cmn.jsonGetFromKpath(acc, kpath, false);
      if (currentValue && attr === null) {
        return acc;
      }

      return cmn.jsonSetFromKpath(acc, kpath, attr);
    },
    null);
}

/* Constructs a rule for aggregating and transforming attributes from a Graph Element and placing them into an accumulator.
 *
 * @param {string[]} attributeIds - The Type IDs of the attributes to aggregate and transform.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function aggregateAndTransformAttributes(attrIds, accKey, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, cxt) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const result = attrIter.findAllVal(attrIds);
      return result.map((v) => { return transform(v, cxt); }).flat();
    },
    (attrs, acc) => {
      const currentValue = cmn.jsonSetDefaultAndGet(acc, accKey, []);
      currentValue.push(...attrs);
      return acc;
    },
    []);
}

/* Constructs a rule for aggregating an attribute from a Graph Element and placing it into an accumulator.
 *
 * @param {string[]} attributeIds - The Type IDs of the attributes to aggregate.
 * @param {string} accKey - The key to aggregate the attributes under in the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function aggregateAttributes(attrIds, accKey) {
  return aggregateAndTransformAttributes(
    attrIds,
    accKey,
    cmn.identity)
}

/* A special rule used to generate tags used for faceting from attributes.
 *
 * @param {string} attributeId - The Type ID of the attribute to generate tags from.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function tagAttribute(attrId, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, cxt) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const result = attrIter.findOneVal([attrId]);
      return transform(result, cxt);
    },
    (tags, acc) => {
      if (!tags) return acc;
      const currentTags = getTags(acc);
      tags.forEach((tag) => {
        if (tag && currentTags[tag.label] === undefined) {
          currentTags[tag.label] = tag.description;
        }
      });

      return acc;
    },
    null);
}

/* A special rule for extracting supporting text for a publication.
 *
 * @returns {function} - The extraction rule.
 */
function getSupText() {
  const supStudyId = bl.tagBiolink('has_supporting_study_result');
  const pubsId = bl.tagBiolink('publications');
  const textId = bl.tagBiolink('supporting_text');
  const subTokenId = bl.tagBiolink('subject_location_in_text');
  const objTokenId = bl.tagBiolink('object_location_in_text');

  function parseTokenIndex(token) {
    const range = token.split('|').map(t => parseInt(t.trim()));
    range[0] = range[0] + 1;
    return range;
  }

  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const supTextEntries = attrIter.findAll([supStudyId]);
      const supText = {};
      supTextEntries.forEach(supTextEntry => {
        const entryAttrs = trapi.getAttrs(supTextEntry);
        const supTextData = {};
        entryAttrs.forEach(attr => {
          const aid = trapi.getAttrId(attr);
          const av = trapi.getAttrVal(attr);
          switch (aid) {
            case pubsId:
              supText[ev.sanitize(av)] = supTextData; break;
            case textId:
              supTextData.text = av; break;
            case subTokenId:
              supTextData.subject = parseTokenIndex(av); break;
            case objTokenId:
              supTextData.object = parseTokenIndex(av); break;
          }
        });
      });

      return supText;
    },
    (supText, acc) =>
    {
      const currentSupText = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.SUPPORTING_TEXT, {});
      Object.keys(supText).forEach((pid) => {
        currentSupText[pid] = supText[pid];
      });
    },
    {});
}

function getSupTrials() {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const supTrials = attrIter.findAll([bl.tagBiolink('supporting_study')]);
      const extractedTrials = {};
      for (let i = 0; i < supTrials.length; i++) {
        const supTrial = supTrials[i];
        const supTrialId = trapi.getAttrVal(supTrial);
        if (extractedTrials[supTrialId]) {
          continue;
        }
        extractedTrials[supTrialId] = {};
        const extractedTrial = extractedTrials[supTrialId];
        const supTrialAttrs = trapi.getAttrs(supTrial);
        for (let j = 0; j < supTrialAttrs.length; j++) {
          const attr = supTrialAttrs[j];
          switch(trapi.getAttrId(attr)) {
            case 'clinical_trial_phase':
              extractedTrial.phase = trapi.getAttrVal(attr); break;
            case 'clinical_trial_status':
              extractedTrial.status = trapi.getAttrVal(attr); break;
            case 'child':
              extractedTrial.child = trapi.getAttrVal(attr); break;
            case 'start_date':
              extractedTrial.start_date = trapi.getAttrVal(attr); break;
            case 'study_size':
              extractedTrial.size = trapi.getAttrVal(attr); break;
            case 'enrollment_type':
              const type = trapi.getAttrVal(attr);
              extractedTrial.type = type === 'ACTUAL' ? 'enrolled' : 'anticipated';
              break;
            default:
              break;
          }
        }
      }
      return extractedTrials;
    },
    (supTrials, acc) => {
      const curSupTrials = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.SUPPORTING_TRIALS, {});
      Object.keys(supTrials).forEach(supTrialId => {
        if (curSupTrials[supTrialId] !== undefined) {
          return;
        }
        curSupTrials[supTrialId] = supTrials[supTrialId];
      });
      return acc;
    },
    {});
}

/* A special rule for extracting publications from attributes.
 *
 * @returns {function} - The extraction rule.
 */
function getPubs() {
  const pubIds = [
      bl.tagBiolink('supporting_document'),
      bl.tagBiolink('Publication'),
      bl.tagBiolink('publications'),
      bl.tagBiolink('publication') // Remove me when this is fixed in the ARA/KPs
  ];

  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, cxt) => {
      const pubs = {};
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const pids = attrIter.findAllVal(pubIds);
      if (cmn.isArrayEmpty(pids)) return pubs;
      const provenance = bl.inforesToProvenance(cxt.primarySrc);
      const kl = getKlevel(obj, provenance);
      if (!pubs[kl]) {
        pubs[kl] = [];
      }

      pids.forEach((pid) => {
        pubs[kl].push({
          id: ev.sanitize(pid),
          src: provenance
        });
      });

      return pubs;
    },
    (pubs, acc) => {
      const currentPubs = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.PUBLICATIONS, {});
      Object.keys(pubs).forEach((kl) => {
        if (!currentPubs[kl]) {
          currentPubs[kl] = [];
        }

        currentPubs[kl].push(...(pubs[kl]));
      });

      return acc;
    },
    {}
  );
}

/**
 * Generates a function to extract attributes or properties from a TRAPI object given a set of rules.
 *
 * @param {object[]} rules - The set of rules to use for extracting attributes.
 *
 * @returns {function} - A rule that will apply a list of rules to a source object and produce a list of transformers.
 */
function makeExtractionRules(rules) {
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
function getResultStartAndEnd(result, startKey, endKey) {
  // Flatten the node bindings for a specific key of a result into a list of IDs
  function flattenBinding(result, key) {
    const nodeBinding = trapi.getNodeBinding(result, key);
    if (cmn.isArrayEmpty(nodeBinding)) {
      throw new NodeBindingNotFoundError(nodeBinding);
    }

    // TODO: move node binding getters to trapi.mjs
    return nodeBinding.map((entry) => {
      const endpoint = cmn.jsonGet(entry, 'id');
      return endpoint;
    });
  }

  const rnodeStart = flattenBinding(result, startKey)[0];
  const rnodeEnds = flattenBinding(result, endKey); // There can be multiple endpoints
  return [rnodeStart, rnodeEnds];
}

/**
 * Flatten all bindings into a list of binding IDs.
 *
 * @mapping
 * Analysis Edge Bindings ===> Knowledge Graph Edge Bindings
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
 * @param {object} bindings - The bindings to flatten.
 *
 * @returns {string[]} - The list of binding IDs.
 */
function flatten_bindings(qg_bindings) {
  return Object.values(qg_bindings).reduce((kg_bindings, qg_binding) => {
    return kg_bindings.concat(qg_binding.map(qg_kg_mapping => {
      return cmn.jsonGet(qg_kg_mapping, 'id');
    }));
  },
  []);
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
function getKlevel(kedge, provenance) {
  const agentType = trapi.getAgentType(kedge);
  if (agentType === 'text_mining_agent') {
    return 'ml';
  }

  const kl = trapi.getKlevel(kedge);
  if (kl === 'knowledge_assertion') {
    return 'trusted';
  } else if (kl === 'not_provided') {
    return 'unknown';
  } else if (kl !== null) {
    return 'inferred';
  }

  return provenance.knowledge_level;
}

function get_edge_type(kedge) {
  const supGraphs = trapi.getSupGraphs(kedge);
  if (cmn.isArrayEmpty(supGraphs)) return CONSTANTS.DIRECT_EDGE;
  return CONSTANTS.INDIRECT_EDGE;
}

/**
 * Convert the array of qualifiers of a knowledge edge into an object.
 *
 * @param {object} kedge - The knowledge edge to extract qualifiers from.
 *
 * @returns {object} - The qualifiers of the knowledge edge or null if there is an error.
 */
function getQualifiers(kedge) {
  try {
    const trapiQualifiers = trapi.getQualifiers(kedge);
    const qualifiers = {};
    trapiQualifiers.forEach((qualifier) => {
      const qualifierId = bl.sanitizeBiolinkItem(trapi.getQualifierId(qualifier));
      const qualifierVal = bl.sanitizeBiolinkItem(trapi.getQualifierVal(qualifier));
      qualifiers[qualifierId] = qualifierVal;
    });

    return qualifiers;
  } catch (err) {
    //console.error(err);
    return null;
  }
}

/**
 * Get the most specific predicate available from a kedge.
 *
 * @param {object} kedge - The knowledge edge to extract the predicate from.
 *
 * @returns {string} - The most specific predicate available.
 */
// TODO: Add biolink constants for qualifier keys
function getMostSpecificPred(kedge) {
  const qualifiers = getQualifiers(kedge);
  if (!qualifiers) {
    return trapi.getPred(kedge);
  }

  return qualifiers['qualified predicate'] || trapi.getPred(kedge);
}

/**
 * Generates the qualified predicate for a kedge.
 *
 * @param {object} kedge - The knowledge edge to generate the qualified predicate for.
 * @param {boolean} invert - Whether or not to invert the predicate.
 *
 * @returns {string} - The qualified predicate.
 */
function genQualifiedPred(kedge, invert = false) {
  function genQualification(type, qualifiers, prefixes) {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // TODO: Add biolink constants for qualifiers
    // The ordering of qualifierKeys impacts the final qualified predicate
    const qualifierKeys = ['form or variant', 'direction', 'aspect', 'part', 'derivative'];
    const qualifierValues = qualifierKeys
      .map(key => cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false))

    let qualification = '';
    qualifierValues.forEach((qv, i) => {
      if (qv) {
        if (qualification) {
          qualification += ' '
        }

        if (prefixes[i]) {
          qualification += `${prefixes[i]} `;
        }

        qualification += qv;
      }
    });

    return qualification;
  }

  function genSubQualification(qualifiers, directionPrefix = false) {
    return genQualification('subject', qualifiers, directionPrefix);
  }

  function genObjQualification(qualifiers, directionPrefix = false) {
    return genQualification('object', qualifiers, directionPrefix);
  }

  function combinePredParts(prefix, pred, suffix) {
    if (prefix) {
      prefix += ' ';
    }

    if (suffix) {
      suffix = ` ${suffix} of`;
    }

    const qualifiedPred = `${prefix}${pred}${suffix}`;
    return qualifiedPred;
  }

  function genSpecialPred(pred, qualifiers, invert) {
    const objDirectionQualifier = qualifiers['object direction qualifier'];
    if (pred === 'regulates' &&
          (objDirectionQualifier === 'upregulated' ||
           objDirectionQualifier === 'downregulated')) {
      if (invert) {
        return `is ${objDirectionQualifier} by`;
      }

      return objDirectionQualifier.replace('ed', 'es');
    }

    return false;
  }

  let pred = bl.sanitizeBiolinkItem(trapi.getPred(kedge));
  let qualifiers = getQualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPred(pred)) {
    [pred, qualifiers] = bl.deprecatedPredToPredAndQualifiers(pred);
  }

  // If we don't have any qualifiers, treat it like biolink v2
  if (!qualifiers) {
    if (invert) {
      pred = bl.invertBiolinkPred(pred);
    }

    return pred;
  }

  pred = bl.sanitizeBiolinkItem(getMostSpecificPred(kedge));
  const specialCase = genSpecialPred(pred, qualifiers, invert);
  if (specialCase) {
    return specialCase;
  }

  const subPrefixes = ['of a', 'has', false, 'of the', false];
  const objPrefixes = ['a', false, false, 'of the', false];
  if (invert) {
    const subQualification = genSubQualification(qualifiers, objPrefixes);
    const objQualification = genObjQualification(qualifiers, subPrefixes);
    return combinePredParts(objQualification, bl.invertBiolinkPred(pred), subQualification);
  }

  const subQualification = genSubQualification(qualifiers, subPrefixes);
  const objQualification = genObjQualification(qualifiers, objPrefixes);
  return combinePredParts(subQualification, pred, objQualification);
}

/**
 * Generate a tag for a any graph summarization object
 *
 * @param {string} label - Label for the tag used for faceting.
 * @param {string} name - User facing name for the tag.
 * @param {string} description - User facing description for the tag.
 *
 * @returns {object} - The tag object.
 */
function makeTag(label, name, description = '') {
  return {
    'label': label,
    'description': makeTagDescription(name, description)
  };
}

/**
 * Generate a user facing description for a tag.
 *
 * @param {string} name - User facing name of the tag.
 * @param {string} description - User facing description of the tag.
 *
 * @returns {object} - The tag description object.
 */
function makeTagDescription(name, description = '') {
  return {
    'name': name,
    'value': description
  };
}

function getTags(graphElem) {
  // TODO: Throw an error if the tags are not found
  return graphElem.tags;
}

/**
 * Add a tag to a summary element.
 *
 * @param {object} summaryElement - Summary element to add the tag to.
 * @param {object} tag - Tag to add to the summary element.
 *
 * @returns {object} - The summary element with the added tag.
 */
function addTag(smryElem, tag) {
  smryElem.tags[tag.label] = tag.description;
  return smryElem;
}

/**
 * Merge the tags of a source summary element into a target summary element with an optional filter for the tags of the source summary element.
 *
 * @param {object} smryTgt - Summary element to add the tag to.
 * @param {object} smrySrc - Summary element to add the tag to.
 * @param {function} filterFunc - Optional filter for the source summary element.
 *
 * @returns {object} - The summary target.
 */
function mergeTags(smryTgt, smrySrc, filterFunc = null) {
  let srcTags = Object.keys(smrySrc.tags);
  if (filterFunc !== null) {
    srcTags = srcTags.filter(filterFunc);
  }

  srcTags.forEach((k) => smryTgt.tags[k] = smrySrc.tags[k]);
  return smryTgt;
}

function getTagFamily(tag) {
  return tag.split('/')[1];
}

function isResultTag(tag) {
  return tag.startsWith('r/');
}

function isPathTag(tag) {
  return tag.startsWith('p/');
}

function isExternalTag(tag) {
  const validFamilies = ['cc', 'di', 'pc', 'pt', 'role', 'ara', 'otc', 'tdl'];
  const family = getTagFamily(tag);
  return validFamilies.includes(family);
}

function isFdaTag(tag) {
  return tag.startsWith('r/fda');
}

function genMaxPhaseTag(node, queryType) {
  function isDrug(node, fdaLevel) {
    return fdaLevel === 4 || node.type === 'Drug';
  }

  function isClinicalPhase(fdaLevel) {
    return fdaLevel > 0 && fdaLevel < 4;
  }

  // Only generate this tag for non-gene/chemical queries
  if (!trapi.isValidQuery(queryType) || trapi.isGeneChemicalQuery(queryType)) {
    return false;
  }

  const fdaTags = Object.keys(getTags(node)).filter(isFdaTag);
  let highestFdaApproval = 0;
  if (!cmn.isArrayEmpty(fdaTags)) {
    highestFdaApproval = Math.max(...fdaTags.map((tag) => { return parseInt(tag.split('/')[2]); }));
  }

  if (highestFdaApproval === 0) return makeTag('r/cc/other', 'Other');
  if (isDrug(node, highestFdaApproval)) return makeTag('r/cc/drug', 'Drug');
  if (isClinicalPhase(highestFdaApproval)) return makeTag(`r/cc/phase${highestFdaApproval}`, `Phase ${highestFdaApproval} Drug`);
  return makeTag(`r/cc/other`, `Other`);
}

function gen_rgraph(node_bindings, edge_topology, kgraph) {
  for (const nb of node_bindings) {
    if (!trapi.hasKnode(nb, kgraph)) return false;
  }
  const rgraph = {};
  rgraph.nodes = node_bindings;
  const edge_bindings = Object.keys(edge_topology);
  if (!edge_bindings) return false;
  rgraph.edges = edge_bindings.filter(eb => {
    const kedge = trapi.getKedge(eb, kgraph);
    // TODO: It would be proper to also delete the edge from edge_topology. The implementation is very complicated because of the cascading effect it could have.
    return bl.isBiolinkPred(trapi.getPred(kedge));
  });
  rgraph.edge_topology = edge_topology;
  return rgraph;
}

function isRedgeInverted(redge, sub, kgraph) {
  const kedge = trapi.getKedge(redge, kgraph);
  return sub === trapi.getObj(kedge);
}

// @mapping
// Analysis ===> Result Graph
// {
//   edge_binding: {
//     <qg_edge_binding>: [ { id: <kg_edge_binding> }, ... ],
//     ...
//   }
// }
// ===>
// {
//   nodes: [<kg_node_binding, ...],
//   edges: [<kg_edge_binding, ...],
//   edge_topology: {
//     <kg_edge_binding>: {
//       embedded_in: [<aux_graph_binding>, ...],
//       supporting_graphs: [<aux_graph_binding>, ...]
//     },
//     ...
//   }
// }
function analysis_to_rgraph(analysis, kgraph, aux_graphs) {
  const edge_topology = new Map(); // This doubles as the set of processed edge bindings
  const unprocessed_edge_bindings = flatten_bindings(trapi.getEdgeBindings(analysis)).map((eb) => {
    edge_topology[eb] = { embedded_in: ['root'] };
    return eb;
  });

  const unprocessed_sup_graphs = [];
  const node_bindings = new Set();
  const processed_sup_graphs = new Set();
  // Invariant: edges and subgraphs will only ever be processed once.
  while (!cmn.isArrayEmpty(unprocessed_edge_bindings) || !cmn.isArrayEmpty(unprocessed_sup_graphs)) {
    while (!cmn.isArrayEmpty(unprocessed_edge_bindings)) {
      const eb = unprocessed_edge_bindings.pop();
      if (edge_topology[eb].supporting_graphs !== undefined) continue; // Skip the edge binding if we've processed it already
      const kedge = trapi.getKedge(eb, kgraph);
      if (!kedge) throw new EdgeBindingNotFoundError(eb);
      node_bindings.add(trapi.getSub(kedge));
      node_bindings.add(trapi.getObj(kedge));
      const edge_sup_graphs = trapi.getSupGraphs(kedge);
      edge_topology[eb].supporting_graphs = edge_sup_graphs;
      // TODO: replace this check with a configurable version
      if (!cmn.isArrayEmpty(edge_sup_graphs) && bl.sanitizeBiolinkItem(trapi.getPred(kedge)) === 'related to') {
        // What we are doing here is replacing an edge with the graphs that support it. This is done by:
        const edge_hosts = [...edge_topology[eb].embedded_in];
        // 1. Ensuring the removed edge is not embedded anywhere
        edge_topology[eb].embedded_in= [];
        // 2. Ensuring all children edges of the removed edge are embedded where the removed edge was embedded
        // This has to be done in a roundabout manner via edge_hosts because we haven't processed the support graphs yet
        edge_sup_graphs.forEach((gid) => {
          if (!processed_sup_graphs.has(gid)) {
            unprocessed_sup_graphs.push([gid, edge_hosts]);
          }
        });
      } else {
        edge_sup_graphs.forEach((gid) => {
          if (!processed_sup_graphs.has(gid)) {
            const edge_hosts = [gid]; // The edge host for the edges of this support graph is just the support graph itself
            unprocessed_sup_graphs.push([gid, edge_hosts]);
          }
        });
      }
    };

    while (!cmn.isArrayEmpty(unprocessed_sup_graphs)) {
      const [gid, edge_hosts] = unprocessed_sup_graphs.pop();
      if (processed_sup_graphs.has(gid)) continue;
      const aux_graph = trapi.getAuxGraph(gid, aux_graphs);
      if (!aux_graph) throw new AuxGraphNotFoundError(gid);
      trapi.getAuxGraphEdges(aux_graph).forEach((eb) => {
        if (edge_topology[eb] === undefined) {
          edge_topology[eb] = { embedded_in: edge_hosts };
          unprocessed_edge_bindings.push(eb);
        } else {
          // We do not want to process the same edge twice, but we need to include this
          // graph as a graph where this edge occurs.
          edge_topology[eb].embedded_in.concat(edge_hosts);
        }
      });

      processed_sup_graphs.add(gid);
    }
  }

  return gen_rgraph(
    [...node_bindings],
    edge_topology,
    kgraph
  );
}

function genNid(rnode, kgraph) {
  return rnode;
}

function genEid(redge, kgraph, doInvert = false) {
  const kedge = trapi.getKedge(redge, kgraph);
  const sub = trapi.getSub(kedge);
  const pred = genQualifiedPred(kedge, doInvert);
  const obj = trapi.getObj(kedge);
  const edge_type = get_edge_type(kedge);
  if (doInvert) {
    return genPid([obj, pred, sub, edge_type]);
  }

  return genPid([sub, pred, obj, edge_type]);
}

function summarizeRnode(rnode, kgraph, nodeRules, cxt) {
  const nid = genNid(rnode, kgraph);
  return cmn.make_pair('id',         nid,
                       'transforms', nodeRules(trapi.getKnode(rnode, kgraph), cxt));
}

function summarizeRedge(redge, kgraph, edgeRules, cxt, edgeBaseIds) {
  let eid = genEid(redge, kgraph);
  if (!edgeBaseIds.has(eid)) {
    eid = genEid(redge, kgraph, true);
  }

  return cmn.make_pair('id',         eid,
                       'transforms', edgeRules(trapi.getKedge(redge, kgraph), cxt));
}

function make_map_redge_sopair(rgraph, kgraph) {
  const map_redge_sopair = new Map();
  for (const redge of rgraph.edges) {
    const kedge = trapi.getKedge(redge, kgraph);
    map_redge_sopair.set(redge, make_sopair(trapi.getSub(kedge), trapi.getObj(kedge)));
  }
  return (redge) => { return map_redge_sopair.get(redge); };

  function make_sopair(sub, obj) {
    return cmn.make_pair('sub', sub, 'obj', obj);
  }
}

function make_map_rnode_outedges(rgraph, kgraph) {
  const map_redge_sopair = make_map_redge_sopair(rgraph, kgraph);
  const map_rnode_outedges = new Map();
  for (const redge of rgraph.edges) {
    const { sub, obj } = map_redge_sopair(redge);
    add_rnode_outedge_mapping(map_rnode_outedges, sub, make_outedge(redge, obj));
    add_rnode_outedge_mapping(map_rnode_outedges, obj, make_outedge(redge, sub));
  }
  return (rnode) => { return map_rnode_outedges.get(rnode); };

  function make_outedge(redge, node) {
    return cmn.make_pair('redge',  redge, 'target', node);
  }
  function add_rnode_outedge_mapping(map, rnode, outedge) {
    let outedges = map.get(rnode);
    if (outedges === undefined) {
      outedges = [];
      map.set(rnode, outedges);
    }
    outedges.push(outedge)
  }
}

function gen_paths(start, ends, max_path_len, map_rnode_outedges) {
  const complete_paths = [];
  const remaining_paths = [[start]];
  while (!cmn.isArrayEmpty(remaining_paths)) {
    const path = remaining_paths.pop();
    const head = path[path.length-1];
    const outedges = map_rnode_outedges(head);
    const is_terminal_len = (path.length === max_path_len-2);
    if (is_terminal_len) {
      for (let i = 0; i < outedges.length; i++) {
        const edge = outedges[i];
        const is_end_node = ends.includes(edge.target);
        const is_cycle = path.includes(edge.target);
        if (is_end_node && !is_cycle) {
          path.push(edge.redge, edge.target);
          complete_paths.push(path);
        }
      }
    } else {
      for (let i = 0; i < outedges.length; i++) {
        const edge = outedges[i];
        const is_cycle = path.includes(edge.target);
        if (!is_cycle) {
          const is_end_node = ends.includes(edge.target);
          if (is_end_node) {
            complete_paths.push([...path, edge.redge, edge.target]);
          }
          remaining_paths.push([...path, edge.redge, edge.target]);
        }
      }
    }
  }
  return complete_paths;
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

function genPid(path) {
  return hash(path);
}

function answersToSmryFgmts(answers, nodeRules, edgeRules, max_hops) {
  function resultToSmryFgmt(result, kgraph, auxGraphs, startKey, endKey, errors) {
    function analysisToSmryFgmt(analysis, kgraph, auxGraphs, start, ends) {
      function finalizePaths(rgraph_paths, edgeMappings, kgraph) {
        function N(n) { return genNid(n, kgraph); }
        function E(e, o) { return genEid(e, kgraph, isRedgeInverted(e, o, kgraph)); }
        const normalizedMappings = {};
        const normalizedPaths = rgraph_paths.map(path => {
          let normalizedPath = [];
          const pathLength = path.length - 1;
          if (pathLength < 0) {
            return normalizedPath;
          }

          for (let i = 0; i < pathLength; i+=2) {
            const node = path[i];
            const edge = path[i+1];
            const normalizedEdge = E(edge, node);
            if (!normalizedMappings[normalizedEdge]) {
              normalizedMappings[normalizedEdge] = { embedded_in: [], supporting_graphs: [] };
            }
            normalizedMappings[normalizedEdge].embedded_in.push(...edgeMappings[edge].embedded_in);
            normalizedMappings[normalizedEdge].supporting_graphs.push(...edgeMappings[edge].supporting_graphs);
            normalizedPath.push(N(node), normalizedEdge);
          }

          normalizedPath.push(N(path[pathLength]));
          return normalizedPath;
        });

        Object.keys(normalizedMappings).forEach(atrId => cmn.objRemoveDuplicates(normalizedMappings[atrId]));
        const pathToSupGraph = {};
        // For every path find which graphs the path appears in. A path appears in a graph iff all
        // edges in the path appear in the graph.
        for (const path of normalizedPaths) {
          let gids = [...normalizedMappings[path[1]].embedded_in];
          for (let i = 3; i < path.length; i+=2) {
            gids = gids.filter((gid) => normalizedMappings[path[i]].embedded_in.includes(gid));
          }

          pathToSupGraph[genPid(path)] = gids;
        }

        const edgeBases = {}
        // Determine which paths support which edges
        for (const edge of Object.keys(normalizedMappings)) {
          const edgeSupGraphs = normalizedMappings[edge].supporting_graphs;
          const edgePaths = [];
          for (const path of Object.keys(pathToSupGraph)) {
            for (const pgid of pathToSupGraph[path]) {
              if (edgeSupGraphs.includes(pgid)) {
                edgePaths.push(path);
                break;
              }
            }
          }

          if (!edgeBases[edge]) {
            edgeBases[edge] = new SummaryEdge();
          }

          const s_edge = edgeBases[edge];
          s_edge.extendSupPaths(edgePaths);
          s_edge.isRootPath = normalizedMappings[edge].embedded_in.includes('root');
          s_edge.type = s_edge.hasSupport() ? CONSTANTS.INDIRECT_EDGE : CONSTANTS.DIRECT_EDGE;
        }

        return [normalizedPaths, edgeBases];
      }

      const agent = cmn.jsonGet(analysis, 'resource_id', false);
      if (!agent) {
        return makeErrSmryFgmt('unknown', 'Expected analysis to have resource_id');
      }

      try {
        const rgraph = analysis_to_rgraph(analysis, kgraph, auxGraphs); // D!
        const map_rnode_outedges = make_map_rnode_outedges(rgraph, kgraph);
        const max_path_len = (2 * max_hops) + 1;
        // This is an exhaustive search based on the max path length. We may have to come up
        // with a better algorithm if the max path length increases significantly.
        const rgraph_paths = gen_paths(start, ends, max_path_len, map_rnode_outedges); // C!
        const [normalizedPaths, edgeBases] = finalizePaths(rgraph_paths, rgraph.edge_topology, kgraph);
        const analysisCxt = {
          agent: agent,
          errors: errors
        };

        return new SummaryFragment(
          [agent],
          normalizedPaths,
          rgraph.nodes.map(rnode => { return summarizeRnode(rnode, kgraph, nodeRules, analysisCxt); }),
          new SummaryFragmentEdges(
            edgeBases,
            rgraph.edges.map(redge => {
              const kedge = trapi.getKedge(redge, kgraph);
              const edgeCxt = cmn.deepCopy(analysisCxt);
              edgeCxt.primarySrc = trapi.getPrimarySrc(kedge);
              return summarizeRedge(redge, kgraph,
                 edgeRules, edgeCxt, new Set(Object.keys(edgeBases)));
            })
          ));
      } catch (err) {
        console.error(err);
        if (err instanceof EdgeBindingNotFoundError) {
          return makeErrSmryFgmt(agent, e.message);
        }

        return makeErrSmryFgmt(agent, 'Unknown error while building RGraph');
      }
    }

    try {
      const [rnodeStart, rnodeEnds] = getResultStartAndEnd(result, startKey, endKey);
      // TODO: There SHOULD only be a single start point. We should probably throw an error when this is not the case.
      const analyses = getResultAnalyses(result);
      const resultSmryFgmt = analyses.reduce(
        (rsf, analysis) => {
          return rsf.merge(analysisToSmryFgmt(analysis, kgraph, auxGraphs, rnodeStart, rnodeEnds));
        },
        new SummaryFragment());

      if (!resultSmryFgmt.isEmpty()) {
        // Ordering components are a property of the result, so we have to add them after the result analyses are summarized.
        const rid = genNid(rnodeStart, kgraph); // The first node uniquely identifies a result.
        const scoringComponents = getResultScoringComponents(result);
        resultSmryFgmt.pushScore(rid, scoringComponents);
      }

      return resultSmryFgmt;
    } catch (err) {
      console.error(err);
      if (err instanceof NodeBindingNotFoundError) {
        return makeErrSmryFgmt('unknown', err.message);
      }

      return makeErrSmryFgmt('unknown', 'Unknown error while building result summary fragment');
    }
  }

  const smryFgmts = [];
  const errors = {};
  answers.forEach((answer) => {
    const results = trapi.getResults(answer);
    if (!results) {
      // TODO: Add warning
      return;
    }

    // TODO: What to do if these fail
    const kgraph = trapi.getKgraph(answer);
    const auxGraphs = trapi.getAuxGraphs(answer);
    const [startKey, endKey] = trapi.messageToEndpoints(answer);

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

function smryPathFromPid(pid, paths) {
  return cmn.jsonGet(paths, pid);
}

function pidSort(pids, paths) {
  function pidCompare(pid1, pid2) {
    const smryPath1 = smryPathFromPid(pid1, paths);
    const smryPath2 = smryPathFromPid(pid2, paths);
    const comparison = smryPathCompare(smryPath1, smryPath2);
    if (comparison === 0) {
      if (pid1 < pid2) return -1;
      if (pid2 < pid1) return 1;
      return 0;
    }

    return comparison;
  }

  if (pids.length < 2) return pids;
  return pids.sort(pidCompare);
}

function isRootPath(pid, paths, edges) {
  const smryPath = smryPathFromPid(pid, paths);
  let isRoot = true;
  smryPath.forEids((eid) => {
    isRoot = isRoot && edges[eid].isRootPath;
  });

  return isRoot;
}

function getRootPids(pids, paths, edges) {
  const rootPids = pids.filter(pid => isRootPath(pid, paths, edges));
  return rootPids;
}

function genSupChain(pids, paths, edges) {
  const seenPids = [];
  const remaining = getRootPids(pids, paths, edges);
  while (remaining.length !== 0) {
    const next = remaining.pop();
    if (seenPids.includes(next)) continue;
    seenPids.push(next);
    const smryPath = smryPathFromPid(next, paths);
    smryPath.forEids((eid) => {
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
      const path = smryPathFromPid(pid, paths);
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

function genMetaPath(path, nodes) {
  const metaPath = [];
  for (let i = 0; i < path.length; i+=2) {
    const nid = path[i];
    const node = nodes[nid];
    metaPath.push(node.type);
  }

  return metaPath;
}

function smryFgmtsToSmry(qid, smryFgmts, kgraph, queryType, errors) {
  function fgmtPathsToResultsAndPaths(fgmtPaths, nodes, queryType) {
    const results = [];
    const paths = [];
    fgmtPaths.forEach((path) => {
      const pid = genPid(path);
      let rid = path[0];
      if (trapi.isPathfinderQuery(queryType)) {
        rid = genPid(genMetaPath(path, nodes));
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
      const smryPath = cmn.jsonGet(paths, path.pid, false);
      if (smryPath) {
        smryPath.extendAgents(agents);
        return;
      }

      cmn.jsonSet(paths, path.pid, new SummaryPath(path.path, agents));
    });
  }

  function extendSmryGraphElem(objs, updates, agents, defaultValue) {
    updates.forEach((update) => {
      let obj = cmn.jsonSetDefaultAndGet(objs, update.id, defaultValue());
      update.transforms.forEach((transform) => {
        transform(obj);
        obj.aras.push(...agents);
      });
    });
  }

  function extendSmryNodes(nodes, nodeUpdates, agents) {
    extendSmryGraphElem(nodes, nodeUpdates, agents, () => new SummaryNode());
  }

  function extendSmryEdges(edges, edgeFgmts, agents) {
    Object.keys(edgeFgmts.base).forEach((eid) => {
      const edge = cmn.jsonSetDefaultAndGet(edges, eid, new SummaryEdge());
      edge.merge(edgeFgmts.base[eid]);
    });

    extendSmryGraphElem(edges, edgeFgmts.updates, agents, () => new SummaryEdge());
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
    const trials = cmn.jsonGet(edge, CONSTANTS.SUPPORTING_TRIALS, {});
    Object.keys(trials).forEach((ctid) => {
      const trial = trials[ctid];
      trial.id = ctid;
      const [_, url] = ev.idToTypeAndUrl(ctid);
      trial.url = url;
      smryTrials[ctid] = trial;
    });
  }

  function extractAndFinalizeEdges(edges) {
    function addInverseEdge(edges, edge) {
      const invertedPred = genQualifiedPred(edge, true);
      const sub = edge.subject;
      const obj = edge.object;
      const invertedEid = genPid([obj, invertedPred, sub, edge.type]);
      let invertedEdge = cmn.deepCopy(edge);
      cmn.jsonMultiSet(invertedEdge,
                      [['subject', obj],
                       ['object', sub],
                       ['predicate', invertedPred]]);

      const unqualifiedInvertedPred = bl.invertBiolinkPred(getMostSpecificPred(edge));
      invertedEdge.predUrl = bl.predToUrl(unqualifiedInvertedPred);
      delete invertedEdge['qualifiers'];
      edges[invertedEid] = invertedEdge;
    }

    const pubs = {};
    const trials = {};
    Object.values(edges).forEach((edge) => {
      extendSmryPubs(pubs, edge);
      extendSmryTrials(trials, edge);
      const edgePubs = cmn.jsonGet(edge, 'publications', {})
      const supText = cmn.jsonGet(edge, CONSTANTS.SUPPORTING_TEXT, {});
      Object.keys(edgePubs).forEach((kl) => {
        edgePubs[kl] = edgePubs[kl].map((pub) => {
          return { id: pub.id, support: supText[pub.id] || null };
        });
      });
      delete edge[CONSTANTS.SUPPORTING_TEXT];
      edge.trials = Object.keys(cmn.jsonGet(edge, CONSTANTS.SUPPORTING_TRIALS, {}));
      delete edge[CONSTANTS.SUPPORTING_TRIALS];
      addInverseEdge(edges, edge);
      cmn.jsonSet(edge, 'predicate_url', bl.predToUrl(getMostSpecificPred(edge)));
      cmn.jsonSet(edge, 'predicate', genQualifiedPred(edge));
      delete edge['qualifiers'];
    });

    return [edges, pubs, trials];
  }

  function resultsToResultsAndTags(results, paths, nodes, edges, scores, errors, queryType) {
    function markPathAsIndirect(pid, edges, paths) {
      function helper(pid, edges, paths, seen) {
        seen.add(pid);
        const tag = 'p/pt/inf';
        const smryPath = smryPathFromPid(pid, paths);
        smryPath.forEids((eid) => {
          const edge = edges[eid];
          if (edge.hasSupport()) {
            for (let spid of edge.supPaths) {
              if (!seen.has(spid)) {
                helper(spid, edges, paths, seen);
              }
            }
          }
        });
        smryPath.tags[tag] = makeTagDescription('Indirect');
      }
      helper(pid, edges, paths, new Set());
    }

    function genName(nodes, smryPath, queryType) {
      if (trapi.isPathfinderQuery(queryType)) {
        const metaPath = genMetaPath(smryPath.subgraph, nodes)
        metaPath[0] = nodes[smryPath.start].name();
        metaPath[metaPath.length-1] = nodes[smryPath.end].name();
        return metaPath.join('/');
      }
      return nodes[smryPath.start].name();
    }

    function genId(nodes, smryPath, queryType) {
      if (trapi.isPathfinderQuery(queryType)) {
        const metaPath = genMetaPath(smryPath.subgraph, nodes);
        return genPid(metaPath);
      }
      return genPid([smryPath.start, smryPath.end]);
    }

    const usedTags = {};
    const expandedResults = [];
    for (const result of results) {
      const pids = result.paths;
      const rootPids = getRootPids(pids, paths, edges);
      // Bail if there are no root paths
      if (rootPids.length === 0) {
        let aras = new Set();
        for (const pid of pids) {
          for (const ara of paths[pid].aras) {
            aras.add(ara);
          }
        }

        aras = [...aras];
        const errorString = "No root paths found";
        console.error(`${aras.join(', ')}: ${errorString}`)
        for (const ara of aras) {
          const araErrors = cmn.jsonSetDefaultAndGet(errors, ara, []);
          araErrors.push(errorString);
        }

        continue;
      }

      const smryPath = smryPathFromPid(rootPids[0], paths);
      const name = genName(nodes, smryPath, queryType);
      const start = smryPath.start;
      const end = smryPath.end;
      const tags = {};
      pids.forEach((pid) => {
        const path = smryPathFromPid(pid, paths);
        for (const tag of Object.keys(path.tags)) {
          if (isResultTag(tag) && path.start !== start) continue;
          usedTags[tag] = path.tags[tag];
          tags[tag] = null;
        };
      });

      // Generate direct/indirect tags for results
      rootPids.forEach((pid) => {
        const smryPath = smryPathFromPid(pid, paths);
        let isPathIndirect = false;
        smryPath.forEids((eid) => {
          const edge = edges[eid];
          isPathIndirect = isPathIndirect || edge.hasSupport();
        });

        if (isPathIndirect) {
          const tag = 'p/pt/inf';
          markPathAsIndirect(pid, edges, paths);
          usedTags[tag] = makeTagDescription('Indirect');
          tags[tag] = null;
        } else {
          const tag = 'p/pt/lkup';
          const directTag = makeTagDescription('Direct');
          usedTags[tag] = directTag;
          smryPath.tags[tag] = directTag;
          tags[tag] = null;
        }
      });

      expandedResults.push({
        'id': genId(nodes, smryPath, queryType),
        'subject': start,
        'drug_name': name,
        'paths': pidSort(rootPids, paths),
        'object': end,
        'scores': scores[start],
        'tags': tags
      });
    }

    return [expandedResults, usedTags];
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
    extendSmryNodes(nodes, sf.nodes, agents);
    extendSmryEdges(edges, sf.edges, agents);
    extendSmryScores(scores, sf.scores);
    extendSmryErrors(errors, sf.errors);
  });

  Object.values(nodes).forEach(node => {
    node.types.sort(bl.biolinkClassCmpFn);
  });

  smryFgmts.forEach((sf) => {
    const agents = sf.agents;
    const [newResults, newPaths] = fgmtPathsToResultsAndPaths(sf.paths, nodes, queryType);
    extendSmryResults(results, newResults);
    extendSmryPaths(paths, newPaths, agents);
  });

  results = Object.values(results).map(cmn.objRemoveDuplicates)
  function pushIfEmpty(arr, val) {
    if (cmn.isArrayEmpty(arr)) {
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

    // Remove duplicates from publications
    const pubs = cmn.jsonGet(edge, 'publications', {});
    Object.keys(pubs).forEach((kl) => {
      const klPubs = cmn.jsonGet(pubs, kl, []);
      const seenIds = new Set();
      cmn.jsonSet(pubs, kl, klPubs.filter((pub) => {
        const shouldInclude = !seenIds.has(pub.id);
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

    edge.knowledgeLevel = edge.provenance[0].knowledge_level;
    edge.type = edge.hasSupport() ? CONSTANTS.INDIRECT_EDGE : CONSTANTS.DIRECT_EDGE;
  });

  [edges, pubs, trials] = extractAndFinalizeEdges(edges);

  const meta = new SummaryMetadata(qid, cmn.distinctArray(smryFgmts.map((sf) => {
    return sf.agents;
  }).flat()));

  try {
    // Node annotation
    const nodeRules = makeExtractionRules(
      [
        renameAndTransformAttribute(
          'biothings_annotations',
          ['descriptions'],
          (annotations) => {
            const description = bta.getDescription(annotations[0]);
            if (description === null) {
              return [];
            }

            return [description];
          }
        ),
        renameAndTransformAttribute(
          'biothings_annotations',
          ['other_names'],
          (annotations) => {
            const otherNames = bta.getNames(annotations[0]);
            if (otherNames === null
                || (cmn.isArrayEmpty(otherNames.commercial) && cmn.isArrayEmpty(otherNames.generic))) {
              return null;
            }

            return otherNames;
          }
        ),
        aggregateAndTransformAttributes(
          ['biothings_annotations'],
          'curies',
          (annotation) => bta.getCuries(annotation)
        ),
        renameAndTransformAttribute(
          'biothings_annotations',
          ['species'],
          (annotations) => {
            const species = bta.getSpecies(annotations[0]);
            return species;
          }
        ),
        tagAttribute(
          'biothings_annotations',
          (annotations) => {
            const tdl = bta.getTdl(annotations[0]);
            if (tdl === null) return false;
            return [makeTag(`r/tdl/${tdl.toLowerCase()}`, tdl)];
          })
      ]
    );

    const resultNodeRules = makeExtractionRules(
      [
        tagAttribute(
          'biothings_annotations',
          (annotations) => {
            const fdaApproval = bta.getFdaApproval(annotations[0]);
            if (fdaApproval === null) {
              return false;
            } else if (fdaApproval < 4) {
              const tags = [];
              if (fdaApproval > 0) {
                tags.push(makeTag(`r/fda/${fdaApproval}`, `Clinical Trial Phase ${fdaApproval}`));
              } else {
                tags.push(makeTag('r/fda/0', 'Not FDA Approved'));
              }

              return tags;
            } else {
              return [makeTag(`r/fda/${fdaApproval}`, `FDA Approved`)];
            }
          }
        ),
        tagAttribute(
          'biothings_annotations',
          (annotations, cxt) => {
            if (trapi.isGeneChemicalQuery(cxt.queryType)) return [];

            const chebiRoles = bta.getChebiRoles(annotations[0]);
            if (chebiRoles === null) {
              return [];
            }

            return chebiRoles.map((role) => { return makeTag(`r/role/${role.id}`, cmn.titleize(role.name))});
          }
        ),
        renameAndTransformAttribute(
          'biothings_annotations',
          ['indications'],
          (annotations) => bta.getDrugIndications(annotations[0])
        ),
        tagAttribute(
          'biothings_annotations',
          (annotations) => {
            const otc = bta.getOtc(annotations[0]);
            if (otc === null) return [];
            return [makeTag(`r/otc/${otc.id}`, otc.name)];
          }
        )
      ]
    );

    const resultNodes = new Set();
    results.forEach((result) => {
        const pids = cmn.jsonGet(result, 'paths');
        pids.forEach((pid) => {
          const smryPath = smryPathFromPid(pid, paths);
          resultNodes.add(smryPath.start);
        });
      });

    const annotationCxt = {
      agent: 'biothings-annotator',
      queryType: queryType,
      errors: {}
    };

    const nodeUpdates = Object.keys(nodes).map((nid) => {
      return summarizeRnode(nid, kgraph, nodeRules, annotationCxt);
    });

    const resultNodeUpdates = [...resultNodes].map((nid) => {
      return summarizeRnode(nid, kgraph, resultNodeRules, annotationCxt);
    });

    extendSmryNodes(nodes, nodeUpdates.concat(resultNodeUpdates), ['biothings-annotator']);
    extendSmryErrors(errors, annotationCxt.errors);
  }
  catch (err) {
    console.error(err);
  }
  finally {
    // Node post-processing
    Object.keys(nodes).forEach((nid) => {
      const node = nodes[nid];
      node.curies.push(nid);
      // Remove any duplicates on all node attributes
      cmn.objRemoveDuplicates(node);
      node.types.sort(bl.biolinkClassCmpFn);

      // Provide a CURIE as a default value if the node has no name
      const nodeNames = cmn.jsonGet(node, 'names');
      pushIfEmpty(nodeNames, nid);

      cmn.jsonSet(node, 'provenance', [bl.curieToNormalizedUrl(nid, node.curies)]);
    });

    // Path post-processing
    Object.keys(paths).forEach((pid) => {
      const smryPath = smryPathFromPid(pid, paths);
      // Remove paths where there is an undefined node reference in the path

      smryPath.forNids((nid) => {
        if (nodes[nid] === undefined) {
          delete paths[pid];
          return;
        }
      });

      // Remove paths where there is an undefined edge reference in the path
      smryPath.forEids((eid) => {
        if (edges[eid] === undefined) {
          delete paths[pid];
          return;
        }
      });

      // Remove duplicates from every attribute on a path
      cmn.objRemoveDuplicates(smryPath);

      if (trapi.isChemicalDiseaseQuery(queryType)) {
        // Consider the chemical indicated for the disease iff
        //   1. The chemical is marked as indicated for the disease
        //   2. The chemical has reached phase 4 approval from the FDA
        const start = nodes[smryPath.start];
        if (start.indications !== undefined) {
          const startIndications = new Set(start.indications);
          const end = nodes[smryPath.end];
          const endMeshIds = end.curies.filter((curie) => { return curie.startsWith('MESH:'); });
          let indicatedFor = false;
          for (let i = 0; i < endMeshIds.length; i++) {
            if (startIndications.has(endMeshIds[i])) {
              indicatedFor = start.tags['r/fda/4'] !== undefined;
              break;
            }
          }

          let indicationTag = null;
          if (indicatedFor) {
            indicationTag = makeTag('r/di/ind', 'In a clinical trial for indicated disease');
          } else {
            indicationTag = makeTag('r/di/not', 'Not in a clinical trial for indicated disease');
          }

          addTag(start, indicationTag);
        }

        cmn.jsonDelete(start, 'indications');
      }

      // Add tags for paths by processing nodes
      mergeTags(smryPath, nodes[smryPath.start], (tag) => { return isResultTag(tag) && isExternalTag(tag) });
      smryPath.forInternalNids((nid) => {
        const node = nodes[nid];
        mergeTags(smryPath, node, (tag) => { return !isResultTag(tag) && isExternalTag(tag) });
        const nodeType = node.type;
        const typeTag = makeTag(`p/pc/${nodeType}`, nodeType);
        addTag(smryPath, typeTag);
      });

      // Generate a special tag for the answer node
      const maxPhaseTag = genMaxPhaseTag(nodes[smryPath.start], queryType);
      if (maxPhaseTag) {
        addTag(smryPath, maxPhaseTag);
      }

      // Generate tags based on the aras for this path
      const agentInfores = smryPath.agents;
      agentInfores.forEach((infores) => {
        const araTag = makeTag(`r/ara/${infores}`, inforesToName(infores));
        addTag(smryPath, araTag);
      });

      // Generate tags for number of connections
      const edgeCount = smryPath.edgeCount
      let tagDescription = 'Connections';
      if (edgeCount === 1) {
        tagDescription = 'Connection';
      }

      const lengthTag = makeTag(`p/pt/${edgeCount}`, `${edgeCount} ${tagDescription}`);
      addTag(smryPath, lengthTag);
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
    [results, tags] = resultsToResultsAndTags(results, paths, nodes, edges, scores, errors, queryType);
    Object.keys(edges).forEach((eid) => {
      const edge = cmn.jsonGet(edges, eid);
      edge.supPaths = pidSort(edge.supPaths, paths);
    });

    return new Summary(meta, results, paths, nodes, edges, pubs, trials, tags, errors);
  }
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

class EdgeBindingNotFoundError extends Error {
  constructor(edgeBinding) {
    super(`Edge binding not found for ${JSON.stringify(edgeBinding)}`);
  }
}

class AuxGraphNotFoundError extends Error {
  constructor(auxGraph) {
    super(`Auxiliary graph not found for ${auxGraph}`);
  }
}
