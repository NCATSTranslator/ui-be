'use strict';

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';
import * as bta from '../biothings-annotation.mjs';
import * as trapi from '../trapi.mjs';
import * as sa from './summary-analysis.mjs';
import * as id from './identifiers.mjs';
import * as pred from './predicates.mjs';
import { SummaryEdge } from './SummaryEdge.mjs';

const CONSTANTS = {
  PUBLICATIONS: 'publications',
  SUPPORTING_TEXT: 'supporting_text',
  SUPPORTING_TRIALS: 'supporting_trials',
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

  const knode_summarization_rules = makeExtractionRules(
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
      get_semmed_sentences(),
      getPropertyRule('subject'),
      getPropertyRule('object'),
      getPubs(),
      get_publication_data(),
      get_clinical_data()
    ]);

  const queryType = answersToQueryTemplate(answers);
  const [sfs, errors] = answersToSmryFgmts(answers, knode_summarization_rules, edgeRules, max_hops);
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
    this.tags = {};
    this.types = [];
    this.species = null;
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

  set_provenance(provenance) {
    this.provenance = [provenance];
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
function get_publication_data() {
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

/* A special rule for extracting supporting text for a publication.
 *
 * @returns {function} - The extraction rule.
 */
function get_semmed_sentences() {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attr_itr = new trapi.AttributeIterator(obj[key]);
      const publication_data = attr_itr.findAllVal(['bts:sentence']);
      const supporting_text = {};
      for (const entry of publication_data) {
        for (const [pid, sentence_data] of Object.entries(entry)) {
          supporting_text[pid].text = cmn.jsonGet(sentence_data, 'sentence');
        }
      }
      return supporting_text;
    },
    (supporting_text, acc) => {
      const current_support_text = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.SUPPORTING_TEXT, {});
      Object.keys(supporting_text).forEach((pid) => {
        current_support_text[pid] = supporting_text[pid];
      });
    },
    {}
  );
}

function get_clinical_data() {
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
            case 'brief_title':
              const title = trapi.getAttrVal(attr);
              extractedTrial.title = title.replace(/"/g, '');
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

function summarize_knode(nid, knode, knode_summarization_rules, summary_context) {
  return cmn.make_pair('id',         nid,
                       'transforms', knode_summarization_rules(knode, summary_context));
}

function summarize_kedge(eid, kedge, edgeRules, summary_context) {
  const edge_context = cmn.deepCopy(summary_context)
  edge_context.primarySrc = trapi.getPrimarySrc(kedge);
  return cmn.make_pair('id',         eid,
                       'transforms', edgeRules(kedge, edge_context));
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

function answersToSmryFgmts(answers, knode_summarization_rules, edgeRules, max_hops) {
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
          const knode = trapi.getKnode(nb, kgraph);
          return summarize_knode(nb, knode, knode_summarization_rules, analysis_context);
        });
        const kedge_properties = [];
        for (const [eid, edge] of Object.entries(summary_edges)) {
          for (const eb of edge.metadata.edge_bindings) {
            const kedge = trapi.getKedge(eb, kgraph);
            kedge_properties.push(summarize_kedge(eid, kedge, edgeRules, analysis_context));
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
      const [rnodeStart, rnodeEnds] = getResultStartAndEnd(result, startKey, endKey);
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

function _pid_to_summary_path(pid, paths) {
  return cmn.jsonGet(paths, pid);
}

function pidSort(pids, paths) {
  function pidCompare(pid1, pid2) {
    const smryPath1 = _pid_to_summary_path(pid1, paths);
    const smryPath2 = _pid_to_summary_path(pid2, paths);
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
  const smryPath = _pid_to_summary_path(pid, paths);
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
    const smryPath = _pid_to_summary_path(next, paths);
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
      const path = _pid_to_summary_path(pid, paths);
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
      const pid = id.gen_pid(path);
      let rid = path[0];
      if (trapi.isPathfinderQuery(queryType)) {
        rid = id.gen_pid(genMetaPath(path, nodes));
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
      const obj = cmn.jsonSetDefaultAndGet(objs, update.id, defaultValue());
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
      edge.trials = Object.keys(cmn.jsonGet(edge, CONSTANTS.SUPPORTING_TRIALS, []));
      delete edge[CONSTANTS.SUPPORTING_TRIALS];
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

  function resultsToResultsAndTags(results, paths, nodes, edges, scores, errors, queryType) {
    function markPathAsIndirect(pid, edges, paths) {
      function helper(pid, edges, paths, seen) {
        seen.add(pid);
        const tag = 'p/pt/inf';
        const smryPath = _pid_to_summary_path(pid, paths);
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
        return id.gen_pid(metaPath);
      }
      return id.gen_pid([smryPath.start, smryPath.end]);
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

      const smryPath = _pid_to_summary_path(rootPids[0], paths);
      const name = genName(nodes, smryPath, queryType);
      const start = smryPath.start;
      const end = smryPath.end;
      const tags = {};
      pids.forEach((pid) => {
        const path = _pid_to_summary_path(pid, paths);
        for (const tag of Object.keys(path.tags)) {
          if (isResultTag(tag) && path.start !== start) continue;
          usedTags[tag] = path.tags[tag];
          tags[tag] = null;
        };
      });

      // Generate direct/indirect tags for results
      rootPids.forEach((pid) => {
        const smryPath = _pid_to_summary_path(pid, paths);
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
    edge.type = edge.hasSupport() ? trapi.CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT : trapi.CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
  });

  [edges, pubs, trials] = extractAndFinalizeEdges(edges);

  const meta = new SummaryMetadata(qid, cmn.distinctArray(smryFgmts.map((sf) => {
    return sf.agents;
  }).flat()));

  try {
    // Node annotation
    const knode_summarization_rules = makeExtractionRules(
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

    const kresult_summarization_rules = makeExtractionRules(
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
          const smryPath = _pid_to_summary_path(pid, paths);
          resultNodes.add(smryPath.start);
        });
      });

    const annotationCxt = {
      agent: 'biothings-annotator',
      queryType: queryType,
      errors: {}
    };

    const nodeUpdates = Object.keys(nodes).map((nid) => {
      const knode = trapi.getKnode(nid, kgraph);
      return summarize_knode(nid, knode, knode_summarization_rules, annotationCxt);
    });

    const resultNodeUpdates = [...resultNodes].map((nid) => {
      const knode = trapi.getKnode(nid, kgraph);
      return summarize_knode(nid, knode, kresult_summarization_rules, annotationCxt);
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

      const provenance = bl.curieToNormalizedUrl(nid, node.curies);
      if (!provenance) {
        logger.warn(`WARNING: No provenance for CURIE: ${nid}`);
      } else {
        node.set_provenance(provenance);
      }
    });

    // Path post-processing
    Object.keys(paths).forEach((pid) => {
      const path = _pid_to_summary_path(pid, paths);
      // Remove paths where there is an undefined node reference in the path
      path.forNids((nid) => {
        if (nodes[nid] === undefined) {
          delete paths[pid];
          return;
        }
      });
      // Remove paths where there is an undefined edge reference in the path
      path.forEids((eid) => {
        if (edges[eid] === undefined) {
          delete paths[pid];
          return;
        }
      });
      // Remove duplicates from every attribute on a path
      cmn.objRemoveDuplicates(path);
      // Generate tags for chemical indications
      if (trapi.isChemicalDiseaseQuery(queryType)) {
        // Consider the chemical indicated for the disease iff
        //   1. The chemical is marked as indicated for the disease
        //   2. The chemical has reached phase 4 approval from the FDA
        const path_start = nodes[path.start];
        if (path_start.indications !== undefined) {
          const path_start_indications = new Set(path_start.indications);
          const path_end = nodes[path.end];
          const path_end_mesh_ids = path_end.curies.filter((curie) => {
            return curie.startsWith('MESH:');
          });
          let is_path_indicated = false;
          for (let i = 0; i < path_end_mesh_ids.length; i++) {
            if (path_start_indications.has(path_end_mesh_ids[i])) {
              is_path_indicated = start.tags['r/fda/4'] !== undefined;
              break;
            }
          }

          let tag_indicated = null;
          if (is_path_indicated) {
            tag_indicated = makeTag('r/di/ind', 'In a clinical trial for indicated disease');
          } else {
            tag_indicated = makeTag('r/di/not', 'Not in a clinical trial for indicated disease');
          }

          addTag(start, tag_indicated);
        }

        cmn.jsonDelete(start, 'indications');
      }
      // Add tags for paths by processing nodes
      mergeTags(path, nodes[path.start], (tag) => { return isResultTag(tag) && isExternalTag(tag) });
      path.forInternalNids((nid) => {
        const node = nodes[nid];
        mergeTags(path, node, (tag) => { return !isResultTag(tag) && isExternalTag(tag) });
        const node_type = node.type;
        const tag_node_type = makeTag(`p/pc/${node_type}`, node_type);
        addTag(path, tag_node_type);
      });
      // Generate a special tag for the answer node
      const tag_max_phase = genMaxPhaseTag(nodes[path.start], queryType);
      if (tag_max_phase) {
        addTag(path, tag_max_phase);
      }
      // Generate tags based on the aras for this path
      const agent_infores = path.agents;
      agent_infores.forEach((infores) => {
        const tag_ara = makeTag(`r/ara/${infores}`, inforesToName(infores));
        addTag(path, tag_ara);
      });
      // Generate tags for number of connections
      const edge_count = path.edgeCount
      let tag_length_description = 'Connections';
      if (edge_count === 1) {
        tag_length_description = 'Connection';
      }
      const tag_length = makeTag(`p/pt/${edge_count}`, `${edge_count} ${tag_length_description}`);
      addTag(path, tag_length);
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

class AuxGraphNotFoundError extends Error {
  constructor(auxGraph) {
    super(`Auxiliary graph not found for ${auxGraph}`);
  }
}
