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
  TAGS: 'tags'
};

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

/**
 * Responsible for converting a set of TRAPI answers into a summarized form that the FE application can use.
 *
 * @param {string}   qid - The query ID for the given answer set.
 * @param {object[]} answers - The set of TRAPI answers to summarize.
 * @param {number}   maxHops - The maximum number of hops to consider when summarizing the answers.
 * @param {object}   nodeAnnotationClient - The annotation client to use for annotating nodes.
 *
 * @returns {object} - The summarized form of the answers.
 */
export function answersToSummary (qid, answers, maxHops, nodeAnnotationClient) {
  const nodeRules = makeExtractionRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAttributes([bl.tagBiolink('xref')], 'curies'),
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
        (obj, key) => trapi.getPrimarySource(obj)),
      transformPropertyRule('qualifiers', (obj, key) => cmn.jsonGet(obj, key, false)),
      getPropertyRule('subject'),
      getPropertyRule('object'),
      getPublications(),
      getSupportingText()
    ]);

  const queryType = answersToQueryTemplate(answers);
  const [sfs, errors] = answersToSummaryFragments(answers, nodeRules, edgeRules, maxHops);
  return summaryFragmentsToSummary(
    qid,
    sfs,
    queryType,
    nodeAnnotationClient,
    errors);
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
  return trapi.messageToQueryTemplate(answers[0]);
}

function inforesToName(infores) {
  return bl.inforesToProvenance(infores).name;
}

/* Constructs a rule on how to extract a property from an object. There are 3 different stages to an extraction rule:
 * 1. Definition: This is what this function does.
 * 2. Application: Applying the rule to an object produces a function that can be applied to an accumulator with some optional context.
 * 3. Accumulation: Once the accumulator (target object) is known, the actual extraction and accumulation can by applied.
 *
 * @param {string} key - The key to extract from an object.
 * @param {function} transform - The transformation to apply to the extracted value.
 * @param {function} update - How to update the accumulator with the extracted value.
 * @param {object} defaultValue - The default value to use if the extraction fails.
 *
 * @returns {function} - The extraction rule.
 */
function makeExtractionRule(key, transform, update, defaultValue) {
  return (obj, context) => {
    return (acc) => {
      try {
        const v = transform(obj, key, context);
        return update(v, acc);
      } catch (e) {
        const agentErrors = cmn.jsonSetDefaultAndGet(context.errors, context.agent, []);
        agentErrors.push(e.message);
        return update(defaultValue, acc);
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
      currentValue.push(...cmn.coerceArray(property));
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
function renameAndTransformAttribute(attributeId, kpath, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const attrValue = attrIter.findOne(attributeId);
      if (attrValue === null) {
        return null;
      }

      return transform(attrValue);
    },
    (attribute, acc) => {
      const currentValue = cmn.jsonGetFromKpath(acc, kpath, false);
      if (currentValue && attribute === null) {
        return acc;
      }

      return cmn.jsonSetFromKpath(acc, kpath, attribute);
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
function aggregateAndTransformAttributes(attributeIds, accKey, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, context) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const result = attrIter.findAll(attributeIds);
      return result.map((v) => { return transform(v, context); });
    },
    (attributes, acc) => {
      const currentValue = cmn.jsonSetDefaultAndGet(acc, accKey, []);
      currentValue.push(...attributes);
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
function aggregateAttributes(attributeIds, accKey) {
  return aggregateAndTransformAttributes(
    attributeIds,
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
function tagAttribute(attributeId, transform) {
  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, context) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const result = attrIter.findAll(attributeIds);
      return result.map((v) => { return transform(v, context); });
    },
    (tags, acc) => {
      const currentTags = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.TAGS, {});
      if (!tags) return acc;
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
function getSupportingText() {
  const searchAttrId = bl.tagBiolink('has_supporting_study_result');
  const publicationsId = bl.tagBiolink('publications');
  const textId = bl.tagBiolink('supporting_text');
  const subjectTokenId = bl.tagBiolink('subject_location_in_text');
  const objectTokenId = bl.tagBiolink('object_location_in_text');

  function parseTokenIndex(token) {
    const range = token.split('|').map(t => parseInt(t.trim()));
    range[0] = range[0] + 1;
    return range;
  }

  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key) => {
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const supportingTextAttributes = attrIter.findAll([searchAttrId]);
      const supportingText = {};
      supportingTextAttributes.forEach(attribute => {
        const innerAttrs = trapi.getAttributes(attribute)
        const supportingTextData = {};
        innerAttrs.forEach(attribute => {
          const aid = trapi.getAttributeId(attribute);
          const av = trapi.getAttributeValue(attribute);
          switch (aid) {
            case publicationsId:
              supportingText[ev.sanitize(av)] = supportingTextData; break;
            case textId:
              supportingTextData.text = av; break;
            case subjectTokenId:
              supportingTextData.subject = parseTokenIndex(av); break;
            case objectTokenId:
              supportingTextData.object = parseTokenIndex(av); break;
          }
        });
      });

      return supportingText;
    },
    (supportingText, acc) =>
    {
      const currentSupportingText = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.SUPPORTING_TEXT, {});
      Object.keys(supportingText).forEach((pid) => {
        currentSupportingText[pid] = supportingText[pid];
      });
    },
    {});
}

/* A special rule for extracting publications from attributes.
 *
 * @returns {function} - The extraction rule.
 */
function getPublications() {
  const publicationIds = [
      bl.tagBiolink('supporting_document'),
      bl.tagBiolink('Publication'),
      bl.tagBiolink('publications'),
      bl.tagBiolink('publication') // Remove me when this is fixed in the ARA/KPs
  ];

  return makeExtractionRule(
    trapi.CONSTANTS.GRAPH.ATTRIBUTES.KEY,
    (obj, key, context) => {
      const publications = {};
      const attrIter = new trapi.AttributeIterator(obj[key]);
      const pubAttrs = attrIter.findAll(publicationIds);
      if (cmn.isArrayEmpty(pubAttrs)) return publications;
      const provenance = bl.inforesToProvenance(context.primarySource);
      const knowledgeLevel = getKnowledgeLevel(obj, provenance);
      if (!publications[knowledgeLevel]) {
        publications[knowledgeLevel] = [];
      }

      pubAttrs.forEach((pid) => {
        publications[knowledgeLevel].push({
          id: ev.sanitize(pid),
          source: provenance
        });
      });

      return publications;
    },
    (publications, acc) =>
    {
      const currentPublications = cmn.jsonSetDefaultAndGet(acc, CONSTANTS.PUBLICATIONS, {});
      Object.keys(publications).forEach((knowledgeLevel) => {
        if (!currentPublications[knowledgeLevel]) {
          currentPublications[knowledgeLevel] = [];
        }

        currentPublications[knowledgeLevel].push(...(publications[knowledgeLevel]));
      });

      return acc;
    },
    {});
}

/**
 * Generates a function to extract attributes or properties from a TRAPI object given a set of rules.
 *
 * @param {object[]} rules - The set of rules to use for extracting attributes.
 *
 * @returns {function} - The function to extract attributes from a TRAPI object.
 */
function makeExtractionRules(rules) {
  return (obj, context) => {
    return rules.map(rule => { return rule(obj, context); });
  };
}

/**
 * Get the endpoints for a TRAPI result graph.
 *
 * @param {object} result - The TRAPI result graph.
 * @param {string} startKey - The key to use for the start node. There should only be a single start node.
 * @param {string} endKey - The key to use for the end nodes. There can be multiple end nodes.
 *
 * @returns {string[]} - The start and end nodes for the result graph.
 * @throws {NodeBindingNotFoundError} - If either of the start or end nodes are not found.
 * @throws {ReferenceError} - If the node bindings are malformed (missing an 'id' field).
 */
function getResultStartAndEnd(result, startKey, endKey) {
  function getEndpoints(result, key) {
    const nodeBinding = trapi.getNodeBinding(result, key);
    if (cmn.isArrayEmpty(nodeBinding)) {
      throw new NodeBindingNotFoundError(nodeBinding);
    }

    return nodeBinding.map((entry) => {
      let endpoint = cmn.jsonGet(entry, 'query_id', false);
      if (!endpoint) {
        endpoint = cmn.jsonGet(entry, 'id');
      }

      return endpoint;
    });
  }

  const rnodeStart = getEndpoints(result, startKey)[0];
  const rnodeEnds = getEndpoints(result, endKey); // There can be multiple endpoints
  return [rnodeStart, rnodeEnds];
}

function flattenBindings(bindings) {
  return Object.values(bindings).reduce((ids, binding) => {
    return ids.concat(binding.map(obj => { return cmn.jsonGet(obj, 'id'); }));
  },
  []);
}

// TODO: Add constants for KL/AT and the summarized KL
function getKnowledgeLevel(kedge, provenance) {
  const agentType = trapi.getAgentType(kedge);
  if (agentType === 'text_mining_agent') {
    return 'ml';
  }

  const knowledgeLevel = trapi.getKnowledgeLevel(kedge);
  if (knowledgeLevel === 'knowledge_assertion') {
    return 'trusted';
  } else if (knowledgeLevel === 'not_provided') {
    return 'unknown';
  } else if (knowledgeLevel !== null) {
    return 'inferred';
  }

  return provenance.knowledge_level;
}

function isNodeIndex(index) {
  return index % 2 === 0;
}

function kedgeAttributes(kedge) {
  const attributes = cmn.jsonGet(kedge, 'attributes', null);
  if (trapi.noAttributes(attributes)) {
    return [];
  }

  return attributes;
}

function kedgeSupportGraphs(kedge) {
  const attributes = kedgeAttributes(kedge);
  for (const attr of attributes) {
    if (trapi.getAttributeId(attr) === bl.tagBiolink('support_graphs')) {
      return trapi.getAttributeValue(attr);
    }
  };

  return [];
}

function kedgeToQualifiers(kedge) {
  const kedgeQualifiers = cmn.jsonGet(kedge, 'qualifiers', false);
  if (!kedgeQualifiers || !cmn.isArray(kedgeQualifiers) || cmn.isArrayEmpty(kedgeQualifiers)) {
    return false;
  }

  const qualifiers = {};
  kedgeQualifiers.forEach((q) => {
    const qualifierKey = q['qualifier_type_id'];
    const qualifierValue = q['qualifier_value'];
    if (qualifierKey === undefined || qualifierValue === undefined)
    {
      return false;
    }

    qualifiers[bl.sanitizeBiolinkItem(qualifierKey)] = bl.sanitizeBiolinkItem(qualifierValue);
  });

  return qualifiers;
}

// Get the most specific predicate available from a kedge
function getSpecificPredicate(kedge) {
  const qualifiers = kedgeToQualifiers(kedge);
  if (!qualifiers) {
    return kedge.predicate;
  }

  return cmn.jsonGet(qualifiers, 'qualified predicate', kedge.predicate);
}

function edgeToQualifiedPredicate(kedge, invert = false) {
  function qualifiersToString(type, qualifiers, prefixes) {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // This ordering is important for building the correct statement
    const qualifierKeys = ['direction', 'aspect', 'form or variant', 'part', 'derivative'];
    const qualifierValues = qualifierKeys.map((key) => {
        return cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false);
    });

    let qualifierStr = '';
    qualifierValues.forEach((qv, i) => {
      if (qv) {
        if (qualifierStr) {
          qualifierStr += ' '
        }

        if (prefixes[i]) {
          qualifierStr += `${prefixes[i]} `;
        }

        qualifierStr += qv;
      }
    });

    return qualifierStr;
  }

  function subjectQualifiersToString(qualifiers, directionPrefix = false) {
    return qualifiersToString('subject',
                              qualifiers,
                              [directionPrefix, false, 'of a', 'of the', false]);
  }

  function objectQualifiersToString(qualifiers, directionPrefix = false) {
    return qualifiersToString('object',
                              qualifiers,
                              [directionPrefix, false, 'of a', 'of the', false]);
  }

  function finalizeQualifiedPredicate(prefix, predicate, suffix) {
    if (prefix) {
      prefix += ' ';
    }

    if (suffix) {
      suffix = ` ${suffix} of`;
    }

    const finalPredicate = `${prefix}${predicate}${suffix}`;
    return finalPredicate;
  }

  function getSpecialCase(predicate, qualifiers, invert) {
    const objDirectionQualifier = qualifiers['object direction qualifier'];
    if (predicate === 'regulates' &&
          (objDirectionQualifier === 'upregulated' ||
           objDirectionQualifier === 'downregulated')) {
      if (invert) {
        return `is ${objDirectionQualifier} by`;
      }

      return objDirectionQualifier.replace('ed', 'es');
    }

    return false;
  }

  let predicate = bl.sanitizeBiolinkItem(trapi.getPredicate(kedge));
  let qualifiers = kedgeToQualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPredicate(predicate)) {
    [predicate, qualifiers] = bl.deprecatedPredicateToPredicateAndQualifiers(predicate);
  }

  // If we don't have any qualifiers, treat it like biolink v2
  if (!qualifiers) {
    if (invert) {
      predicate = bl.invertBiolinkPredicate(predicate);
    }

    return predicate;
  }

  predicate = getSpecificPredicate(kedge);
  const specialCase = getSpecialCase(predicate, qualifiers, invert);
  if (specialCase) {
    return specialCase;
  }

  if (invert) {
    const subjectQualifierStr = subjectQualifiersToString(qualifiers);
    const objectQualifierStr = objectQualifiersToString(qualifiers, 'has');
    return finalizeQualifiedPredicate(objectQualifierStr,
                                      bl.invertBiolinkPredicate(predicate),
                                      subjectQualifierStr);
  }

  const subjectQualifierStr = subjectQualifiersToString(qualifiers, 'has');
  const objectQualifierStr = objectQualifiersToString(qualifiers);
  return finalizeQualifiedPredicate(subjectQualifierStr,
                                    predicate,
                                    objectQualifierStr);
}

function makeTag(tag, name, description = '') {
  return {
    'label': tag,
    'description': makeTagDescription(name, description)
  };
}

function makeTagDescription(name, description = '') {
  return {
    'name': name,
    'value': description
  };
}

function determineAnswerTag(type, answerTags, queryType) {
  function isDrug(type, fdaLevel) {
    return fdaLevel === 4 || type === 'Drug';
  }

  function isClinicalPhase(fdaLevel) {
    return fdaLevel > 0 && fdaLevel < 4;
  }

  if (!trapi.isValidQuery(queryType) || trapi.isGeneChemicalQuery(queryType)) {
    return [false, false];
  }

  const fdaTags = Object.keys(answerTags).filter((tag) => { return tag.startsWith('fda'); });
  let highestFdaApproval = 0;
  if (!cmn.isArrayEmpty(fdaTags)) {
    highestFdaApproval = Math.max(...fdaTags.map((tag) => { return parseInt(tag.split(':')[1]); }));
  }

  if (highestFdaApproval === 0) return ['cc:other', 'Other'];

  if (isDrug(type, highestFdaApproval)) return ['cc:drug', 'Drug'];

  if (isClinicalPhase(highestFdaApproval)) return [`cc:phase${highestFdaApproval}`, `Phase ${highestFdaApproval} Drug`];

  return [`cc:other`, `Other`];
}

function makeRgraph(rnodes, redges, edgeMappings, kgraph) {
  if (!redges) {
    return false;
  }

  const knodes = cmn.jsonGet(kgraph, 'nodes');
  for (const rnode of rnodes) {
    if (!cmn.jsonHasKey(knodes, rnode)) {
      return false;
    }
  }

  const rgraph = {};
  rgraph.nodes = rnodes;
  rgraph.edges = redges.filter(redge => {
    const kedge = trapi.getKnowledgeEdge(redge, kgraph);
    return bl.isBiolinkPredicate(trapi.getPredicate(kedge));
  });
  rgraph.edgeMappings = edgeMappings;

  return rgraph;
}

function isRedgeInverted(redge, subject, kgraph) {
  const kedge = trapi.getKnowledgeEdge(redge, kgraph);
  return subject === trapi.getObject(kedge);
}

function analysisToRgraph(analysis, kgraph, auxGraphs) {
  const edgeBindingData = new Map();
  let unprocessedEdgeBindings = flattenBindings(cmn.jsonGet(analysis, 'edge_bindings', [])).map((eb) => {
    edgeBindingData[eb] = { partOf: ['root'] };
    return eb;
  });

  let unprocessedSupportGraphs = [];
  const nodeBindings = new Set();
  const supportGraphs = new Set();
  // Invariant: edges and subgraphs will only ever be processed once. This is very important
  //            for how the following code works.
  while (!cmn.isArrayEmpty(unprocessedEdgeBindings) || !cmn.isArrayEmpty(unprocessedSupportGraphs)) {
    while (!cmn.isArrayEmpty(unprocessedEdgeBindings)) {
      const eb = unprocessedEdgeBindings.pop();
      if (edgeBindingData[eb].supportPaths !== undefined) {
        continue;
      }

      const kedge = trapi.getKnowledgeEdge(eb, kgraph);
      if (!kedge) {
        throw new EdgeBindingNotFoundError(eb);
      }

      nodeBindings.add(trapi.getSubject(kedge));
      nodeBindings.add(trapi.getObject(kedge));
      const edgeSupportGraphs = kedgeSupportGraphs(kedge);
      edgeBindingData[eb].supportPaths = edgeSupportGraphs;
      edgeSupportGraphs.forEach((sg) => {
        if (!supportGraphs.has(sg)) {
          unprocessedSupportGraphs.push(sg);
        }
      });
    };

    while (!cmn.isArrayEmpty(unprocessedSupportGraphs)) {
      const gid = unprocessedSupportGraphs.pop();
      if (supportGraphs.has(gid)) {
        continue;
      }

      const auxGraph = cmn.jsonGet(auxGraphs, gid, false);
      if (!auxGraph) {
        throw new AuxGraphNotFoundError(gid);
      }

      const sgEdgeBindings = cmn.jsonGet(auxGraph, 'edges', []);
      sgEdgeBindings.forEach((eb) => {
        if (!edgeBindingData[eb]) {
          edgeBindingData[eb] = { partOf: [gid] };
          unprocessedEdgeBindings.push(eb);
        } else {
          // We do not want to process the same edge twice, but we need to include this
          // graph as a graph where this edge occurs.
          edgeBindingData[eb].partOf.push(gid);
        }
      });

      supportGraphs.add(gid);
    }
  }

  return makeRgraph([...nodeBindings], [...Object.keys(edgeBindingData)], edgeBindingData, kgraph);
}

function rnodeToKey(rnode, kgraph) {
  return rnode;
}

function redgeToKey(redge, kgraph, doInvert = false) {
  const kedge = trapi.getKnowledgeEdge(redge, kgraph);
  const ksubject = trapi.getSubject(kedge);
  const predicate = edgeToQualifiedPredicate(kedge, doInvert);
  const kobject = trapi.getObject(kedge);
  const provenance = bl.inforesToProvenance(trapi.getPrimarySource(kedge));
  const knowledgeLevel = getKnowledgeLevel(kedge, provenance);
  if (doInvert) {
    return pathToKey([kobject, predicate, ksubject, knowledgeLevel]);
  }

  return pathToKey([ksubject, predicate, kobject, knowledgeLevel]);
}

function summarizeRnode(rnode, kgraph, nodeRules, context) {
  const rnodeKey = rnodeToKey(rnode, kgraph);
  return cmn.makePair(rnodeKey,
                      nodeRules(trapi.getKnowledgeNode(rnode, kgraph), context),
                      'key',
                      'transforms');
}

function summarizeRedge(redge, kgraph, edgeRules, context, edgeBaseKeys) {
  let edgeKey = redgeToKey(redge, kgraph);
  if (!edgeBaseKeys.has(edgeKey)) {
    edgeKey = redgeToKey(redge, kgraph, true);
  }

  return cmn.makePair(edgeKey,
    edgeRules(trapi.getKnowledgeEdge(redge, kgraph), context),
    'key',
    'transforms');
}

function makeRedgeToEdgeId(rgraph, kgraph) {
  function makeEdgeId(subject, object)
  {
    return cmn.makePair(subject, object, 'subject', 'object');
  }

  let redgeToEdgeId = {};
  rgraph.edges.forEach(redge => {
    const kedge = trapi.getKnowledgeEdge(redge, kgraph);
    cmn.jsonSet(redgeToEdgeId, redge, makeEdgeId(trapi.getSubject(kedge), trapi.getObject(kedge)));
  });

  return (redge) => { return cmn.jsonGet(redgeToEdgeId, redge); };
}

function makeRnodeToOutEdges(rgraph, kgraph) {

  function makeOutEdge(redge, node) {
    return cmn.makePair(redge, node, 'redge', 'target');
  }

  const redgeToEdgeId = makeRedgeToEdgeId(rgraph, kgraph);
  const rnodeToOutEdges = {};
  rnodeToOutEdges.update = (rnode, val) => {
    const outEdges = cmn.jsonGet(rnodeToOutEdges, rnode, []);
    outEdges.push(val);
    cmn.jsonSet(rnodeToOutEdges, rnode, outEdges);
  };

  rgraph.edges.forEach(redge => {
    const edgeId = redgeToEdgeId(redge);
    const subject = edgeId.subject;
    const object = edgeId.object;

    rnodeToOutEdges.update(subject, makeOutEdge(redge, object));
    rnodeToOutEdges.update(object, makeOutEdge(redge, subject));
  });

  return (rnode) => { return cmn.jsonGet(rnodeToOutEdges, rnode, []); };
}

function rgraphFold(proc, init, acc) {
  let objLeft = init;
  let res = acc;
  while (!cmn.isArrayEmpty(objLeft)) {
    const paths = proc(objLeft.pop());
    objLeft.push(...paths.first);
    res.push(...paths.second);
  }

  return res;
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

  pushScore(resultKey, scoringComponents) {
    const resultScores = cmn.jsonSetDefaultAndGet(this.scores, resultKey, []);
    resultScores.push(scoringComponents);
  }

  pushAgent(agent) {
    this.agents.push(agent);
  }

  pushError(agent, error) {
    const currentError = cmn.jsonSetDefaultAndGet(this.errors, agent, []);
    currentError.push(error);
  }

  merge(summaryFragment) {
    this.agents.push(...summaryFragment.agents);
    this.paths.push(...summaryFragment.paths);
    this.nodes.push(...summaryFragment.nodes);
    this.edges.merge(summaryFragment.edges);
    this._mergeFragmentObjects(this.scores, summaryFragment.scores);
    this._mergeFragmentObjects(this.errors, summaryFragment.errors);
    return this
  }

  _mergeFragmentObjects(obj1Prop, obj2Prop) {
    Object.keys(obj2Prop).forEach((k) => {
      const current = cmn.jsonSetDefaultAndGet(obj1Prop, k, []);
      current.push(...obj2Prop[k]);
    });
  }
}

class SummaryFragmentEdges {
  constructor(base, updates) {
    this.base = base || {};
    this.updates = updates || [];
  }

  isEmpty() {
    return cmn.isObjEmpty(this.base) && cmn.isArrayEmpty(this.updates);
  }

  merge(summaryFragmentEdges) {
    Object.keys(summaryFragmentEdges.base).forEach((edgeKey) => {
      const currentEdge = cmn.jsonSetDefaultAndGet(this.base, edgeKey, new SummaryEdge());
      currentEdge.merge(summaryFragmentEdges.base[edgeKey]);
    });

    this.updates.push(...summaryFragmentEdges.updates);
    return this;
  }
}

class SummaryNode {
  constructor(supportingAgents) {
    this.aras = supportingAgents || [];
    this.curies = [];
    this.descriptions = [];
    this.names = [];
    this.other_names = [];
    this.provenance = []
    this.tags = {}
    this.types = [];
  }

  get otherNames () { return this.other_names; }
  set otherNames (otherNames) { this.other_names = otherNames; }

  extendSupportingAgents(agents) {
    this.aras.push(...agents)
  }
}

class SummaryEdge {
  constructor(supportingAgents, supportPaths, isRootPath) {
    this.aras = supportingAgents || [];
    this.support = supportPaths || [];
    this.is_root = isRootPath || false;
    this.knowledge_level = null;
    this.subject = null;
    this.object = null;
    this.predicate = null;
    this.predicate_url = null;
    this.provenance = [];
    this.publications = {};
  }

  get isRootPath() { return this.is_root; }
  set isRootPath(isRootPath) { this.is_root = isRootPath; }
  get supportPaths() { return this.support; }
  set supportPaths(supportPaths) { this.support = supportPaths; }
  get knowledgeLevel() { return this.knowledge_level; }
  set knowledgeLevel(knowledgeLevel) { this.knowledge_level = knowledgeLevel; }
  get predicateUrl() { return this.predicate_url; }
  set predicateUrl(predicateUrl) { this.predicate_url = predicateUrl; }

  extendSupportingAgents(agents) {
    this.aras.push(...agents)
  }

  extendSupportPaths(paths) {
    this.supportPaths.push(...paths);
  }

  merge(summaryEdge) {
    this.extendSupportingAgents(summaryEdge.aras);
    this.extendSupportPaths(summaryEdge.supportPaths);
    this.isRootPath = this.isRootPath || summaryEdge.isRootPath;
    this.knowledgeLevel = this.knowledgeLevel || summaryEdge.knowledgeLevel;
    this.subject = this.subject || summaryEdge.subject;
    this.predicate = this.predicate || summaryEdge.predicate;
    this.object = this.object || summaryEdge.object;
    this.predicateUrl = this.predicateUrl || summaryEdge.predicateUrl;
    this.provenance.push(...summaryEdge.provenance);
    Object.keys(summaryEdge.publications).forEach((kl) => {
      if (!this.publications[kl]) {
        this.publications[kl] = [];
      }

      this.publications[kl].push(...summaryEdge.publications[kl]);
    });

    return this
  }
}

function ErrorSummaryFragment(agent, error) {
  const summaryFragment = new SummaryFragment();
  summaryFragment.pushAgent(agent);
  summaryFragment.pushError(agent, error);
  return summaryFragment;
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

function pathToKey(path) {
  return hash(path);
}

function answersToSummaryFragments(answers, nodeRules, edgeRules, maxHops) {
  function resultToSummaryFragment(result, kgraph, auxGraphs, startKey, endKey, errors) {
    function analysisToSummaryFragment(analysis, kgraph, auxGraphs, start, ends) {
      function finalizePaths(rgraphPaths, edgeMappings, kgraph) {
        function N(n) { return rnodeToKey(n, kgraph); }
        function E(e, o) { return redgeToKey(e, kgraph, isRedgeInverted(e, o, kgraph)); }
        const normalizedMappings = {};
        const normalizedPaths = rgraphPaths.map(path => {
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
              normalizedMappings[normalizedEdge] = { partOf: [], supportPaths: [] };
            }
            normalizedMappings[normalizedEdge].partOf.push(...edgeMappings[edge].partOf);
            normalizedMappings[normalizedEdge].supportPaths.push(...edgeMappings[edge].supportPaths);
            normalizedPath.push(N(node), normalizedEdge);
          }

          normalizedPath.push(N(path[pathLength]));
          return normalizedPath;
        });

        Object.keys(normalizedMappings).forEach(key => cmn.objRemoveDuplicates(normalizedMappings[key]));
        const pathToSupportGraph = {};
        // For every path find which graphs the path appears in. A path appears in a graph iff all
        // edges in the path appear in the graph.
        for (const path of normalizedPaths) {
          let gids = [...normalizedMappings[path[1]].partOf];
          for (let i = 3; i < path.length; i+=2) {
            gids = gids.filter((gid) => normalizedMappings[path[i]].partOf.includes(gid));
          }

          pathToSupportGraph[pathToKey(path)] = gids;
        }

        const edgeBases = {}
        // Determine which paths support which edges
        for (const edge of Object.keys(normalizedMappings)) {
          const edgeSupportGraphs = normalizedMappings[edge].supportPaths;
          const edgePaths = [];
          for (const path of Object.keys(pathToSupportGraph)) {
            for (const pgid of pathToSupportGraph[path]) {
              if (edgeSupportGraphs.includes(pgid)) {
                edgePaths.push(path);
                break;
              }
            }
          }

          if (!edgeBases[edge]) {
            edgeBases[edge] = new SummaryEdge();
          }

          edgeBases[edge].extendSupportPaths(edgePaths);
          edgeBases[edge].isRootPath = normalizedMappings[edge].partOf.includes('root');
        }

        return [normalizedPaths, edgeBases];
      }

      const agent = cmn.jsonGet(analysis, 'resource_id', false);
      if (!agent) {
        return ErrorSummaryFragment('unknown', 'Expected analysis to have resource_id');
      }

      try {
        const rgraph = analysisToRgraph(analysis, kgraph, auxGraphs);
        const rnodeToOutEdges = makeRnodeToOutEdges(rgraph, kgraph);
        const maxPathLength = (2 * maxHops) + 1;
        // This is an exhaustive search based on the max path length. We may have to come up
        // with a better algorithm if the max path length increases significantly.
        const rgraphPaths = rgraphFold((path) => {
          const currentRnode = path[path.length-1];
          if (maxPathLength < path.length) {
            return cmn.makePair([], []);
          }

          let validPaths = [];
          rnodeToOutEdges(currentRnode).forEach((edge) => {
            const target = edge.target
            // Do not allow cycles
            if (!path.includes(target)) {
              let newPath = [...path, edge.redge, edge.target];
              validPaths.push(newPath);
            }
          });

          const finalPaths = [];
          if (ends.includes(currentRnode)) {
            finalPaths.push(path);
          }

          return cmn.makePair(validPaths, finalPaths);
        },
        [[start]],
        []);

        const [normalizedPaths, edgeBases] = finalizePaths(rgraphPaths, rgraph.edgeMappings, kgraph);
        const analysisContext = {
          agent: agent,
          errors: errors
        };

        return new SummaryFragment(
          [agent],
          normalizedPaths,
          rgraph.nodes.map(rnode => { return summarizeRnode(rnode, kgraph, nodeRules, analysisContext); }),
          new SummaryFragmentEdges(
            edgeBases,
            rgraph.edges.map(redge => {
              const kedge = trapi.getKnowledgeEdge(redge, kgraph);
              const edgeContext = cmn.deepCopy(analysisContext);
              edgeContext.primarySource = trapi.getPrimarySource(kedge);
              return summarizeRedge(redge, kgraph,
                 edgeRules, edgeContext, new Set(Object.keys(edgeBases)));
            })
          ));
      } catch (err) {
        console.error(err);
        if (err instanceof EdgeBindingNotFoundError) {
          return ErrorSummaryFragment(agent, e.message);
        }

        return ErrorSummaryFragment(agent, 'Unknown error while building RGraph');
      }
    }

    try {
      const [rnodeStart, rnodeEnds] = getResultStartAndEnd(result, startKey, endKey);
      // TODO: There SHOULD only be a single start point. We should probably throw an error when this is not the case.
      const analyses = getResultAnalyses(result);
      const resultSummaryFragment = analyses.reduce(
        (rsf, analysis) => {
          return rsf.merge(analysisToSummaryFragment(analysis, kgraph, auxGraphs, rnodeStart, rnodeEnds));
        },
        new SummaryFragment());

      if (!resultSummaryFragment.isEmpty()) {
        // Ordering components are a property of the result, so we have to add them after the result analyses are summarized.
        const resultKey = rnodeToKey(rnodeStart, kgraph);
        const scoringComponents = getResultScoringComponents(result);
        resultSummaryFragment.pushScore(resultKey, scoringComponents);
      }

      return resultSummaryFragment;
    } catch (err) {
      console.error(err);
      if (err instanceof NodeBindingNotFoundError) {
        return ErrorSummaryFragment('unknown', err.message);
      }

      return ErrorSummaryFragment('unknown', 'Unknown error while building result summary fragment');
    }
  }

  const summaryFragments = [];
  const errors = {};
  answers.forEach((answer) => {
    const results = getAnswerResults(answer);
    if (!results) {
      // TODO: Add warning
      return;
    }

    const kgraph = getAnswerKnowledgeGraph(answer);
    const auxGraphs = getAnswerAuxiliaryGraphs(answer);
    const [startKey, endKey] = trapi.messageToEndpoints(answer);

    // TODO: Where is the error handling?
    results.forEach((result) => {
      const sf = resultToSummaryFragment(result, kgraph, auxGraphs, startKey, endKey, errors);
      // TODO: Empty summary fragments should throw an error (at least in some cases)
      if (!sf.isEmpty()) {
        summaryFragments.push(sf);
      }
    });
  });

  return [summaryFragments, errors];
}

async function summaryFragmentsToSummary(qid, summaryFragments, queryType, nodeAnnotationClient, errors) {
  function fragmentPathsToResultsAndPaths(fragmentPaths) {
    // TODO: use objects instead of arrays?
    let results = [];
    let paths = [];
    fragmentPaths.forEach((path) => {
      const pathKey = pathToKey(path);
      results.push(cmn.makePair(path[0], pathKey, 'start', 'pathKey'))
      paths.push(cmn.makePair(pathKey, path, 'key', 'path'))
    });

    return [results, paths];
  }

  function extendSummaryResults(results, newResults) {
    newResults.forEach((result) => {
      let existingResult = cmn.jsonSetDefaultAndGet(results, result.start, {});
      let paths = cmn.jsonSetDefaultAndGet(existingResult, 'paths', [])
      paths.push(result.pathKey);
    });
  }

  function extendSummaryPaths(paths, newPaths, agents) {
    newPaths.forEach((path) => {
      let existingPath = cmn.jsonGet(paths, path.key, false);
      if (existingPath) {
        cmn.jsonGet(existingPath, 'aras').concat(agents);
        return;
      }

      cmn.jsonSet(paths, path.key, {'subgraph': path.path, 'aras': agents});
    });
  }

  function extendSummaryGraphElement(objs, updates, agents, defaultValue) {
    updates.forEach((update) => {
      let obj = cmn.jsonSetDefaultAndGet(objs, update.key, defaultValue());
      update.transforms.forEach((transform) => {
        transform(obj);
        obj.aras.push(...agents);
      });
    });
  }

  function extendSummaryNodes(nodes, nodeUpdates, agents) {
    extendSummaryGraphElement(nodes, nodeUpdates, agents, () => new SummaryNode());
  }

  function extendSummaryEdges(edges, edgeFragments, agents) {
    Object.keys(edgeFragments.base).forEach((k) => {
      const edge = cmn.jsonSetDefaultAndGet(edges, k, new SummaryEdge());
      edge.merge(edgeFragments.base[k]);
    });

    extendSummaryGraphElement(edges, edgeFragments.updates, agents, () => new SummaryEdge());
  }

  function extendSummaryScores(scores, newScores) {
    Object.keys(newScores).forEach((resultNode) => {
      const currentScores = cmn.jsonSetDefaultAndGet(scores, resultNode, []);
      currentScores.push(...newScores[resultNode]);
    });
  }

  function extendSummaryErrors(errors, newErrors) {
    Object.keys(newErrors).forEach((agent) => {
      const currentErrors = cmn.jsonSetDefaultAndGet(errors, agent, []);
      currentErrors.push(...newErrors[agent]);
    });
  }

  function extendSummaryPublications(publications, edge)
  {
    function makePublicationObject(type, url, source)
    {
      return {
        'type': type,
        'url': url,
        'source': source
      };
    }

    const pubs = cmn.jsonGet(edge, 'publications', {});
    Object.keys(pubs).forEach((ks) => {
      const publicationData = cmn.jsonGet(pubs, ks, []);
      publicationData.forEach((pub) => {
        const id = pub.id;
        const [type, url] = ev.idToTypeAndUrl(id);
        cmn.jsonSet(publications, id, makePublicationObject(type, url, pub.source));
      });
    });
  }

  function edgesToEdgesAndPublications(edges) {
    function addInverseEdge(edges, edge) {
      const invertedPredicate = edgeToQualifiedPredicate(edge, true);
      const subject = cmn.jsonGet(edge, 'subject');
      const object = cmn.jsonGet(edge, 'object');
      const knowledgeLevel = cmn.jsonGet(edge, 'knowledge_level');

      const invertedEdgeKey = pathToKey([object, invertedPredicate, subject, knowledgeLevel]);
      let invertedEdge = cmn.deepCopy(edge);
      cmn.jsonMultiSet(invertedEdge,
                      [['subject', object],
                       ['object', subject],
                       ['predicate', invertedPredicate]]);

      const unqualifiedInvertedPredicate = bl.invertBiolinkPredicate(getSpecificPredicate(edge));
      invertedEdge.predicateUrl = bl.predicateToUrl(unqualifiedInvertedPredicate);
      delete invertedEdge['qualifiers'];
      edges[invertedEdgeKey] = invertedEdge;
    }

    const publications = {};
    Object.values(edges).forEach((edge) => {
      extendSummaryPublications(publications, edge);
      const edgePublications = cmn.jsonGet(edge, 'publications', {})
      const supportingText = cmn.jsonGet(edge, 'supporting_text', {});
      Object.keys(edgePublications).forEach((knowledgeLevel) => {
        edgePublications[knowledgeLevel] = edgePublications[knowledgeLevel].map((pub) => {
          return { id: pub.id, support: supportingText[pub.id] || null };
        });
      });
      delete edge['supporting_text'];
      addInverseEdge(edges, edge);
      cmn.jsonSet(edge, 'predicate_url', bl.predicateToUrl(getSpecificPredicate(edge)));
      cmn.jsonSet(edge, 'predicate', edgeToQualifiedPredicate(edge));
      delete edge['qualifiers'];
    });

    return [edges, publications];
  }

  function resultsToResultsAndTags(results, paths, nodes, edges, scores, errors) {
    function isPidLessThan(pid1, pid2) {
      const path1 = getPathFromPid(paths, pid1);
      const path2 = getPathFromPid(paths, pid2);
      const p1Len = path1.length;
      const p2Len = path2.length;
      if (p1Len === p2Len) {
        for (let i = 0; i < path1.length; i+=2) {
          if (path1[i] < path2[i]) {
            return -1;
          }
          else if (path1[i] > path2[i]) {
            return 1;
          }
        }

        return 0;
      }

      if (p1Len < p2Len) {
        return -1;
      }

      return 1;
    }

    function isRootPath(pid) {
      const path = getPathFromPid(paths, pid);
      return getPathFromPid(paths, pid).length === 3
             && edges[path[1]].isRootPath;
    }

    const usedTags = {};
    const expandedResults = [];
    for (const result of results) {
      const pids = cmn.jsonGet(result, 'paths');
      const rootPids = pids.filter(isRootPath);
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

      const subgraph = getPathFromPid(paths, rootPids[0]);
      const start = subgraph[0];
      const startNames = cmn.jsonGetFromKpath(nodes, [start, 'names']);
      const end = subgraph[subgraph.length-1];
      const tags = {};
      pids.forEach((pid) => {
        Object.keys(paths[pid].tags).forEach((tag) => {
          usedTags[tag] = paths[pid].tags[tag];
          tags[tag] = null;
        });
      });

      // Generate inferred/lookup tags for results
      rootPids.forEach((pid) => {
        const subgraph = getPathFromPid(paths, pid);
        let tag = 'pt:inf';
        if (!cmn.isArrayEmpty(edges[subgraph[1]].supportPaths)) {
          usedTags[tag] = makeTagDescription('Inferred');
          tags[tag] = null;
        } else {
          tag = 'pt:lkup';
          usedTags[tag] = makeTagDescription('Lookup');
          tags[tag] = null;
        }
      });

      expandedResults.push({
        'id': hash([start, end]),
        'subject': start,
        'drug_name': (cmn.isArrayEmpty(startNames)) ? start : startNames[0],
        'paths': rootPids.sort(isPidLessThan),
        'object': end,
        'scores': scores[start],
        'tags': tags
      });
    }

    return [expandedResults, usedTags];
  }

  function getPathFromPid(paths, pid) {
    return cmn.jsonGetFromKpath(paths, [pid, 'subgraph']);
  }

  let results = {};
  let paths = {};
  let nodes = {};
  let edges = {};
  let publications = {};
  let scores = {};
  let tags = [];
  summaryFragments.forEach((sf) => {
    const agents = sf.agents;;
    const [newResults, newPaths] = fragmentPathsToResultsAndPaths(sf.paths);

    extendSummaryResults(results, newResults);
    extendSummaryPaths(paths, newPaths, agents);
    extendSummaryNodes(nodes, sf.nodes, agents);
    extendSummaryEdges(edges, sf.edges, agents);
    extendSummaryScores(scores, sf.scores);
    extendSummaryErrors(errors, sf.errors);
  });

  results = Object.values(results).map(cmn.objRemoveDuplicates)
  const annotationMessage = trapi.nodeIdsToTrapiMessage(Object.keys(nodes));
  const annotationPromise = nodeAnnotationClient.annotateGraph(annotationMessage);
  function pushIfEmpty(arr, val) {
    if (cmn.isArrayEmpty(arr)) {
      arr.push(val);
    }
  };

  // Edge post-processing
  Object.keys(edges).forEach((ek) => {
    const edge = edges[ek];
    // Remove any empty edges. TODO: Why are these even here?
    if (Object.keys(edge).length === 2 && edge.aras !== undefined && edge.supportPaths !== undefined) {
      delete edges[ek];
      return;
    }

    // Remove any edges that have a missing subject, object, predicate, or provenance
    const edgeErrorReasons = reasonsForEdgeErrors(edge);
    if (edgeErrorReasons.length !== 0) {
      console.error(`Found invalid edge ${ek}. Reasons: ${JSON.stringify(edgeErrorReasons)}`);
      updateErrorsFromEdge(edge, errors, edgeErrorReasons);
      delete edges[ek];
      return;
    }

    // Remove any duplicates on all edge attributes
    cmn.objRemoveDuplicates(edge);

    // Remove duplicates from publications
    const publications = cmn.jsonGet(edge, 'publications', {});
    Object.keys(publications).forEach((kl) => {
      const klPublications = cmn.jsonGet(publications, kl, []);
      const seenIds = new Set();
      cmn.jsonSet(publications, kl, klPublications.filter((pub) => {
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
      delete edges[ek];
      return
    }

    // Populate knowledge level
    edge.knowledgeLevel = edge.provenance[0].knowledge_level;
  });

  [edges, publications] = edgesToEdgesAndPublications(edges);

  const metadataObject = new SummaryMetadata(qid, cmn.distinctArray(summaryFragments.map((sf) => {
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
            const description = bta.getDescription(annotations);
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
            const otherNames = bta.getNames(annotations);
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
          (annotations) => {
            const curies = bta.getCuries(annotations);
            if (curies === null) {
              return [];
            }

            return curies;
          }
        )
      ]
    );

    const resultNodeRules = makeExtractionRules(
      [
        tagAttribute(
          'biothings_annotations',
          (annotations) => {
            const fdaApproval = bta.getFdaApproval(annotations);
            if (fdaApproval === null) {
              return false;
            } else if (fdaApproval < 4) {
              const tags = [];
              if (fdaApproval > 0) {
                tags.push(makeTag(`fda:${fdaApproval}`, `Clinical Trial Phase ${fdaApproval}`));
              } else {
                tags.push(makeTag('fda:0', 'Not FDA Approved'));
              }

              return tags;
            } else {
              return makeTag(`fda:${fdaApproval}`, `FDA Approved`);
            }
          }
        ),
        tagAttribute(
          'biothings_annotations',
          (annotations, context) => {
            if (trapi.isGeneChemicalQuery(context.queryType)) return [];

            const chebiRoles = bta.getChebiRoles(annotations);
            if (chebiRoles === null) {
              return [];
            }

            return chebiRoles.map((role) => { return makeTag(`role:${role.id}`, cmn.titleize(role.name))});
          }
        ),
        renameAndTransformAttribute(
          'biothings_annotations',
          ['indications'],
          (annotations) => {
            const indications = bta.getDrugIndications(annotations);
            if (indications === null) {
              return [];
            }

            return indications;
          }
        )
        ]
      );


    const resultNodes = new Set();
    results.forEach((result) => {
        const ps = cmn.jsonGet(result, 'paths');
        ps.forEach((p) => {
          const subgraph = getPathFromPid(paths, p);
          resultNodes.add(subgraph[0]);
        });
      });

    const knodes = await annotationPromise;
    const kgraph = { 'nodes': knodes };
    const annotationContext = {
      agent: 'biothings-annotator',
      queryType: queryType,
      errors: {}
    };

    const nodeUpdates = Object.keys(knodes).map((rnode) => {
      return summarizeRnode(rnode, kgraph, nodeRules, annotationContext);
    });

    const resultNodeUpdates = [...resultNodes].map((rnode) => {
      return summarizeRnode(rnode, kgraph, resultNodeRules, annotationContext);
    });

    extendSummaryNodes(nodes, nodeUpdates.concat(resultNodeUpdates), ['biothings-annotator']);
    extendSummaryErrors(errors, annotationContext.errors);
  }
  catch (err) {
    console.error(err);
  }
  finally {
    // Node post-processing
    Object.keys(nodes).forEach((k) => {
      const node = nodes[k];
      node.curies.push(k);
      // Remove any duplicates on all node attributes
      cmn.objRemoveDuplicates(node);
      node.types.sort(bl.biolinkClassCmpFn);

      // Provide a CURIE as a default value if the node has no name
      const nodeNames = cmn.jsonGet(node, 'names');
      pushIfEmpty(nodeNames, k);

      cmn.jsonSet(node, 'provenance', [bl.curieToUrl(k)])

      // Add tag attribute to nodes that don't have one
      cmn.jsonSetDefaultAndGet(node, 'tags', []);
    });

    // Path post-processing
    Object.keys(paths).forEach((pk) => {
      const path = paths[pk];
      // Remove paths where there is an undefined node reference in the path
      for (let i = 0; i < path.subgraph.length; i += 2) {
        if (nodes[path.subgraph[i]] === undefined) {
          delete paths[pk];
          return;
        }
      }

      // Remove paths where there is an undefined edge reference in the path
      for (let i = 1; i < path.subgraph.length; i += 2) {
        if (edges[path.subgraph[i]] === undefined) {
          delete paths[pk];
          return;
        }
      }

      // Remove duplicates from every attribute on a path
      cmn.objRemoveDuplicates(path);

      if (trapi.isChemicalDiseaseQuery(queryType)) {
        // Consider the chemical indicated for the disease iff
        //   1. The chemical is marked as indicated for the disease
        //   2. The chemical has reached phase 4 approval from the FDA
        const start = nodes[path.subgraph[0]];
        if (start.indications !== undefined) {
          const startIndications = new Set(start.indications);
          const end = nodes[path.subgraph[path.subgraph.length-1]];
          const endMeshIds = end.curies.filter((curie) => { return curie.startsWith('MESH:'); });
          let indicatedFor = false;
          for (let i = 0; i < endMeshIds.length; i++) {
            if (startIndications.has(endMeshIds[i])) {
              indicatedFor = start.tags['fda:4'] !== undefined;
              break;
            }
          }

          if (indicatedFor) {
            start.tags['di:ind'] = makeTagDescription('In a clinical trial for indicated disease');
          } else {
            start.tags['di:not'] = makeTagDescription('Not in a clinical trial for indicated disease');
          }
        }

        cmn.jsonDelete(start, 'indications');
      }

      // Add tags for paths by processing nodes
      const tags = {};
      for (let i = 0; i < path.subgraph.length; ++i) {
        if (isNodeIndex(i)) {
          const node = nodes[path.subgraph[i]];
          if (node !== undefined) { // Remove me when result graphs are fixed
            // Take all node tags
            Object.keys(node.tags).forEach((k) => { tags[k] = node.tags[k]; });

            // Generate tags based on the node category
            const type = cmn.isArrayEmpty(node.types) ?
                         'Named Thing' :
                          bl.sanitizeBiolinkItem(node.types[0]);
            if (i === 0) {
              const [answerTag, answerDescription] = determineAnswerTag(type, node.tags, queryType);
              if (answerTag) {
                tags[answerTag] = makeTagDescription(answerDescription);
              }
            }

            tags[`pc:${type}`] = makeTagDescription(type);
          }
        }
      }

      // Generate tags based on the aras for this path
      const arasInfores = cmn.jsonGet(path, 'aras');
      arasInfores.forEach((infores) => {
        tags[`ara:${infores}`] = makeTagDescription(inforesToName(infores));
      });

      path.tags = tags;
    });

    // Remove PIDs that are no longer valid from results and support for edges
    Object.keys(edges).forEach((k) => {
      const edge = cmn.jsonGet(edges, k);
      edge.supportPaths = edge.supportPaths.filter(p => paths[p] !== undefined);
    });

    results.forEach((r) => {
      r.paths = r.paths.filter(p => paths[p] !== undefined);
    });

    // Remove results that no longer have any paths
    results = results.filter(r => r.paths.length > 0);

    [results, tags] = resultsToResultsAndTags(results, paths, nodes, edges, scores, errors);
    return {
      'meta': metadataObject,
      'results': results,
      'paths': paths,
      'nodes': nodes,
      'edges': edges,
      'publications': publications,
      'tags': tags,
      'errors': errors
    };
  }
}
function getAnswerResults(answer) {
  return cmn.jsonGetFromKpath(answer, ['message', 'results'], false);
}

function getAnswerKnowledgeGraph(answer) {
  return cmn.jsonGetFromKpath(answer, ['message', 'knowledge_graph']);
}

function getAnswerAuxiliaryGraphs(answer) {
  return cmn.jsonGetFromKpath(answer, ['message', 'auxiliary_graphs'], {});
}

function getAnswerQueryGraph(answer) {
  return cmn.jsonGetFromKpath(answer, ['message', 'query_graph']);
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