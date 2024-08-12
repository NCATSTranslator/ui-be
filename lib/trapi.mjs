'use strict';

import { default as hash } from 'hash-sum';
import * as cmn from './common.mjs';
import * as ev from './evidence.mjs';
import * as bl from './biolink-model.mjs';
import * as bta from './biothings-annotation.mjs';

const subjectKey = 'sn';
const objectKey = 'on';

export function makeMetadataObject(qid, agents) {
  if (qid === undefined || !cmn.isString(qid)) {
    throw new TypeError(`Expected argument qid to be of type string, got: ${qid}`);
  }

  if (agents === undefined || !cmn.isArray(agents)) {
    throw new TypeError(`Expected argument agents to be type array, got: ${agents}`);
  }

  return {
    'qid': qid,
    'aras': agents
  };
}

export function queryToCreativeQuery(query) {
  function buildCreativeQgraph(subject, object, predicate, direction) {
    function nodeToQgNode(node) {
      const qgNode = {};
      qgNode['categories'] = [bl.tagBiolink(node.category)];
      if (node.id) {
        qgNode['ids'] = [node.id];
      }

      return qgNode;
    }

    const qgNodes = {};
    qgNodes[subjectKey] = nodeToQgNode(subject);
    qgNodes[objectKey] = nodeToQgNode(object);

    const qgEdge = {
      'subject': subjectKey,
      'object': objectKey,
      'predicates': [bl.tagBiolink(predicate)],
      'knowledge_type': 'inferred',
    };

    if (direction) {
      qgEdge['qualifier_constraints'] = [
        {
          'qualifier_set': [
            {
              'qualifier_type_id': 'biolink:qualified_predicate',
              'qualifier_value': bl.tagBiolink('causes')
            },
            {
              'qualifier_type_id': 'biolink:object_aspect_qualifier',
              'qualifier_value': 'activity_or_abundance'
            },
            {
              'qualifier_type_id': 'biolink:object_direction_qualifier',
              'qualifier_value': direction
            }
          ]
        }
      ]
    }

    return {
      'nodes': qgNodes,
      'edges': {'t_edge': qgEdge}
    }
  }

  function diseaseToTrapiQgraph(disease) {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Disease', 'id': disease},
      'treats',
      null);
  }

  function geneToTrapiQgraph(gene, direction) {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Gene', 'id': gene},
      'affects',
      direction);
  }

  function chemicalToTrapiQgraph(chemical, direction) {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity', 'id': chemical},
      {'category': 'Gene'},
      'affects',
      direction);
  }

  if (!cmn.isObject(query)) {
    throw new TypeError(`Expected query to be type object, got: ${query}`);
  }

  const validKeys = ['type', 'curie', 'direction'];
  for (const key of validKeys) {
    if (!cmn.jsonHasKey(query, key)) {
      throw new ReferenceError(`Expected query to have key ${key}, got: ${query}`);
    }
  }

  let qg = null;
  const queryType = cmn.jsonGet(query, 'type');
  switch (queryType) {
    case 'drug':
      qg = diseaseToTrapiQgraph(cmn.jsonGet(query, 'curie'));
      break;
    case 'gene':
      qg = chemicalToTrapiQgraph(cmn.jsonGet(query, 'curie'), cmn.jsonGet(query, 'direction'));
      break;
    case 'chemical':
      qg = geneToTrapiQgraph(cmn.jsonGet(query, 'curie'), cmn.jsonGet(query, 'direction'));
      break;
    default:
      throw new RangeError(`Expected query type to be one of [drug, gene, chemical], got: ${queryType}`);
  }

  return {
    'message': {
      'query_graph': qg
    }
  };
}

export function creativeAnswersToSummary (qid, answers, maxHops) {
  const nodeRules = makeSummarizeRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAttributes([bl.tagBiolink('xref')], 'curies'),
      aggregateAttributes([bl.tagBiolink('description')], 'descriptions'),
    ]);

  const edgeRules = makeSummarizeRules(
    [
      transformProperty('predicate', (obj, key) => bl.sanitizeBiolinkItem(cmn.jsonGet(obj, key))),
      aggregateAndTransformProperty('sources', ['provenance'], (obj, key) => getPrimarySource(cmn.jsonGet(obj, key))),
      transformProperty('qualifiers', (obj, key) => cmn.jsonGet(obj, key, false)),
      getProperty('subject'),
      getProperty('object'),
      getPublications(),
      getSupportingText()
    ]);

  const queryType = answerToQueryType(answers[0]);
  function agentToName(agent) {
    return bl.inforesToProvenance(agent).name;
  }

  const [sfs, errors] = creativeAnswersToSummaryFragments(answers, nodeRules, edgeRules, maxHops);

  return summaryFragmentsToSummary(
    qid,
    sfs,
    answerToKGraph(answers[0]),
    queryType,
    agentToName,
    errors);
}

// Create a minimal TRAPI message for the annotator
function createKGFromNodeIds(nodeIds) {
  const nodes = {};
  nodeIds.forEach(id => {
    if (bl.isValidCurie(id)) {
      nodes[id] = {};
    }
  });

  const retval = {
    message: {
      knowledge_graph: {
        edges: {},
        nodes: nodes
      }
    }
  };

  return retval;
}

const QUERY_TYPE = {
  CHEMICAL_GENE: 0,
  CHEMICAL_DISEASE: 1,
  GENE_CHEMICAL: 2
}

function isChemicalGeneQuery(queryType) {
  return queryType === QUERY_TYPE.CHEMICAL_GENE;
}

function isChemicalDiseaseQuery(queryType) {
  return queryType === QUERY_TYPE.CHEMICAL_DISEASE;
}

function isGeneChemicalQuery(queryType) {
  return queryType === QUERY_TYPE.GENE_CHEMICAL;
}

function isValidQuery(queryType) {
  return Object.values(QUERY_TYPE).includes(queryType);
}

function answerToKGraph(answer) {
  const kg = cmn.jsonGetFromKpath(answer, ['message', 'knowledge_graph'], false);
  if (!kg) return {};
  return kg;
}

function answerToQueryType(answer) {
  const qg = cmn.jsonGetFromKpath(answer, ['message', 'query_graph'], false);
  if (!qg)
  {
    return false;
  }

  const [subjectKey, objectKey] = getPathDirection(qg);

  const subCategory = cmn.jsonGetFromKpath(qg, ['nodes', subjectKey, 'categories'], false)[0];
  const objCategory = cmn.jsonGetFromKpath(qg, ['nodes', objectKey, 'categories'], false)[0];
  if (subCategory === bl.tagBiolink('ChemicalEntity') &&
      objCategory === bl.tagBiolink('Gene')) {
    return QUERY_TYPE.CHEMICAL_GENE;
  }
  else if (subCategory === bl.tagBiolink('ChemicalEntity') &&
           objCategory === bl.tagBiolink('Disease')) {
    return QUERY_TYPE.CHEMICAL_DISEASE;
  }
  else if (subCategory === bl.tagBiolink('Gene') &&
           objCategory === bl.tagBiolink('ChemicalEntity')) {
    return QUERY_TYPE.GENE_CHEMICAL;
  }

  return false;
}

function makeMapping(key, transform, update, fallback) {
  return (obj, context) => {
    return (acc) => {
      try {
        const v = transform(obj, key, context);
        return update(v, acc);
      } catch (e) {
        const agentErrors = cmn.jsonSetDefaultAndGet(context.errors, context.agent, []);
        agentErrors.push(e.message);
        return update(fallback, acc);
      }
    }
  }
}

function aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate) {
  const cv = cmn.jsonGetFromKpath(obj, kpath, false);
  if (doUpdate(v)) {
    const uv = cmn.isArray(v) ? v : [v];
    return cmn.jsonSetFromKpath(obj, kpath, cv ? cv.concat(uv) : uv);
  }
  else if (cv) {
    return obj
  }
  else {
    return cmn.jsonSetFromKpath(obj, kpath, []);
  }
}

function aggregatePropertyUpdate(v, obj, kpath) {
  return aggregatePropertyUpdateWhen(v, obj, kpath, v => v !== null);
}

function renameAndTransformProperty(key, kpath, transform) {
  return makeMapping(
    key,
    transform,
    (v, obj) => { return cmn.jsonSetFromKpath(obj, kpath, v); },
    null);
}

function transformProperty(key, transform) {
  return renameAndTransformProperty(key, [key], transform);
}

function renameProperty(key, kpath) {
  return renameAndTransformProperty(key, kpath, (obj, key) => cmn.jsonGet(obj, key), null);
}

function getProperty(key) {
  return renameProperty(key, [key]);
}

function aggregatePropertyWhen(key, kpath, doUpdate) {
  return makeMapping(
    key,
    (obj, key) => cmn.jsonGet(obj, key),
    (v, obj) => { return aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate); },
    []);
}

function aggregateAndTransformProperty(key, kpath, transform) {
  return makeMapping(
    key,
    transform,
    (v, obj) => { return aggregatePropertyUpdate(v, obj, kpath); },
    []);
}

function aggregateProperty(key, kpath) {
  return aggregatePropertyWhen(key, kpath, v => v !== null);
}

function attrId(attribute) {
  return cmn.jsonGet(attribute, 'attribute_type_id');
}

function attrValue(attribute) {
  return cmn.jsonGet(attribute, 'value');
}

function attrAttributes(attribute) {
  return cmn.jsonGet(attribute, 'attributes');
}

function areNoAttributes(attributes) {
  return attributes === undefined || attributes === null || cmn.isArrayEmpty(attributes);
}

function renameAndTransformAttribute(attributeId, kpath, transform) {
  return makeMapping(
    'attributes',
    (obj, key) => {
      const attributes = cmn.jsonGet(obj, key, null);
      if (areNoAttributes(attributes)) {
        return null;
      }

      for (const attribute of attributes) {
        if (attributeId === attrId(attribute)) {
          return transform(attrValue(attribute));
        }
      }

      return null;
    },
    (v, obj) => {
      const currentValue = cmn.jsonGetFromKpath(obj, kpath, false);
      if (currentValue && v === null) {
        return obj;
      }

      return cmn.jsonSetFromKpath(obj, kpath, v);
    },
    null);
}

function aggregateAndTransformAttributes(attributeIds, tgtKey, transform) {
  return makeMapping(
    'attributes',
    (obj, key, context) => {
      const attributes = cmn.jsonGet(obj, key, null);
      if (areNoAttributes(attributes)) {
        return [];
      }

      const result = [];
      attributes.forEach(attribute => {
        if (attributeIds.includes(attrId(attribute))) {
          result.push(...transform(attrValue(attribute), context));
        }
      });

      return result;
    },
    (v, obj) => {
      const cv = cmn.jsonGet(obj, tgtKey, false);
      cmn.jsonSet(obj, tgtKey, cv ? cv.concat(v) : v);
    },
    []);
}

function aggregateAttributes(attributeIds, tgtKey) {
  return aggregateAndTransformAttributes(
    attributeIds,
    tgtKey,
    (v) => { return cmn.isArray(v) ? v : [v] });
}

function tagAttribute(attributeId, transform) {
  return makeMapping(
    'attributes',
    (obj, key, context) => {
      const attributes = obj[key];
      if (areNoAttributes(attributes)) {
        return [];
      }

      for (const attribute of attributes) {
        if (attributeId === attrId(attribute)) {
          return transform(attrValue(attribute), context);
        }
      }

      return [];
    },
    (vs, obj) => {
      const currentTags = cmn.jsonSetDefaultAndGet(obj, 'tags', {});
      if (!vs) {
        return obj;
      }

      if (!cmn.isArray(vs)) {
        vs = [vs];
      }

      vs.forEach((v) => {
        if (v && currentTags[v.tag] === undefined) {
          currentTags[v.tag] = v.description;
        }
      });

      return obj
    },
    null);
}

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

  return makeMapping(
    'attributes',
    (obj, key) => {
      const attributes = obj[key];
      if (areNoAttributes(attributes)) {
        return {};
      }

      const supportingText = {};
      attributes.forEach(attribute => {
        const innerAttrs = attrId(attribute) === searchAttrId ? attrAttributes(attribute) : [];
        const supportingTextData = {};
        innerAttrs.forEach(attribute => {
          const aid = attrId(attribute);
          const av = attrValue(attribute);
          if (aid === publicationsId) {
            supportingText[ev.sanitize(av)] = supportingTextData;
          } else if (aid === textId) {
            supportingTextData.text = av;
          } else if (aid === subjectTokenId) {
            supportingTextData.subject = av;
          } else if (aid === objectTokenId) {
            supportingTextData.object = av;
          }
        });

        if (!cmn.isObjectEmpty(supportingTextData)) {
          supportingTextData.subject = parseTokenIndex(supportingTextData.subject);
          supportingTextData.object = parseTokenIndex(supportingTextData.object);
        }
      });

      return supportingText;
    },
    (vs, obj) =>
    {
      const currentSupportingText = cmn.jsonSetDefaultAndGet(obj, 'supporting_text', {});
      Object.keys(vs).forEach((pid) => {
        currentSupportingText[pid] = vs[pid];
      });
    },
    {});
}

function getPublications() {
  const publicationIds = [
      bl.tagBiolink('supporting_document'),
      bl.tagBiolink('Publication'),
      bl.tagBiolink('publications'),
      bl.tagBiolink('publication') // Remove me when this is fixed in the ARA/KPs
  ];

  return makeMapping(
    'attributes',
    (obj, key, context) => {
      const attributes = obj[key];
      if (areNoAttributes(attributes)) {
        return {};
      }

      const result = {};
      const provenance = bl.inforesToProvenance(context.primarySource);
      const knowledgeLevel = getKnowledgeLevel(obj, provenance);
      attributes.forEach(attribute => {
        let v = (publicationIds.includes(attrId(attribute))) ? attrValue(attribute) : null;
        if (v !== null) {
          v = cmn.isArray(v) ? v : [v];
          if (!result[knowledgeLevel]) {
            result[knowledgeLevel] = [];
          }

          result[knowledgeLevel].push(...(v.map((pubId => {
            return {
              id: ev.sanitize(pubId),
              source: provenance
            };
          }))));
        };
      });

      return result;
    },
    (vs, obj) =>
    {
      const currentPublications = cmn.jsonSetDefaultAndGet(obj, 'publications', {});
      Object.keys(vs).forEach((knowledgeLevel) => {
        if (!currentPublications[knowledgeLevel]) {
          currentPublications[knowledgeLevel] = [];
        }

        currentPublications[knowledgeLevel].push(...(vs[knowledgeLevel]));
      });

      return obj;
    },
    {});
}

function getPrimarySource(sources) {
for (let source of sources) {
    const id = cmn.jsonGet(source, 'resource_id', false);
    const role = cmn.jsonGet(source, 'resource_role', false);
    if (!role || !id) {
      continue;
    }
    else if (role === 'primary_knowledge_source') {
      return [id];
    }
  }

  throw new Error('No primary knowledge source found');
}

function getKnowledgeLevel(kedge, provenance) {
  const attributes = cmn.jsonGet(kedge, 'attributes', []);
  if (areNoAttributes(attributes)) {
    return provenance.knowledge_level;
  }

  let rawKnowledgeLevel = null;
  let rawAgentType = null;
  for (const attribute of attributes) {
    if (attrId(attribute) === bl.tagBiolink('knowledge_level')) {
      rawKnowledgeLevel = attrValue(attribute);
    }

    if (attrId(attribute) === bl.tagBiolink('agent_type')) {
      rawAgentType = attrValue(attribute);
    }
  }

  if (rawAgentType === 'text_mining_agent') {
    return 'ml';
  } else if (rawKnowledgeLevel === 'knowledge_assertion') {
    return 'trusted';
  } else if (rawKnowledgeLevel === 'not_provided') {
    return 'unknown';
  } else if (rawKnowledgeLevel !== null) {
    return 'inferred';
  }

  return provenance.knowledge_level;
}

function makeSummarizeRules(rules) {
  return (obj, context) => {
    return rules.map(rule => { return rule(obj, context); });
  };
}

function trapiBindingToKobj(binding, type, kgraph) {
  return cmn.jsonGet(cmn.jsonGet(kgraph, type, {}), binding, false);
}

function redgeToTrapiKedge(edgeBinding, kgraph) {
  return trapiBindingToKobj(edgeBinding, 'edges', kgraph);
}

function rnodeToTrapiKnode(nodeBinding, kgraph) {
  return trapiBindingToKobj(nodeBinding, 'nodes', kgraph);
}

function getNodeBindingEndpoints(bindings, key) {
  const nodeBinding = cmn.jsonGet(bindings, key, false);
  if (!nodeBinding) {
    throw new NodeBindingNotFoundError(nodeBinding);
  }

  return nodeBinding.map((entry) => {
    let endpoint = cmn.jsonGet(entry, 'query_id', false);
    if (!endpoint) {
      endpoint = cmn.jsonGet(entry, 'id')
    }

    return endpoint;
  });
}

function flattenBindings(bindings) {
  return Object.values(bindings).reduce((ids, binding) => {
    return ids.concat(binding.map(obj => { return cmn.jsonGet(obj, 'id'); }));
  },
  []);
}

function kedgeSubject(kedge) {
  return cmn.jsonGet(kedge, 'subject');
}

function kedgeObject(kedge) {
  return cmn.jsonGet(kedge, 'object');
}

function kedgePredicate(kedge) {
  return cmn.jsonGet(kedge, 'predicate');
}

function isNodeIndex(index) {
  return index % 2 === 0;
}

function kedgeAttributes(kedge) {
  const attributes = cmn.jsonGet(kedge, 'attributes', null);
  if (areNoAttributes(attributes)) {
    return [];
  }

  return attributes;
}

function kedgeSupportGraphs(kedge) {
  const attributes = kedgeAttributes(kedge);
  for (const attr of attributes) {
    if (attrId(attr) === bl.tagBiolink('support_graphs')) {
      return attrValue(attr);
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
    const qualifierKeys = ['form or variant', 'direction', 'aspect', 'part', 'derivative'];
    const qualifierValues = qualifierKeys.map(key => cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false));

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

  function subjectQualifiersToString(qualifiers, prefixes) {
    return qualifiersToString('subject', qualifiers, prefixes);
  }

  function objectQualifiersToString(qualifiers, prefixes) {
    return qualifiersToString('object', qualifiers, prefixes);
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

  let predicate = bl.sanitizeBiolinkItem(kedgePredicate(kedge));
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

  predicate = bl.sanitizeBiolinkItem(getSpecificPredicate(kedge));
  const specialCase = getSpecialCase(predicate, qualifiers, invert);
  if (specialCase) {
    return specialCase;
  }

  const subjectPrefixes = ['of a', 'has', false, 'of the', false];
  const objectPrefixes = ['a', false, false, 'of the', false];
  if (invert) {
    const subjectQualifierStr = subjectQualifiersToString(qualifiers, objectPrefixes);
    const objectQualifierStr = objectQualifiersToString(qualifiers, subjectPrefixes);
    return finalizeQualifiedPredicate(objectQualifierStr,
                                      bl.invertBiolinkPredicate(predicate),
                                      subjectQualifierStr);
  }

  const subjectQualifierStr = subjectQualifiersToString(qualifiers, subjectPrefixes);
  const objectQualifierStr = objectQualifiersToString(qualifiers, objectPrefixes);
  return finalizeQualifiedPredicate(subjectQualifierStr,
                                    predicate,
                                    objectQualifierStr);
}

function makeTag(tag, name, description = '') {
  return {
    'tag': tag,
    'description': makeTagDescription(name, description)
  };
}

function makeTagDescription(name, description = '') {
  return {
    'name': name,
    'value': description
  };
}

function isResultTag(tag) {
  return tag.startsWith('role:') ||
         tag.startsWith('fda:') ||
         tag.startsWith('di:') ||
         tag.startsWith('cc:');
}

function determineAnswerTag(type, answerTags, queryType) {
  function isDrug(type, fdaLevel) {
    return fdaLevel === 4 || type === 'Drug';
  }

  function isClinicalPhase(fdaLevel) {
    return fdaLevel > 0 && fdaLevel < 4;
  }

  if (!isValidQuery(queryType) || isGeneChemicalQuery(queryType)) {
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
    const kedge = redgeToTrapiKedge(redge, kgraph);
    return bl.isBiolinkPredicate(kedgePredicate(kedge));
  });
  rgraph.edgeMappings = edgeMappings;

  return rgraph;
}

function isRedgeInverted(redge, subject, kgraph) {
  const kedge = redgeToTrapiKedge(redge, kgraph);
  return subject === kedgeObject(kedge);
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
      if (edgeBindingData[eb].support !== undefined) {
        continue;
      }

      const kedge = redgeToTrapiKedge(eb, kgraph);
      if (!kedge) {
        throw new EdgeBindingNotFoundError(eb);
      }

      nodeBindings.add(kedgeSubject(kedge));
      nodeBindings.add(kedgeObject(kedge));
      const edgeSupportGraphs = kedgeSupportGraphs(kedge);
      edgeBindingData[eb].support = edgeSupportGraphs;
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
  const kedge = redgeToTrapiKedge(redge, kgraph);
  const ksubject = kedgeSubject(kedge);
  const predicate = edgeToQualifiedPredicate(kedge, doInvert);
  const kobject = kedgeObject(kedge);
  const isInferred = !cmn.isArrayEmpty(kedgeSupportGraphs(kedge));
  const provenance = bl.inforesToProvenance(getPrimarySource(cmn.jsonGet(kedge, 'sources'))[0]);
  const knowledgeLevel = getKnowledgeLevel(kedge, provenance);
  if (doInvert) {
    return pathToKey([kobject, predicate, ksubject, knowledgeLevel, isInferred]);
  }

  return pathToKey([ksubject, predicate, kobject, knowledgeLevel, isInferred]);
}

function summarizeRnode(rnode, kgraph, nodeRules, context) {
  const rnodeKey = rnodeToKey(rnode, kgraph);
  return cmn.makePair(rnodeToKey(rnode, kgraph),
    nodeRules(rnodeToTrapiKnode(rnode, kgraph), context),
    'key',
    'transforms');
}

function summarizeRedge(redge, kgraph, edgeRules, context, edgeBaseKeys) {
  let edgeKey = redgeToKey(redge, kgraph);
  if (!edgeBaseKeys.has(edgeKey)) {
    edgeKey = redgeToKey(redge, kgraph, true);
  }

  return cmn.makePair(edgeKey,
    edgeRules(redgeToTrapiKedge(redge, kgraph), context),
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
    const kedge = redgeToTrapiKedge(redge, kgraph);
    cmn.jsonSet(redgeToEdgeId, redge, makeEdgeId(kedgeSubject(kedge), kedgeObject(kedge)));
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

function makeSummaryFragment(agents, paths, nodes, edges, scores, errors) {
  const summaryFragment = {};
  summaryFragment.agents = agents;
  summaryFragment.paths = paths;
  summaryFragment.nodes = nodes;
  summaryFragment.edges = edges;
  summaryFragment.scores = scores;
  summaryFragment.errors = errors;
  return summaryFragment;
}

function emptySummaryFragment() {
  return makeSummaryFragment(
    [], // agents
    [], // paths
    [], // nodes
    {base: {}, updates: []}, //edges
    {}, // scores
    {}  // errors
  );
}

function errorSummaryFragment(agent, error) {
  const summaryFragment = emptySummaryFragment();
  summaryFragment.agents = [agent];
  summaryFragment.errors[agent] = [error];
  return summaryFragment;
}

function isEmptySummaryFragment(summaryFragment) {
  return cmn.isArrayEmpty(summaryFragment.paths) &&
         cmn.isArrayEmpty(summaryFragment.nodes) &&
         cmn.isObjectEmpty(summaryFragment.edges.base) &&
         cmn.isArrayEmpty(summaryFragment.edges.updates);
}

function condensedSummaryAgents(condensedSummary) {
  return condensedSummary.agents;
}

function condensedSummaryPaths(condensedSummary) {
  return condensedSummary.paths;
}

function condensedSummaryNodes(condensedSummary) {
  return condensedSummary.nodes;
}

function condensedSummaryEdges(condensedSummary) {
  return condensedSummary.edges;
}

function condensedSummaryScores(condensedSummary) {
  return condensedSummary.scores;
}

function condensedSummaryErrors(condensedSummary) {
  return condensedSummary.errors;
}

function makeEdgeBase() {
  return {
    aras: [],
    support: [],
    is_root: false
  };
}

function mergeEdgeBase(eb1, eb2) {
  eb1.aras.push(...eb2.aras);
  eb1.support.push(...eb2.support);
  eb1.is_root = eb1.is_root || eb2.is_root;
  return eb1
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

function mergeFragmentObjects(obj1, obj2) {
  Object.keys(obj2).forEach((k) => {
    const current = cmn.jsonSetDefaultAndGet(obj1, k, []);
    current.push(...obj2[k]);
  });
}

function mergeSummaryFragments(f1, f2) {
  f1.agents.push(...f2.agents);
  f1.paths.push(...f2.paths);
  f1.nodes.push(...f2.nodes);
  f1.edges.updates.push(...f2.edges.updates);
  Object.keys(f2.edges.base).forEach((ek) => {
    const currentEdge = cmn.jsonSetDefaultAndGet(f1.edges.base, ek, makeEdgeBase());
    mergeEdgeBase(currentEdge, f2.edges.base[ek]);
  });
  mergeFragmentObjects(f1.scores, f2.scores);
  mergeFragmentObjects(f1.errors, f2.errors);
  return f1;
}

function getPathDirection(qgraph) {
  const startIsObject = cmn.jsonGetFromKpath(qgraph, ['nodes', subjectKey, 'ids'], false);
  if (startIsObject) {
    return [objectKey, subjectKey];
  }

  return [subjectKey, objectKey];
}

function creativeAnswersToSummaryFragments(answers, nodeRules, edgeRules, maxHops) {
  function trapiResultToSummaryFragment(trapiResult, kgraph, auxGraphs, startKey, endKey, errors) {
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
              normalizedMappings[normalizedEdge] = { partOf: [], support: [] };
            }
            normalizedMappings[normalizedEdge].partOf.push(...edgeMappings[edge].partOf);
            normalizedMappings[normalizedEdge].support.push(...edgeMappings[edge].support);
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
          const edgeSupportGraphs = normalizedMappings[edge].support;
          const edgePaths = [];
          for (const path of Object.keys(pathToSupportGraph)) {
            for (const pgid of pathToSupportGraph[path]) {
              if (edgeSupportGraphs.includes(pgid)) {
                edgePaths.push(path);
                break;
              }
            }
          }

          if (edgeBases[edge] === undefined) {
            edgeBases[edge] = makeEdgeBase();
          }

          edgeBases[edge].support.push(...edgePaths);
          edgeBases[edge].is_root = normalizedMappings[edge].partOf.includes('root');
        }

        return [normalizedPaths, edgeBases];
      }

      const agent = cmn.jsonGet(analysis, 'resource_id', false);
      if (!agent) {
        return errorSummaryFragment('unknown', 'Expected analysis to have resource_id');
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

        return makeSummaryFragment(
          [agent],
          normalizedPaths,
          rgraph.nodes.map(rnode => { return summarizeRnode(rnode, kgraph, nodeRules, analysisContext); }),
          {
            base: edgeBases,
            updates: rgraph.edges.map(redge => {
                       const kedge = redgeToTrapiKedge(redge, kgraph);
                       const edgeContext = cmn.deepCopy(analysisContext);
                       edgeContext.primarySource = getPrimarySource(cmn.jsonGet(kedge, 'sources'))[0];
                       return summarizeRedge(redge, kgraph,
                          edgeRules, edgeContext, new Set(Object.keys(edgeBases)));
            })
          },
          {},
          {});
      } catch (err) {
        console.error(err);
        if (err instanceof EdgeBindingNotFoundError) {
          return errorSummaryFragment(agent, e.message);
        }

        return errorSummaryFragment(agent, 'Unknown error with building RGraph');
      }
    }

    try {
      const resultNodeBindings = cmn.jsonGet(trapiResult, 'node_bindings');
      const start = getNodeBindingEndpoints(resultNodeBindings, startKey)[0];
      const ends = getNodeBindingEndpoints(resultNodeBindings, endKey);
      const analyses = cmn.jsonGet(trapiResult, 'analyses');
      const resultSummaryFragment = analyses.reduce(
        (rsf, analysis) => {
          return mergeSummaryFragments(
            rsf,
            analysisToSummaryFragment(analysis, kgraph, auxGraphs, start, ends));
        },
        emptySummaryFragment());

      if (!isEmptySummaryFragment(resultSummaryFragment)) {
        // Insert the ordering components after the analyses have been merged
        const resultStartKey = rnodeToKey(start, kgraph);
        const scoringComponents = cmn.jsonGet(trapiResult, 'ordering_components', {confidence: 0, novelty: 0, clinical_evidence: 0});
        const normalizedScore = cmn.jsonGet(trapiResult, 'normalized_score', 0);
        scoringComponents['normalized_score'] = normalizedScore;
        resultSummaryFragment.scores[resultStartKey] = [scoringComponents];
      }

      return resultSummaryFragment;
    } catch (e) {
      if (e instanceof NodeBindingNotFoundError) {
        return errorSummaryFragment('unknown', e.message);
      }

      return errorSummaryFragment('unknown', 'Unknown error while building result summary fragment');
    }
  }

  const summaryFragments = [];
  const errors = {};
  answers.forEach((answer) => {
    const trapiMessage = answer.message;
    const trapiResults = cmn.jsonGet(trapiMessage, 'results', false);
    if (!trapiResults) {
      return;
    }

    const kgraph = cmn.jsonGet(trapiMessage, 'knowledge_graph');
    const auxGraphs = cmn.jsonGet(trapiMessage, 'auxiliary_graphs', {});
    const [startKey, endKey] = getPathDirection(cmn.jsonGet(trapiMessage, 'query_graph'));

    trapiResults.forEach((result) => {
      const sf = trapiResultToSummaryFragment(result, kgraph, auxGraphs, startKey, endKey, errors);
      if (!isEmptySummaryFragment(sf)) {
        summaryFragments.push(sf);
      }
    });
  });

  return [summaryFragments, errors];
}

function getPathFromPid(paths, pid) {
  return cmn.jsonGetFromKpath(paths, [pid, 'subgraph']);
}

function sortPaths(pids, paths) {
  function isPidLessThan(pid1, pid2) {
    if (pid1 === undefined || pid2 === undefined) return 0;
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

      if (pid1 < pid2) {
        return -1;
      } else if (pid2 < pid1) {
        return 1;
      }

      return 0;
    }

    if (p1Len < p2Len) {
      return -1;
    }

    return 1;
  }

  if (pids.length < 2) return pids;
  return pids.sort(isPidLessThan);
}

function isRootPath(pid, paths, edges) {
  const path = getPathFromPid(paths, pid);
  return getPathFromPid(paths, pid).length === 3
         && edges[path[1]].is_root;
}

function getRootPids(pids, paths, edges) {
  const rootPids = pids.filter(pid => isRootPath(pid, paths, edges));
  return rootPids;
}

function generateSupportChain(pids, paths, edges) {
  const seenPids = [];
  const remaining = getRootPids(pids, paths, edges);
  while (remaining.length !== 0) {
    const next = remaining.pop();
    if (seenPids.includes(next)) continue;
    seenPids.push(next);
    const summaryPath = getPathFromPid(paths, next);
    for (let i = 1; i < summaryPath.length; i+=2) {
      const eid = summaryPath[i];
      const edgeSupport = edges[eid].support;
      remaining.push(...edgeSupport.filter((spid) => !seenPids.includes(spid)));
    }
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
      const path = getPathFromPid(paths, pid);
      if (path.length !== 0) {
        resPaths.push(pid);
        seenPaths.add(pid);
      }

      for (let i = 0; i < path.length; i++) {
        if (isNodeIndex(i)) {
          seenNodes.add(path[i]);
        } else {
          seenEdges.add(path[i]);
        }
      }
    }

    res.paths = resPaths;
  }

  clean(paths, seenPaths);
  clean(edges, seenEdges);
  clean(nodes, seenNodes);
}

async function summaryFragmentsToSummary(qid, condensedSummaries, kgraph, queryType, agentToName, errors) {
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

  function extendSummaryObj(objs, updates, agents, fallback) {
    updates.forEach((update) => {
      let obj = cmn.jsonSetDefaultAndGet(objs, update.key, fallback());
      update.transforms.forEach((transform) => {
        transform(obj);
        obj.aras.push(...agents);
      });
    });
  }

  function extendSummaryNodes(nodes, nodeUpdates, agents) {
    extendSummaryObj(nodes, nodeUpdates, agents, () => new Object({aras: []}));
  }

  function extendSummaryEdges(edges, edgeUpdates, agents) {
    extendSummaryObj(edges, edgeUpdates, agents, makeEdgeBase);
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
      cmn.jsonSet(invertedEdge, 'predicate_url', bl.predicateToUrl(unqualifiedInvertedPredicate));
      delete invertedEdge['qualifiers'];
      edges[invertedEdgeKey] = invertedEdge;
    }

    const publications = {};
    Object.values(edges).forEach((edge) => {
      extendSummaryPublications(publications, edge);
      const edgePublications = cmn.jsonGet(edge, 'publications', {});
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
    function isRootPath(pid, paths, edges) {
      const path = getPathFromPid(paths, pid);
      return getPathFromPid(paths, pid).length === 3
             && edges[path[1]].is_root;
    }

    function supportChainIncludesPid(rootPids, paths, edges, pid) {
      if (isRootPath(pid, paths, edges)) return true;

      const seenPids = new Set();
      const pids = [...rootPids];
      while (pids.length > 0) {
        const validPid = pids.pop();
        seenPids.add(validPid);
        const path = getPathFromPid(paths, validPid);
        for (let i = 1; i < path.length; i+=2) {
          const edgeSupport = edges[path[i]].support;
          if (edgeSupport.includes(pid)) return true;
          pids.push(...edgeSupport.filter((spid) => !seenPids.has(spid)));
        }
      }

      return false;
    }

    const usedTags = {};
    const expandedResults = [];
    for (const result of results) {
      const pids = cmn.jsonGet(result, 'paths');
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

      const subgraph = getPathFromPid(paths, rootPids[0]);
      const start = subgraph[0];
      const startNames = cmn.jsonGetFromKpath(nodes, [start, 'names']);
      const end = subgraph[subgraph.length-1];
      const tags = {};
      pids.forEach((pid) => {
        // Include the tags if the path is a root path or appears in the chain of support for a root path
        if (supportChainIncludesPid(rootPids, paths, edges, pid)) {
          const path = paths[pid];
          for (const tag of Object.keys(path.tags)) {
            if (isResultTag(tag) && path.subgraph[0] !== start) {
              continue;
            }

            usedTags[tag] = path.tags[tag];
            tags[tag] = null;
          }
        }
      });

      // Generate inferred/lookup tags for results
      rootPids.forEach((pid) => {
        const subgraph = getPathFromPid(paths, pid);
        let tag = 'pt:inf';
        if (!cmn.isArrayEmpty(edges[subgraph[1]].support)) {
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
        'paths': sortPaths(rootPids, paths),
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
  let publications = {};
  let scores = {};
  let tags = [];
  condensedSummaries.forEach((cs) => {
    const agents = condensedSummaryAgents(cs);
    const [newResults, newPaths] = fragmentPathsToResultsAndPaths(condensedSummaryPaths(cs));
    const summaryEdges = condensedSummaryEdges(cs);

    extendSummaryResults(results, newResults);
    extendSummaryPaths(paths, newPaths, agents);
    extendSummaryNodes(nodes, condensedSummaryNodes(cs), agents);
    Object.keys(summaryEdges.base).forEach((k) => {
      const edge = cmn.jsonSetDefaultAndGet(edges, k, makeEdgeBase());
      mergeEdgeBase(edge, summaryEdges.base[k]);
    });

    extendSummaryEdges(edges, summaryEdges.updates, agents);
    extendSummaryScores(scores, condensedSummaryScores(cs));
    extendSummaryErrors(errors, condensedSummaryErrors(cs));
  });

  results = Object.values(results).map(cmn.objRemoveDuplicates)
  function pushIfEmpty(arr, val) {
    if (cmn.isArrayEmpty(arr)) {
      arr.push(val);
    }
  };

  // Edge post-processing
  Object.keys(edges).forEach((ek) => {
    const edge = edges[ek];
    // Remove any empty edges. TODO: Why are these even here?
    if (Object.keys(edge).length === 2 && edge.aras !== undefined && edge.support !== undefined) {
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
    edge.knowledge_level = edge.provenance[0].knowledge_level;
  });

  [edges, publications] = edgesToEdgesAndPublications(edges);
  const metadataObject = makeMetadataObject(qid, cmn.distinctArray(condensedSummaries.map((cs) => { return cs.agents; }).flat()));
  try {
    // Node annotation
    const nodeRules = makeSummarizeRules(
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
              return [];
            }

            return otherNames;
          }
        ),
        aggregateAndTransformAttributes(
          ['biothings_annotations'],
          'curies',
          (annotations) => {
            const curies = bta.getCuries(annotations);
            return curies;
          }
        )
      ]
    );

    const resultNodeRules = makeSummarizeRules(
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
            if (isGeneChemicalQuery(context.queryType)) return [];

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

    const annotationContext = {
      agent: 'biothings-annotator',
      queryType: queryType,
      errors: {}
    };

    const nodeUpdates = Object.keys(nodes).map((rnode) => {
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

      // Provide a CURIE as a fallback if the node has no name
      const nodeNames = cmn.jsonGet(node, 'names');
      pushIfEmpty(nodeNames, k);

      cmn.jsonSet(node, 'provenance', [bl.curieToNormalizedUrl(k, node.curies)])

      // Add tag attribute to nodes that don't have one
      cmn.jsonSetDefaultAndGet(node, 'tags', {});

      if (cmn.jsonGet(node, 'other_names') === null) {
        cmn.jsonSet(node, 'other_names', []);
      }
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

      if (isChemicalDiseaseQuery(queryType)) {
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
            // Merge all global node tags into the path
            Object.keys(node.tags).filter((tag) => !isResultTag(tag)).forEach((k) => { tags[k] = node.tags[k]; });

            // Generate tags based on the node category
            const type = cmn.isArrayEmpty(node.types) ?
                         'Named Thing' :
                          bl.sanitizeBiolinkItem(node.types[0]);
            if (i === 0) {
              // Merge result level node tags
              Object.keys(node.tags).filter(isResultTag).forEach((k) => { tags[k] = node.tags[k]; });
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
      const aras = cmn.jsonGet(path, 'aras');
      aras.forEach((ara) => {
        tags[`ara:${ara}`] = makeTagDescription(agentToName(ara));
      });

      // Generate tags for path length
      const pathLength = (path.subgraph.length-1)/2;
      let tagDescription = 'Connections';
      if (pathLength == 1) {
        tagDescription = 'Connection';
      }

      tags[`pt:${pathLength}`] = makeTagDescription(`${pathLength} ${tagDescription}`);
      path.tags = tags;
    });

    // Remove PIDs that are no longer valid from results and support for edges and sort
    // support paths
    Object.keys(edges).forEach((k) => {
      edges[k].support = edges[k].support.filter(p => paths[p] !== undefined);
      edges[k].support = sortPaths(edges[k].support, paths);
    });

    results.forEach((r) => {
      r.paths = r.paths.filter(p => paths[p] !== undefined);
      r.paths = generateSupportChain(r.paths, paths, edges);
    });

    // Remove all unneeded items from results, paths, edges and nodes
    cleanup(results, paths, edges, nodes);

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
