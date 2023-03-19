'use strict';

import { default as hash } from 'hash-sum';
import * as cmn from './common.mjs';
import * as ev from './evidence.mjs';
import * as bl from './biolink-model.mjs';

const subjectKey = 'sn';
const objectKey = 'on';

export function makeMetadataObject(qid, agents)
{
  if (qid === undefined || !cmn.isString(qid))
  {
    throw new TypeError(`Expected argument qid to be of type string, got: ${qid}`);
  }

  if (agents === undefined || !cmn.isArray(agents))
  {
    throw new TypeError(`Expected argument agents to be type array, got: ${agents}`);
  }

  return {
    'qid': qid,
    'aras': agents
  };
}

export function queryToCreativeQuery(query)
{
  function buildCreativeQgraph(subject, object, predicate, direction)
  {
    function nodeToQgNode(node)
    {
      const qgNode = {};
      qgNode['categories'] = [bl.tagBiolink(node.category)];
      if (node.id)
      {
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

    if (direction)
    {
      qgEdge['qualifier_constraints'] = [
        {
          'qualifier_set': [
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

  function diseaseToTrapiQgraph(disease)
  {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Disease', 'id': disease},
      'treats',
      null);
  }

  function geneToTrapiQgraph(gene, direction)
  {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Gene', 'id': gene},
      'affects',
      direction);
  }

  function chemicalToTrapiQgraph(chemical, direction)
  {
    return buildCreativeQgraph(
      {'category': 'ChemicalEntity', 'id': chemical},
      {'category': 'Gene'},
      'affects',
      direction);
  }

  if (!cmn.isObj(query))
  {
    throw new TypeError(`Expected query to be type object, got: ${query}`);
  }

  const validKeys = ['type', 'curie', 'direction'];
  for (const key of validKeys)
  {
    if (!cmn.jsonHasKey(query, key))
    {
      throw new ReferenceError(`Expected query to have key ${key}, got: ${query}`);
    }
  }

  let qg = null;
  const queryType = cmn.jsonGet(query, 'type');
  switch (queryType)
  {
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

export function creativeAnswersToSummary (qid, answers, maxHops, canonPriority, annotationClient)
{
  const resultNodes = answers.map((answer) =>
    {
      return cmn.jsonGetFromKpath(answer.message, ['knowledge_graph', 'nodes']);
    });
  const nodeRules = makeSummarizeRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAttributes([bl.tagBiolink('xref')], 'curies'),
      tagFdaApproval,
      aggregateAttributes([bl.tagBiolink('description')], 'descriptions'),
      aggregateAttributes([bl.tagBiolink('synonym')], 'synonyms'),
      aggregateAttributes([bl.tagBiolink('same_as')], 'same_as'),
      aggregateAttributes([bl.tagBiolink('IriType')], 'iri_types')
    ]);

  const edgeRules = makeSummarizeRules(
    [
      transformProperty('predicate', bl.sanitizeBiolinkPredicate),
      getProperty('qualifiers'),
      getProperty('subject'),
      getProperty('object'),
      aggregateAttributes([bl.tagBiolink('IriType')], 'iri_types'),
      aggregateAttributes(['bts:sentence'], 'snippets'),
      aggregateAttributes([bl.tagBiolink('primary_knowledge_source')],
                          'provenance'),
      aggregateAndTransformAttributes(
        [
          bl.tagBiolink('supporting_document'),
          bl.tagBiolink('Publication'),
          bl.tagBiolink('publications')
        ],
        'publications',
        (evidence) =>
        {
          if (cmn.isArray(evidence))
          {
            return evidence;
          }

          // Split on ',' OR (|) '|'
          return evidence.split(/,|\|/);
        })
    ]);

  return condensedSummariesToSummary(
    qid,
    creativeAnswersToCondensedSummaries(
      answers,
      nodeRules,
      edgeRules,
      maxHops),
    annotationClient);
}

function createKGFromNodeIds(nodeIds, attributes)
{
  const nodes = {};
  nodeIds.forEach(e => { nodes[e] = {}; });
  const retval = {
    submitter: 'annotate_nodes',
    workflow: [ {
      id: 'annotate_nodes',
      parameters: { attributes: attributes }
    }],
    message: {
      knowledge_graph: {
        edges: {},
        nodes: nodes
      }
    }
  };
  return retval;
}

function makeMapping(key, transform, update, fallback)
{
  return (obj) =>
  {
    const val = cmn.jsonGet(obj, key, false);
    return (acc) => { return update((val ? transform(val) : fallback), acc); }
  }
}

function aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate)
{
  const cv = cmn.jsonGetFromKpath(obj, kpath, false);
  if (doUpdate(v))
  {
    const uv = cmn.isArray(v) ? v : [v];
    return cmn.jsonSetFromKpath(obj, kpath, cv ? cv.concat(uv) : uv);
  }
  else if (cv)
  {
    return obj
  }
  else
  {
    return cmn.jsonSetFromKpath(obj, kpath, []);
  }
}

function aggregatePropertyUpdate(v, obj, kpath)
{
  return aggregatePropertyUpdateWhen(v, obj, kpath, (v) => { return true; });
}

function renameAndTransformProperty(key, kpath, transform)
{
  return makeMapping(
    key,
    transform,
    (v, obj) => { return cmn.jsonSetFromKpath(obj, kpath, v); },
    null);
}

function transformProperty(key, transform)
{
  return renameAndTransformProperty(key, [key], transform);
}

function renameProperty(key, kpath)
{
  return renameAndTransformProperty(key, kpath, cmn.identity);
}

function getPropertyWhen(key, kpath, doUpdate)
{
  return makeMapping(
    key,
    cmn.identity,
    (v, obj) =>
    {
      const cv = cmn.jsonGet(obj, key, false);
      if (update(v))
      {
        return cmn.jsonSet(obj, key, v);
      }
      else if (cv)
      {
        return obj;
      }

      return cmn.jsonSet(obj, key, null);
    },
    null);
}

function getProperty(key)
{
  return renameProperty(key, [key]);
}

function aggregatePropertyWhen(key, kpath, doUpdate)
{
  return makeMapping(
    key,
    cmn.identity,
    (v, obj) => { return aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate); },
    []);
}

function aggregateAndTransformProperty(key, kpath, transform)
{
  return makeMapping(
    key,
    transform,
    (v, obj) => { return aggregatePropertyUpdate(v, obj, kpath); },
    []);
}

function aggregateProperty(key, kpath)
{
  return aggregatePropertyWhen(key, kpath, v => true);
}

function attrId(attribute)
{
  return cmn.jsonGet(attribute, 'attribute_type_id');
}

function attrValue(attribute)
{
  return cmn.jsonGet(attribute, 'value');
}

function areNoAttributes(attributes)
{
  return attributes === undefined || attributes === null || cmn.isArrayEmpty(attributes);
}

function renameAndTransformAttribute(attributeId, kpath, transform)
{
  return makeMapping(
    'attributes',
    (attributes) =>
    {
      if (areNoAttributes(attributes))
      {
        return null;
      }

      for (const attribute of attributes)
      {
        if (attributeId === attrId(attribute))
        {
          return transform(attrValue(attribute));
        }
      }

      return null;
    },
    (v, obj) => { return cmn.jsonSetFromKpath(obj, kpath, v); },
    null);
}

function renameAttribute(attributeId, kpath)
{
  return renameAndTransformAttribute(attributeId, kpath, cmn.identity);
}

function aggregateAndTransformAttributes(attributeIds, tgtKey, transform)
{
  return makeMapping(
    'attributes',
    (attributes) =>
    {
      const result = [];
      if (areNoAttributes(attributes))
      {
        return result;
      }

      attributes.forEach(attribute =>
        {
          const v = (attributeIds.includes(attrId(attribute))) ? attrValue(attribute) : [];
          result.push(...transform(v));
        });

      return result;
    },
    (v, obj) =>
    {
      const cv = cmn.jsonGet(obj, tgtKey, false);
      cmn.jsonSet(obj, tgtKey, cv ? cv.concat(v) : v);
    },
    []);
}

function aggregateAttributes(attributeIds, tgtKey)
{
  return aggregateAndTransformAttributes(
    attributeIds,
    tgtKey,
    (v) => { return cmn.isArray(v) ? v : [v] });
}

function tagAttribute(attributeId, transform)
{
  return makeMapping(
    'attributes',
    (attributes) =>
    {
      if (areNoAttributes(attributes))
      {
        return null;
      }

      for (const attribute of attributes)
      {
        if (attributeId === attrId(attribute))
        {
          return transform(attrValue(attribute));
        }
      }

      return null;
    },
    (v, obj) =>
    {
      const currentTags = cmn.jsonSetDefaultAndGet(obj, 'tags', {});
      if (v && currentTags[v.tag] === undefined)
      {
        currentTags[v.tag] = v.description;
      }

      return obj
    },
    null);
}

const tagFdaApproval = tagAttribute(
  bl.tagBiolink('highest_FDA_approval_status'),
  (fdaDescription) =>
  {
    if (fdaDescription === 'regular approval' ||
        fdaDescription === 'FDA Approval')
    {
      return makeTag('fda_approved', 'FDA Approved');
    }

    return false;
  });

function makeSummarizeRules(rules)
{
  return (obj) =>
  {
    return rules.map(rule => { return rule(obj); });
  };
}

function trapiBindingToKobj(binding, type, kgraph)
{
  return cmn.jsonGet(cmn.jsonGet(kgraph, type), binding);
}

function redgeToTrapiKedge(edgeBinding, kgraph)
{
  return trapiBindingToKobj(edgeBinding, 'edges', kgraph);
}

function rnodeToTrapiKnode(nodeBinding, kgraph)
{
  return trapiBindingToKobj(nodeBinding, 'nodes', kgraph);
}

function getBindingId(bindings, key)
{
  const nodeBinding = cmn.jsonGet(bindings, key, false);
  if (!nodeBinding)
  {
    return false;
  }

  return cmn.jsonGet(nodeBinding[0], 'id');
}

function flattenBindings(bindings)
{
  return Object.values(bindings).reduce((ids, binding) =>
    {
      return ids.concat(binding.map(obj => { return cmn.jsonGet(obj, 'id'); }));
    },
    []);
}

function kedgeSubject(kedge)
{
  return cmn.jsonGet(kedge, 'subject');
}

function kedgeObject(kedge)
{
  return cmn.jsonGet(kedge, 'object');
}

function kedgePredicate(kedge)
{
  return cmn.jsonGet(kedge, 'predicate');
}

function kedgeToQualifiers(kedge)
{
  const kedgeQualifiers = cmn.jsonGet(kedge, 'qualifiers', false);
  if (!kedgeQualifiers)
  {
    return false;
  }

  const qualifiers = {};
  kedgeQualifiers.forEach((q) =>
    {
      const qualifierKey = bl.sanitizeBiolinkPredicate(q['qualifier_type_id']);
      const qualifierValue = bl.sanitizeBiolinkPredicate(q['qualifier_value']);
      qualifiers[qualifierKey] = qualifierValue;
    });

  return qualifiers;
}

function edgeToQualifiedPredicate(kedge, invert = false)
{
  function qualifiersToString(type, qualifiers, prefixes)
  {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // This ordering is important for building the correct statement
    const qualifierKeys = ['direction', 'aspect', 'form or variant', 'part', 'derivative'];
    const qualifierValues = qualifierKeys.map((key) =>
      {
        return cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false);
      });

    let qualifierStr = '';
    qualifierValues.forEach((qv, i) =>
      {
        if (qv)
        {
          if (qualifierStr)
          {
            qualifierStr += ' '
          }

          if (prefixes[i])
          {
            qualifierStr += `${prefixes[i]} `;

          }

          qualifierStr += qv;
        }
      });

    return qualifierStr;
  }

  function subjectQualifiersToString(qualifiers, directionPrefix = false)
  {
    return qualifiersToString('subject',
                              qualifiers,
                              [directionPrefix, false, 'of a', 'of the', false]);
  }

  function objectQualifiersToString(qualifiers, directionPrefix = false)
  {
    return qualifiersToString('object',
                              qualifiers,
                              [directionPrefix, false, 'of a', 'of the', false]);
  }

  function finalizeQualifiedPredicate(prefix, predicate, suffix)
  {
    if (prefix)
    {
      prefix += ' ';
    }

    if (suffix)
    {
      suffix = ` ${suffix} of`;
    }

    const finalPredicate = `${prefix}${predicate}${suffix}`;
    return finalPredicate;
  }

  function getSpecialCase(predicate, qualifiers, invert)
  {
    const objDirectionQualifier = qualifiers['object direction qualifier'];
    if (predicate === 'regulates' &&
          (objDirectionQualifier === 'upregulated' ||
           objDirectionQualifier === 'downregulated'))
    {
      if (invert)
      {
        return `is ${objDirectionQualifier} by`;
      }

      return objDirectionQualifier.replace('ed', 'es');
    }

    return false;
  }

  let predicate = bl.sanitizeBiolinkPredicate(kedgePredicate(kedge));
  let qualifiers = kedgeToQualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPredicate(predicate))
  {
    [predicate, qualifiers] = bl.deprecatedPredicateToPredicateAndQualifiers(predicate);
  }
  // If we don't have any qualifiers, treat it like biolink v2
  if (!qualifiers)
  {
    if (invert)
    {
      predicate = bl.invertBiolinkPredicate(predicate);
    }

    return predicate;
  }

  const qualifiedPredicate = cmn.jsonGet(qualifiers, 'qualified predicate', false);
  if (qualifiedPredicate)
  {
    predicate = qualifiedPredicate;
  }

  const specialCase = getSpecialCase(predicate, qualifiers, invert);
  if (specialCase)
  {
    return specialCase;
  }

  if (invert)
  {
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

function makeTag(tag, name, description = '')
{
  return {
    'tag': tag,
    'description': {
      'name': name,
      'value': description
    }
  }
}

function makeRgraph(rnodes, redges, kgraph)
{
  const knodes = cmn.jsonGet(kgraph, 'nodes');
  for (const rnode of rnodes)
  {
    if (!cmn.jsonHasKey(knodes, rnode))
    {
      return false;
    }
  }

  const rgraph = {};
  rgraph.nodes = rnodes;
  rgraph.edges = redges.filter(redge =>
    {
      const kedge = redgeToTrapiKedge(redge, kgraph);
      return bl.isBiolinkPredicate(kedgePredicate(kedge));
    });

  return rgraph;
}

function isRedgeInverted(redge, subject, kgraph)
{
  const kedge = redgeToTrapiKedge(redge, kgraph);
  return subject === kedgeObject(kedge);
}

function trapiResultToRgraph(trapiResult, kgraph)
{
  return makeRgraph(flattenBindings(cmn.jsonGet(trapiResult, 'node_bindings')),
    flattenBindings(cmn.jsonGet(trapiResult, 'edge_bindings')),
    kgraph);
}

function rnodeToKey(rnode, kgraph)
{
  return rnode;
}

function redgeToKey(redge, kgraph, doInvert = false)
{
  const kedge = redgeToTrapiKedge(redge, kgraph);
  const ksubject = kedgeSubject(kedge);
  const predicate = edgeToQualifiedPredicate(kedge, doInvert);
  const kobject = kedgeObject(kedge);
  if (doInvert)
  {
    return pathToKey([kobject, predicate, ksubject]);
  }

  return pathToKey([ksubject, predicate, kobject]);
}

function summarizeRnode(rnode, kgraph, nodeRules)
{
  return cmn.makePair(rnodeToKey(rnode, kgraph),
    nodeRules(rnodeToTrapiKnode(rnode, kgraph)),
    'key',
    'transforms');
}

function summarizeRedge(redge, kgraph, edgeRules)
{
  return cmn.makePair(redgeToKey(redge, kgraph),
    edgeRules(redgeToTrapiKedge(redge, kgraph)),
    'key',
    'transforms');
}

function makeRedgeToEdgeId(rgraph, kgraph)
{
  function makeEdgeId(subject, object)
  {
    return cmn.makePair(subject, object, 'subject', 'object');
  }

  let redgeToEdgeId = {};
  rgraph.edges.forEach(redge =>
    {
      const kedge = redgeToTrapiKedge(redge, kgraph);
      cmn.jsonSet(redgeToEdgeId, redge, makeEdgeId(kedgeSubject(kedge), kedgeObject(kedge)));
    });

  return (redge) => { return cmn.jsonGet(redgeToEdgeId, redge); };
}

function makeRnodeToOutEdges(rgraph, kgraph)
{

  function makeOutEdge(redge, node)
  {
    return cmn.makePair(redge, node, 'redge', 'target');
  }

  const redgeToEdgeId = makeRedgeToEdgeId(rgraph, kgraph);
  const rnodeToOutEdges = {};
  rnodeToOutEdges.update = (rnode, val) =>
  {
    const outEdges = cmn.jsonGet(rnodeToOutEdges, rnode, []);
    outEdges.push(val);
    cmn.jsonSet(rnodeToOutEdges, rnode, outEdges);
  };

  rgraph.edges.forEach(redge =>
    {
      const edgeId = redgeToEdgeId(redge);
      const subject = edgeId.subject;
      const object = edgeId.object;

      rnodeToOutEdges.update(subject, makeOutEdge(redge, object));
      rnodeToOutEdges.update(object, makeOutEdge(redge, subject));
    });

  return (rnode) => { return cmn.jsonGet(rnodeToOutEdges, rnode, []); };
}

function rgraphFold(proc, init, acc)
{
  let objLeft = init;
  let res = acc;
  while (!cmn.isArrayEmpty(objLeft))
  {
    const paths = proc(objLeft.pop());
    objLeft.push(...paths.first);
    res.push(...paths.second);
  }

  return res;
}

function makeSummaryFragment(paths, nodes, edges, scores)
{
  const summaryFragment = {};
  summaryFragment.paths = paths;
  summaryFragment.nodes = nodes;
  summaryFragment.edges = edges;
  summaryFragment.scores = scores;
  return summaryFragment;
}

function emptySummaryFragment()
{
  return makeSummaryFragment([], [], [], {});
}

function makeCondensedSummary(agent, summaryFragment)
{
  return cmn.makePair(agent, summaryFragment, 'agent', 'fragment');
}

function condensedSummaryPaths(condensedSummary)
{
  return condensedSummary.fragment.paths;
}

function condensedSummaryNodes(condensedSummary)
{
  return condensedSummary.fragment.nodes;
}

function condensedSummaryEdges(condensedSummary)
{
  return condensedSummary.fragment.edges;
}

function condensedSummaryScores(condensedSummary)
{
  return condensedSummary.fragment.scores;
}

function pathToKey(path)
{
  return hash(path);
}

function mergeSummaryFragments(f1, f2)
{
  f1.paths.push(...f2.paths);
  f1.nodes.push(...f2.nodes);
  f1.edges.push(...f2.edges);

  const newScoreKey = Object.keys(f2.scores)[0]; // There is only one score per new summary fragment
  const currentScores = cmn.jsonSetDefaultAndGet(f1.scores, newScoreKey, []);
  currentScores.push(f2.scores[newScoreKey]);

  return f1;
}

function creativeAnswersToCondensedSummaries(answers, nodeRules, edgeRules, maxHops)
{
  function trapiResultToSummaryFragment(trapiResult, kgraph, startKey, endKey)
  {
    const rgraph = trapiResultToRgraph(trapiResult, kgraph);
    if (!rgraph)
    {
      return emptySummaryFragment();
    }

    const nodeBindings = trapiResult['node_bindings'];
    const start = getBindingId(nodeBindings, startKey);
    const end = getBindingId(nodeBindings, endKey);
    if (!start || !end)
    {
      return emptySummaryFragment();
    }

    const rnodeToOutEdges = makeRnodeToOutEdges(rgraph, kgraph);
    const maxPathLength = (2 * maxHops) + 1;
    const rgraphPaths = rgraphFold((path) =>
      {
        const currentRnode = path[path.length-1];
        if (maxPathLength < path.length)
        {
          return cmn.makePair([], []);
        }
        else if (currentRnode === end)
        {
          return cmn.makePair([], [path]);
        }
        else
        {
          let validPaths = [];
          rnodeToOutEdges(currentRnode).forEach((edge) =>
            {
              const target = edge.target
              if (!path.includes(target))
              {
                let newPath = [...path, edge.redge, edge.target];
                validPaths.push(newPath);
              }
            });

          return cmn.makePair(validPaths, []);
        }
      },
      [[start]],
      []);

    function normalizePaths(rgraphPaths, kgraph)
    {
      function N(n) { return rnodeToKey(n, kgraph); }
      function E(e, o) { return redgeToKey(e, kgraph, isRedgeInverted(e, o, kgraph)); }
      return rgraphPaths.map(path =>
        {
          let normalizedPath = [];
          const pathLength = path.length - 1;
          if (pathLength < 0)
          {
            return normalizedPath;
          }

          for (let i = 0; i < pathLength; i+=2)
          {
            const node = path[i];
            const edge = path[i+1];
            normalizedPath.push(N(node), E(edge, node));
          }

          normalizedPath.push(N(path[pathLength]));
          return normalizedPath;
        });
    }

    const resultStartKey = rnodeToKey(start, kgraph);
    const resultScore = cmn.jsonGet(trapiResult, 'normalized_score', 0);
    const fragmentScore = {};
    fragmentScore[resultStartKey] = resultScore;

    return makeSummaryFragment(
      normalizePaths(rgraphPaths, kgraph),
      rgraph.nodes.map(rnode => { return summarizeRnode(rnode, kgraph, nodeRules); }),
      rgraph.edges.map(redge => { return summarizeRedge(redge, kgraph, edgeRules); }),
      fragmentScore
    );
  }

  function getPathDirection(qgraph)
  {
    const qgraphNodes = cmn.jsonGet(qgraph, 'nodes');
    const startIsObject = cmn.jsonGetFromKpath(qgraphNodes, [subjectKey, 'ids'], false);
    if (startIsObject)
    {
      return [objectKey, subjectKey];
    }

    return [subjectKey, objectKey];
  }

  return answers.map((answer) =>
    {
      const reportingAgent = answer.agent;
      const trapiMessage = answer.message;
      const trapiResults = cmn.jsonGet(trapiMessage, 'results');
      const kgraph = cmn.jsonGet(trapiMessage, 'knowledge_graph');
      const [startKey, endKey] = getPathDirection(cmn.jsonGet(trapiMessage, 'query_graph'));

      return makeCondensedSummary(
        reportingAgent,
        trapiResults.reduce(
          (summaryFragment, result) =>
          {
            return mergeSummaryFragments(
              summaryFragment,
              trapiResultToSummaryFragment(result, kgraph, startKey, endKey));
          },
          emptySummaryFragment()));
    });
}

async function condensedSummariesToSummary(qid, condensedSummaries, annotationClient)
{
  function fragmentPathsToResultsAndPaths(fragmentPaths)
  {
    // TODO: use objects instead of arrays?
    let results = [];
    let paths = [];
    fragmentPaths.forEach((path) =>
      {
        const pathKey = pathToKey(path);
        results.push(cmn.makePair(path[0], pathKey, 'start', 'pathKey'))
        paths.push(cmn.makePair(pathKey, path, 'key', 'path'))
      });

    return [results, paths];
  }

  function extendSummaryResults(results, newResults)
  {
    newResults.forEach((result) =>
      {
        let existingResult = cmn.jsonSetDefaultAndGet(results, result.start, {});
        let paths = cmn.jsonSetDefaultAndGet(existingResult, 'paths', [])
        paths.push(result.pathKey);
      });
  }

  function extendSummaryPaths(paths, newPaths, agent)
  {
    newPaths.forEach((path) =>
      {
        let existingPath = cmn.jsonGet(paths, path.key, false);
        if (existingPath)
        {
          cmn.jsonGet(existingPath, 'aras').push(agent);
          return;
        }

        cmn.jsonSet(paths, path.key, {'subgraph': path.path, 'aras': [agent]});
      });
  }

  function extendSummaryObj(objs, updates, agent)
  {
    updates.forEach((update) =>
      {
        let obj = cmn.jsonSetDefaultAndGet(objs, update.key, {'aras': []});
        update.transforms.forEach((transform) =>
          {
            transform(obj);
            obj.aras.push(agent);
          });
      });
  }

  function extendSummaryNodes(nodes, nodeUpdates, agent)
  {
    extendSummaryObj(nodes, nodeUpdates, agent);
  }

  function extendSummaryEdges(edges, edgeUpdates, agent)
  {
    extendSummaryObj(edges, edgeUpdates, agent);
  }

  function extendSummaryScores(scores, newScores)
  {
    Object.keys(newScores).forEach((resultNode) =>
      {
        const currentScores = cmn.jsonSetDefaultAndGet(scores, resultNode, []);
        currentScores.push(...newScores[resultNode]);
      });
  }

  function extendSummaryPublications(publications, edge)
  {
    function makePublicationObject(type, url, snippet, pubdate)
    {
      return {'type': type, 'url': url, 'snippet': snippet, 'pubdate': pubdate};
    }

    const snippets = cmn.jsonGet(edge, 'snippets');
    const publicationIds = cmn.jsonGet(edge, 'publications', []);
    publicationIds.forEach((id) =>
      {
        const [type, url] = ev.idToTypeAndUrl(id);
        let publicationObj = false;
        for (const snippet of snippets)
        {
          publicationObj = cmn.jsonGet(snippet, id, false);
          if (!!publicationObj)
          {
            break;
          }
        }

        if (publicationObj)
        {
          const snippet = cmn.jsonGet(publicationObj, 'sentence', null);
          const pubdate = cmn.jsonGet(publicationObj, 'publication date', null);
          cmn.jsonSet(publications, id, makePublicationObject(type, url, snippet, pubdate));
          return;
        }

        cmn.jsonSet(publications, id, makePublicationObject(type, url, null, null));
      });
  }

  function edgesToEdgesAndPublications(edges)
  {
    function addInverseEdge(edges, edge)
    {
      const edgePredicate = cmn.jsonGet(edge, 'predicate');
      const invertedPredicate = edgeToQualifiedPredicate(edge, true);
      const subject = cmn.jsonGet(edge, 'subject');
      const object = cmn.jsonGet(edge, 'object');

      const invertedEdgeKey = pathToKey([object, invertedPredicate, subject]);
      let invertedEdge = cmn.deepCopy(edge);
      cmn.jsonMultiSet(invertedEdge,
                      [['subject', object],
                       ['object', subject],
                       ['predicate', invertedPredicate]]);

      delete invertedEdge['qualifiers'];
      edges[invertedEdgeKey] = invertedEdge;
    }

    const publications = {};
    Object.values(edges).forEach((edge) =>
      {
        extendSummaryPublications(publications, edge);
        delete edge['snippets'];
        addInverseEdge(edges, edge);
        cmn.jsonSet(edge, 'predicate', edgeToQualifiedPredicate(edge));
        delete edge['qualifiers'];
      });

    return [edges, publications];
  }

  function resultsToResultsAndTags(results, paths, nodes, scores)
  {
    function isPathLessThan(pid1, pid2)
    {
      const path1 = getPathFromPid(paths, pid1);
      const path2 = getPathFromPid(paths, pid2);
      const p1Len = path1.length;
      const p2Len = path2.length;
      if (p1Len === p2Len)
      {
        for (let i = 0; i < path1.length; i+=2)
        {
          if (path1[i] < path2[i])
          {
            return -1;
          }
          else if (path1[i] > path2[i])
          {
            return 1;
          }
        }

        return 0;
      }

      if (p1Len < p2Len)
      {
        return -1;
      }

      return 1;
    }

    const usedTags = {};
    const expandedResults = results.map((result) =>
      {
        const ps = cmn.jsonGet(result, 'paths');
        const subgraph = getPathFromPid(paths, ps[0]);
        const start = subgraph[0];
        const startNames = cmn.jsonGetFromKpath(nodes, [start, 'names']);
        const end = subgraph[subgraph.length-1];
        const startScores = scores[start];
        const startTags = cmn.jsonGetFromKpath(nodes, [start, 'tags']);
        const uniqStartTags = Object.keys(startTags);
        const endTags = cmn.jsonGetFromKpath(nodes, [end, 'tags']);
        const uniqEndTags = Object.keys(endTags);
        const tags = cmn.setUnion([new Set(uniqStartTags),
                                   new Set(uniqEndTags)]);

        uniqStartTags.forEach((tag) =>
          {
            if (usedTags[tag] === undefined)
            {
              usedTags[tag] = startTags[tag];
            }
          });

        uniqEndTags.forEach((tag) =>
          {
            if (usedTags[tag] === undefined)
            {
              usedTags[tag] = endTags[tag];
            }
          });

        return {
          'subject': start,
          'drug_name': (cmn.isArrayEmpty(startNames)) ? start : startNames[0],
          'paths': ps.sort(isPathLessThan),
          'object': end,
          // startScores.length is guarateed to be > 0
          'score': startScores.reduce((a, b) => { return a + b; }) / startScores.length,
          'tags': cmn.setToObject(tags)
        }
      });

    return [expandedResults, usedTags];
  }

  function objRemoveDuplicates(obj)
  {
    Object.keys(obj).forEach((k) =>
      {
        let v = cmn.jsonGet(obj, k);
        if (cmn.isArray(v))
        {
          obj[k] = [...new Set(v)];
        }
      });

    return obj;
  }

  function getPathFromPid(paths, pid)
  {
    return cmn.jsonGetFromKpath(paths, [pid, 'subgraph']);
  }

  let results = {};
  let paths = {};
  let nodes = {};
  let edges = {};
  let publications = {};
  let scores = {};
  let tags = [];
  condensedSummaries.forEach((cs) =>
    {
      const agent = cs.agent;
      const [newResults, newPaths] = fragmentPathsToResultsAndPaths(condensedSummaryPaths(cs));
      extendSummaryResults(results, newResults);
      extendSummaryPaths(paths, newPaths, agent);
      extendSummaryNodes(nodes, condensedSummaryNodes(cs), agent);
      extendSummaryEdges(edges, condensedSummaryEdges(cs), agent);
      extendSummaryScores(scores, condensedSummaryScores(cs));
    });

  results = Object.values(results).map(objRemoveDuplicates)
  const endpoints = new Set();
  results.forEach((result) =>
    {
      const ps = cmn.jsonGet(result, 'paths');
      const subgraph = getPathFromPid(paths, ps[0]);
      endpoints.add(subgraph[0]);
      endpoints.add(subgraph[subgraph.length-1]);
    });

  const annotationPromise = annotationClient.annotateGraph(
    createKGFromNodeIds([...endpoints],
                        [
                          'ChEMBL:atc_classification',
                          bl.tagBiolink('highest_FDA_approval_status')
                        ]));

  function pushIfEmpty(arr, val)
  {
    if (cmn.isArrayEmpty(arr))
    {
      arr.push(val);
    }
  };

  Object.keys(nodes).forEach((k) =>
    {
      let node = nodes[k];
      objRemoveDuplicates(node);
      let nodeNames = cmn.jsonGet(node, 'names');
      pushIfEmpty(nodeNames, k);

      let nodeCuries = cmn.jsonGet(node, 'curies');
      pushIfEmpty(nodeCuries, k);
      cmn.jsonSet(node, 'provenance', [bl.curieToUrl(k)])
    });

  Object.values(edges).forEach((edge) =>
    {
      objRemoveDuplicates(edge);
      cmn.jsonUpdate(edge, 'publications', (publications) => { return publications.filter(ev.isValidId); });
      cmn.jsonUpdate(edge, 'provenance', (provenance) =>
        {
          return provenance.map(bl.inforesToProvenance).filter(cmn.identity);
        });
    });

  [edges, publications] = edgesToEdgesAndPublications(edges);

  const metadataObject = makeMetadataObject(qid, condensedSummaries.map((cs) => { return cs.agent; }));
  try
  {
    const nodeRules = makeSummarizeRules(
        [
          tagAttribute(
            'ChEMBL:atc_classification',
            (classification) =>
            {
              const highestLevel = classification.split('|')[0];
              const [tag, description] = highestLevel.split(/-(.*)/s);
              return makeTag(`ATC_${tag}`, cmn.capitalize(description));
            }),
          tagFdaApproval
        ]);

    const annotationMessage = await annotationPromise;
    const kgraph = cmn.jsonGetFromKpath(annotationMessage, ['message', 'knowledge_graph'])
    const knodes = cmn.jsonGet(kgraph, 'nodes');
    const annotationUpdates = Object.keys(knodes).map((rnode) =>
      {
        return summarizeRnode(rnode, kgraph, nodeRules);
      });

    extendSummaryNodes(nodes, annotationUpdates, 'kp-molecular');
  }
  catch (err)
  {
    console.error(err);
  }
  finally
  {
    [results, tags] = resultsToResultsAndTags(results, paths, nodes, scores);
    Object.values(paths).forEach(objRemoveDuplicates);
    return {
      'meta': metadataObject,
      'results': results,
      'paths': paths,
      'nodes': nodes,
      'edges': edges,
      'publications': publications,
      'tags': tags
    };
  }
}
