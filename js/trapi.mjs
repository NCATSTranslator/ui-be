'use strict';

import * as cmn from './common.mjs';
import { idToTypeAndUrl, isValidId } from './evidence.mjs';
import * as bl from './biolink-model.mjs';
import { SERVER_CONFIG } from './config.mjs';

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

export function diseaseToCreativeQuery(diseaseObj)
{
  function diseaseToTrapiQgraph(disease)
  {
    return {
      'nodes': {
        'drug': {
          'categories': [bl.tagBiolink('ChemicalEntity')]
        },
        'disease': {
          'ids': [disease],
          'categories': [bl.tagBiolink('Disease')]
        }
      },
      'edges': {
        'treats': {
          'subject': 'drug',
          'object': 'disease',
          'predicates': [bl.tagBiolink('treats')],
          'knowledge_type': 'inferred'
        }
      }
    }
  }

  if (!cmn.isObj(diseaseObj))
  {
    throw new TypeError(`Expected diseaseObj to be type object, got: ${diseaseObj}`);
  }

  if (!cmn.jsonHasKey(diseaseObj, 'disease'))
  {
    throw new ReferenceError(`Expected diseaseObj to have key disease, got: ${diseaseObj}`);
  }

  return {
    'message': {
      'query_graph': diseaseToTrapiQgraph(cmn.jsonGet(diseaseObj, 'disease'))
    }
  };
}

export function creativeAnswersToSummary (qid, answers)
{
  const resultNodes = answers.map((answer) =>
    {
      return cmn.jsonGetFromKpath(answer.message(), ['knowledge_graph', 'nodes']);
    });
  const nodeToCanonicalNode = makeCanonicalNodeMapping(resultNodes);
  const nodeRules = makeSummarizeRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAttributes([bl.tagBiolink('xref')], 'curies'),
      renameAndTransformAttribute(
        bl.tagBiolink('highest_FDA_approval_status'),
        ['fda_info'],
        (fdaDescription) =>
        {
          return {
            'highest_fda_approval_status': fdaDescription,
            'max_level': fdaDescriptionToFdaLevel(fdaDescription)
          };
        }),
      aggregateAttributes([bl.tagBiolink('description')], 'description'),
      aggregateAttributes([bl.tagBiolink('synonym')], 'synonym'),
      aggregateAttributes([bl.tagBiolink('same_as')], 'same_as'),
      aggregateAttributes([bl.tagBiolink('IriType')], 'iri_type')
    ]);

  const edgeRules = makeSummarizeRules(
    [
      aggregateAndTransformProperty(
        'predicate',
        ['predicates'],
        bl.sanitizePredicate),
      transformProperty('subject', nodeToCanonicalNode),
      transformProperty('object', nodeToCanonicalNode),
      aggregateAttributes([bl.tagBiolink('IriType')], 'iri_type'),
      aggregateAttributes(['bts:sentence'], 'snippets'),
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

  const maxHops = SERVER_CONFIG.maxHops;
  return condensedSummariesToSummary(
           qid,
           creativeAnswersToCondensedSummaries(
             answers,
             nodeRules,
             edgeRules,
             nodeToCanonicalNode,
             maxHops));
}

function makeMapping(key, transform, update, fallback)
{
  return obj =>
  {
    const val = cmn.jsonGet(obj, key, false);
    return acc => { return update((val ? transform(val) : fallback, acc)); }
  }
}

function aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate)
{
  const cv = cmn.jsonGetFromKpath(obj, kpath);
  if (doUpdate(v))
  {
    const uv = cmn.isArray(v) ? v : [v];
    return cmn.jsonSetFromKpath(obj, kpath, cv ? v.concat(cv) : v);
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
  return aggregatePropertyUpdateWhen(v, obj, kpath, v => { return true; });
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
             const cv = cmn.jsonGet(obj, key);
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
  return attributes === null || cmn.isArrayEmpty(attributes);
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

             attributes.forEach(attribute =>
               {
                 if (attributeId === attrId(attribute))
                 {
                   return transform(attrValue(attribute));
                 }
               });

               return null;
           },
           (v, obj) => { return cmn.jsonSetFromKpath(obj, kpath, v); },
           null);
}

function renameAttribute(attributeId, kpath)
{
  renameAndTransformAttribute(attributeId, kpath, cmn.identity);
}

function aggregateAndTransformAttributes(attributeIds, tgtKey, transform)
{
  return makeMapping(
           'attributes',
           (attributes) =>
           {
             let result = [];
             if (areNoAttributes)
             {
               return result;
             }

             attributes.forEach(attribute =>
             {
               const v = (attributeIds.includes(attrId(attribute))) ? attrValue(attribute) : [];
               result.concat(transform(v));
             });

             return result;
           },
           (v, obj) =>
           {
             return cmn.jsonSet(obj, tgtKey, v);
           });
}

function aggregateAttributes(attributeIds, tgtKey)
{
  aggregateAndTransformAttributes(
    attributeIds,
    tgtKey,
    (v) => (cmn.isArray(v)) ? v : [v]);
}

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
  return cmn.jsonGet(cmn.jsonGet(bindings, key), 'id');
}

function flattenBindings(bindings)
{
  return bindings.reduce((binding, ids) =>
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

function makeRgraph(rnodes, redges, kgraph)
{
  let rgraph = {};
  rgraph.nodes = () => { return rnodes; };
  rgraph.edges = () =>
  {
    return redges.filter(redge =>
           {
             const kedge = redgeToTrapiKedge(redge, kgraph);
             return isBiolinkPredicate(kedgePredicate(kedge));
           });
  };

  return rgraph;
}

function isRedgeInverted(redge, object, kgraph)
{
  const kgedge = redgeToTrapiKedge(redge, kgraph);
  return object === kedgeSubject(kedge);
}

function trapiResultToRgraph(trapiResult, kgraph)
{
  return makeRgraph(flattenBindings(cmn.jsonGet(trapiResult, 'node_bindings')),
                    flattenBindings(cmn.jsonGet(trapiResult, 'edge_bindings')),
                    kgraph);
}

function rnodeToKey(rnode, kgraph, nodeToCanonicalNode)
{
  return nodeToCanonicalNode(rnode);
}

function redgeToKey(redge, kgraph, nodeToCanonicalNode, doInvert = false)
{
  const kedge = redgeToTrapiKedge(redge, kgraph);
  const ksubject = nodeToCanonicalNode(kedgeSubject(kedge));
  const kpredicate = kedgePredicate(kedge);
  const kobject = nodeToCanonicalNode(kedgeObject(kedge));
  if (doInvert)
  {
    return pathToKey([kobject, invertBiolinkPredicate(kpredicate), ksubject]);
  }

  return pathToKey([ksubject, kpredicate, kobject]);
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
    const kedge = redgeToTrapiKedge(redge, kedge);
    cmn.jsonSet(redgeToEdgeId, redge, makeEdgeId(kedgeSubject(kedge), kedgeObject(kedge)));
  });

  return (redge) => { return cmn.jsonGet(redgeToEdgeId, redge); };
}

function makeRnodeToOutEdges(rgraph, kgraph)
{

  function makeOutEdge(redge, node)
  {
    return cmn.makePair(redge, node, 'edge', 'node');
  }

  const redgeToEdgeId = makeRedgeToEdgeId(rgraph, kgraph);
  let rnodeToOutEdges = {};
  rnodeToOutEdges.update = (rnode, val) =>
  {
    let outEdges = cmn.jsonGet(rnodeToOutEdges, rnode, []);
    outEdges.push(val);
    cmn.jsonSet(rnodeToOutEdges, rnode, outEdges);
  };

  rgraph.edges.forEach(redge =>
  {
    const edgeId = redgeToEdgeId(redge);
    const subject = edgeId.subject();
    const object = edgeId.object();

    rnodeToOutEdges.update(subject, makeOutEdge(redge, object));
    rnodeToOutEdges.update(object, makeOutEdge(redge, subject));
  });

  return (rnode) => { return cmn.jsonGet(rnodeToOutEdges, redge, []); };
}

function rgraphFold(proc, init, acc)
{
  let objLeft = init;
  let res = acc;
  while (!cmn.isArrayEmpty(objLeft))
  {
    const paths = proc(objLeft.pop());
    objLeft = objLeft.concat(paths.first);
    res = res.concat(paths.second);
  }

  return res;
}

function makeSummaryFragment(paths, nodes, edges)
{
  let summaryFragment = {};
  summaryFragment.paths = () => { return paths; };
  summaryFragment.nodes = () => { return nodes; };
  summaryFragment.edges = () => { return edges; };
  return summaryFragment;
}

function makeCondensedSummary(agent, summaryFragment)
{
  return makePair(agent, summaryFragment, 'agent', 'fragment');
}

function condensedSummaryPaths(condensedSummary)
{
  return condensedSummary.fragment().paths();
}

function condensedSummaryNodes(condensedSummary)
{
  return condensedSummary.fragment().nodes();
}

function condensedSummaryEdges(condensedSummary)
{
  return condensedSummary.fragment().edges();
}

function pathToKey(path)
{
  return path; // TODO: find a good array hashing method
}

function mergeSummaryFragments(f1, f2)
{
  Object.keys(f1).forEach((k) =>
  {
    f1[k]().push(...f2[k]());
  });
}

function makeCanonicalNodeMapping(allNodes)
{
  function isAttributeAlias(attr)
  {
    const attrType = attrId(attr);
    return attrType === 'same_as' || attrType === 'xref';
  }

  const nodeSets = [];
  const resultCuries = [];
  allNodes.forEach((nodes) =>
    {
      let curies = Object.keys(nodes);
      resultCuries.concat(curies);
      curies.forEach((curie) =>
        {
          let node = cmn.jsonGet(nodes, curie);
          let attributes = cmn.jsonGet(node, 'attributes', []);
          let aliases = [curie];
          attributes.forEach((attr) =>
            {
              if (isAttributeAlias(attr))
              {
                aliases.append(attrValue(attr));
              }
            });

          nodeSets.push(new Set(aliases));
        });
    });

  const allCuries = resultCuries.concat(cmn.setUnion(nodeSets).keys());
  const mergedNodes = allCuries.reduce((nodeSets, curie) =>
    {
      let mergeableBags = [];
      let unmergedBags = [];
      nodeSets.forEach((nodeSet) =>
        {
          if (nodeSet.has(curie))
          {
            mergeableBags.push(nodeSet);
          }
          else
          {
            unmergedBags.push(nodeSet);
          }
        });

      if (cmn.isArrayEmpty(mergeableBags))
      {
        return unmergedBags;
      }
      else
      {
        return unmergedBags.push(cmn.setUnion(mergeableBags));
      }
    },
    nodeSets);

  const nodeToCanonicalNode = new Object();
  mergedNodes.forEach((nodeSet) =>
    {
      let nodes = nodeSet.keys();
      let canonicalNode = nodes[0];
      nodes.forEach((curie) =>
        {
          nodeToCanonicalNode[curie] = canonicalNode;
        });
    });

  return function(node)
  {
    nodeToCanonicalNode[node] || false;
  }
}

function creativeAnswersToCondensedSummaries(answers, nodeRules, edgeRules, nodeToCanonicalNode, maxHops)
{
  function trapiResultToSummaryFragment(trapiResult, kgraph)
  {
    const nodeBindings = trapiResult['node_bindings'];
    const rgraph = trapiResultToRgraph(trapiResult, kgraph);
    const drug = getBindingId(nodeBindings, 'drug');
    const disease = getBindingId(nodeBindings, 'disease');
    const rnodeToOutEdges = makeRnodeToOutEdges(rgraph, kgraph);
    const maxPathLength = (2 * maxHops) + 1;
    const rgraphPaths = rgraphFold(path =>
      {
        const currentRnode = path[0];
        if (maxPathLength < path.length)
        {
          return cmn.makePair([], []);
        }
        else if (currentRnode === disease)
        {
          return cmn.makePair([], [path]);
        }
        else
        {
          let validPaths = [];
          rnodeToOutEdges.forEach(edge =>
          {
            const target = edge.target
            if (!path.includes(target) && !!nodeToCanonicalNode(target))
            {
              validPaths.push(path.push(edge.id, edge.target));
            }
          });

          return cmn.makePair(validPaths, []);
        }
      },
      [[drug]],
      []);

    function summarizeRnode(rnode, kgraph)
    {
      return cmn.makePair(rnodeToKey(rnode, kgraph, nodeToCanonicalNode),
                          nodeRules(rnodeToTrapiKnode(rnode, kgraph)),
                          'key',
                          'transforms');
    }

    function summarizeRedge(redge, kgraph)
    {
      return cmn.makePair(redgeToKey(redge, kgraph, nodeToCanonicalNode),
                          edgeRules(redgeToTrapiKedge(redge, kgraph)),
                         'key',
                         'transforms');
    }

    function normalizePaths(rgraphPaths, kgraph)
    {
      function N(n) { return rnodeToKey(n, kgraph, nodeToCanonicalNode); }
      function E(e, o) { return redgeToKey(e, kgraph, nodeToCanonicalNode, isRedgeInverted(e, o, kgraph)); }
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

    return makeSummaryFragment(
      normalizePaths(rgraphPaths, kgraph),
      rgraph.nodes().map(node => { return summarizeRnode(rnode, kgraph); }),
      rgraph.edges().map(edge => { return summarizeRedge(redge, kgraph); })
    );
  }

  return answers.map((answer) =>
          {
            const reportingAgent = answer.agent();
            const trapiMessage = answer.message();
            const trapiResults = cmn.jsonGet(trapiMessage, 'results');
            const kgraph = cmn.jsonGet(trapiMessage, 'knowledge_graph');
            return makeCondensedSummary(
                    reportingAgent,
                    trapiResults.reduce(
                      (result, summaryFragment) =>
                        {
                          return mergeSummaryFragments(
                            trapiResultToSummaryFragment(result, kgraph),
                            summaryFragment);
                        },
                      emptySummaryFragment()));
          });
}

function condensedSummariesToSummary(qid, condensedSummaries)
{
  function fragmentPathsToResultsAndPaths(fragmentPaths)
  {
    // TODO: use objects instead of arrays?
    let results = [];
    let paths = [];
    fragmentPaths.forEach((path) =>
    {
      const pathKey = pathToKey(path);
      results.push(cmn.makePair(path[0], pathKey, 'drug', 'pathKey'))
      paths.push(cmn.makePair(pathKey, path, 'key', 'path'))
    });

    return cmn.makePair(results, paths, 'results', 'paths');
  }

  function extendSummaryResults(results, newResults)
  {
    newResults.forEach((result) =>
    {
      let existingResult = cmn.jsonSetDefaultAndGet(results, result.drug(), {});
      let paths = cmn.jsonSetDefaultAndGet(existingResult, 'paths', [])
      paths.push(result.pathKey());
    });
  }

  function extendSummaryPaths(paths, newPaths, agent)
  {
    newPaths.forEach((path) =>
    {
      let existingPath = cmn.jsonGet(paths, path.key(), false);
      if (existingPath)
      {
        cmn.jsonGet(existingPath, 'aras').push(agent);
        return;
      }

      cmn.jsonSet(paths, path.key(), {'subgraph': path.path(), 'aras': [agent]});
    });
  }

  function extendSummaryObj(objs, updates, agent)
  {
    updates.forEach((update) =>
    {
      let obj = cmn.jsonSetDefaultOrGet(objs, update.key(), {'aras': []});
      update.transforms().forEach((transform) =>
      {
        transform(obj);
        cmn.jsonSet(obj, 'aras', agent);
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
      const [type, url] = idToTypeAndUrl(id);
      const publicationObj = cmn.jsonGet(snippets, id, false);
      if (snippet)
      {
        const snippet = cmn.jsonGet(publicationObj, 'sentence', null);
        const pubdate = cmn.jsonGet(publicationObj, 'publication date', null);
        cmn.jsonSet(publication, id, makePublicationObject(type, url, snippet, pubdate));
        return;
      }

      cmn.jsonSet(publications, id, makePublicationObject(type, url, null, null));
    });
  }

  function edgesToEdgesAndPublications(edges)
  {
    function addInvertEdge(edges, edge)
    {
      const edgePredicate = cmn.jsonGet(edge, 'predicate')[0];
      const invertedPredicate = bl.invertBiolinkPredicate(edgePredicate);
      const subject = cmn.jsonGet(edge, 'subject');
      const object = cmn.jsonGet(edge, 'object');

      const invertedEdgeKey = pathToKey([object, invertedPredicate, subject]);
      let invertedEdge = cmn.deepCopy(edge);
      cmn.jsonMultiSet(invertedEdge, [['subject', subject],
                                      ['object', object],
                                      ['predicates', [invertedPredicate]]]);

      edges[invertedEdgeKey] = invertedEdge;
    }

    let publications = {};
    Object.values(edges).forEach((edge) =>
    {
      extendSummaryPublications(publications, edge);
      delete edge['snippets'];
      addInvertEdge(edges, edge);
    });

    return [edges, publications];
  }

  function expandResults(results, paths, nodes)
  {
    function getPathFromPid(paths, pid)
    {
      return cmn.jsonGetFromKpath(paths, [pid, 'subgraph']);
    }

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

    return results.map((result) =>
    {
      const ps = cmn.jsonGet(result, 'paths');
      const subgraph = getPathFromPid(paths, ps[0]);
      const drug = subgraph[0];
      const drugName = cmn.jsonGetFromKpath(nodes, [drug, 'names']);
      const disease = subgraph[subgraph.length-1];
      return {
        'subject': drug,
        'drug_name': (cmn.isArrayEmpty(drugName)) ? null : drugName[0],
        'paths': ps.sort(isPathLessThan),
        'object': disease
      }
    });
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
  }

  let results = {};
  let paths = {};
  let nodes = {};
  let edges = {};
  let publications = {};
  condensedSummaries.forEach((cs) =>
  {
    const agent = cs.agent();
    [newResults, newPaths] = fragmentPathsToResultsAndPaths(condensedSummaryPaths(cs));
    extendSummaryResults(results, newResults);
    extendSummaryPaths(paths, newPaths, agent);
    extendSummaryNodes(nodes, condensedSummaryNodes(cs), agent);
    extendSummaryEdges(edges, condensedSummaryEdges(cs), agent);
  });

  Object.values(edges).forEach((edge) =>
  {
    objRemoveDuplicates(edges);
    cmn.jsonUpdate(edge, 'publications', (publications) => { return publications.filter(isValidId) });
  });
  [edges, publications] = edgesToEdgesAndPublications(edges);

  const metadataObject = makeMetadataObject(qid, condensedSummaries.map((cs) => cs.agent()));
  results = expandResults(Object.values(results).map(objRemoveDuplicates), paths, nodes);
  Object.values(paths).forEach(objRemoveDuplicates);
  Object.keys(nodes).forEach((k) =>
  {
    let node = nodes[k];
    objRemoveDuplicates(node);
    let nodeNames = jsonGet(node, 'names');
    if (cmn.isEmptyArray(nodeNames))
    {
      nodeNames.push(k);
    }
  });

  return {
    'meta': metadataObject,
    'results': results,
    'paths': paths,
    'nodes': nodes,
    'edges': edges,
    'publications': publications
  };
}
