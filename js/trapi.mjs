'use strict';

import * as cmn from './common.mjs';
import { tagBiolink, isBiolinkPredicate } from './biolink-model.mjs';

let config = {};

function makeMetadataObject(qid, agents)
{
  return {
    'qid': qid,
    'aras': agents
  };
}

function makeMapping(key, transform, update, fallback)
{
  return obj =>
  {
    const val = cmn.jsonGet(obj, key) || false;
    return acc => { return update((val ? transform(val) : fallback, acc); }
  }
}

function aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate)
{
  const cv = cmn.jsonGetFromKpath(obj, kpath);
  if (doUpdate v)
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
    return cmn.jsonSetFromKpath(obj, kpath []);
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

function aggregateAndTransformProperty(key, kpath, transform)
{
  return makeMapping(
           src-key,
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
           'attributes'
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

function aggregateAndTransformAttributes(attributeIds tgtKey transform)
{
  makeMapping(
    'attributes'
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
    }
    (v, obj) =>
    {
      return cmn.jsonSet(obj, tgtKey, v);
    }
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

function diseaseToCreativeQuery(diseaseObj)
{
  function diseaseToTrapiQgraph(disease)
  {
    return {
      'nodes': {
        'drug': {
          'categories': [bl.tagBiolink('ChemicalEntity')]
        }
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

  return {
    'message': {
      'query_graph': diseaseToTrapiQgraph(cmn.jsonGet(diseaseObj, 'disease'));
    }
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

function rnodeToKey(rnode, kgraph)
{
  return rnode;
}

function redgeToKey(redge, kgraph, doInvert = false)
{
  const kedge = redgeToTrapiKedge(redge, kgraph);
  const ksubject = kedgeSubject(kedge);
  const kpredicate = kedgePredicate(kedge);
  const kobject = kedgeObject(kedge);
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
  let condensedSummary = {};
  condensedSummary.agent = () => { return agent; };
  condensedSummary.fragment = () => { return summaryFragment; };
  return condensedSummary;
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

function creativeAnswersToSummary (qid, answers)
{
  const nodeRules = makeSummarizeRules(
    [
      aggregateProperty('name', ['names']),
      aggregateProperty('categories', ['types']),
      aggregateAttributes(bl.tagBiolink('xref'), 'curies'),
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
      aggregateAttributes(bl.tagBiolink('description'), 'description'),
      aggregateAttributes(bl.tagBiolink('synonym'), 'synonym'),
      aggregateAttributes(bl.tagBiolink('same_as'), 'same_as'),
      aggregateAttributes(bl.tagBiolink('IriType'), 'iri_type')
    ]);

  const edgeRules = makeSummarizeRules(
    [
      aggregateAndTransformProperty(
        'predicate',
        ['predicates'],
        bl.sanitizePredicate),
      getProperty('subject'),
      getProperty('object'),
      aggregateAttributes(bl.tagBiolink('IriType'), 'iri_type'),
      aggregateAndTransformAttributes(
        ['bts:sentence'],
        'snippets',
        (snippets) =>
        {
          if (cmn.isArray(snippets))
          {
            return snippets
          }

          return Object.values(snippets);
        }),
      aggregateAndTransformAttributes(
        [
          bl.tagBiolink('supporting_document'),
          bl.tagBiolink('Publication'),
          bl.tagBiolink('publications')
        ],
        'publications'
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

  const maxHops = config.maxHops;
  return condensedSummariesToSummary(
           qid,
           creativeAnswersToCondensedSummaries(answers, nodeRules, edgeRules, maxHops));
}

function creativeAnswersToCondensedSummaries(answers, nodeRules, edgeRules, maxHops)
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
            if (!path.includes(target))
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
      return cmn.makePair(rnodeToKey(rnode, kgraph),
                          nodeRules(rnodeToTrapiKnode(rnode, kgraph)));
    }

    function summarizeRedge(redge, kgraph)
    {
      return cmn.makePair(redgeToKey(redge, kgraph),
                          edgeRules(redgeToTrapiKedge(redge, kgraph)));
    }

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

          for (let i = 0; i < pathLength; i+=2;)
          {
            const node = path[i];
            const edge = path[i+1];
            normalizedPath.push(N(node), E(edge, node));
          }

          normalizedPath.push(N(path[pathLength));
          return normalizedPath;
        });
    }

    return makeSummaryFragment(
      normalizePaths(rgraphPaths, kgraph),
      rgraph.nodes().map(node => { return summarizeRnode(rnode, kgraph); }),
      rgraph.edges().map(edge => { return summarizeRedge(redge, kgraph); })
    );
  }

  answers.map((answer) =>
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
      results.push(cmn.makePair(path[0], pathKey, 'disease', 'key'))
      paths.push(cmn.makePair(pathKey, path, 'key', 'path'))
    });

    return cmn.makePair(results, paths, 'results', 'paths');
  }

  function extendSummaryResults(results, newResults)
  {
    newResults.forEach((result) =>
    {
      // TODO
    }
  }
}
