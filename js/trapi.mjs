'use strict';

import * as cmn from './common.mjs';
import { isBiolinkPredicate } from './biolink-model.mjs';

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
    const val = cmn.jsObjGet(obj, key) || false;
    return acc => { return update((val ? transform(val) : fallback, acc); }
  }
}

function aggregatePropertyUpdateWhen(v, obj, kpath, doUpdate)
{
  const cv = cmn.jsObjGetFromKpath(obj, kpath);
  if (doUpdate v)
  {
    const uv = cmn.isArray(v) ? v : [v];
    return cmn.jsObjSetFromKpath(obj, kpath, cv ? v.concat(cv) : v);
  }
  else if (cv)
  {
    return obj
  }
  else
  {
    return cmn.jsObjSetFromKpath(obj, kpath []);
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
          (v, obj) => { return cmn.jsObjSetFromKpath(obj, kpath, v); },
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
             const cv = cmn.jsObjGet(obj, key);
             if (update(v))
             {
               return cmn.jsObjSet(obj, key, v);
             }
             else if (cv)
             {
               return obj;
             }

             return cmn.jsObjSet(obj, key, null);
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
  return cmn.jsObjGet(attribute, 'attribute_type_id');
}

function attrValue(attribute)
{
  return cmn.jsObjGet(attribute, 'value');
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
           (v, obj) => { return cmn.jsObjSetFromKpath(obj, kpath, v); },
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
      return cmn.jsObjSet(obj, tgtKey, v);
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
          'categories': [cmn.biolinkTag('ChemicalEntity')]
        }
        'disease': {
          'ids': [disease],
          'categories': [cmn.biolinkTag('Disease')]
        }
      },
      'edges': {
        'treats': {
          'subject': 'drug',
          'object': 'disease',
          'predicates': [cmn.biolinkTag('treats')],
          'knowledge_type': 'inferred'
        }
      }
    }
  }

  return {
    'message': {
      'query_graph': diseaseToTrapiQgraph(cmn.jsObjGet(diseaseObj, 'disease'));
    }
  };
}

function trapiBindingToKobj(binding, type, kgraph)
{
  return cmn.jsObjGet(cmn.jsObjGet(kgraph, type), binding);
}

function trapiEdgeBindingToTrapiKedge(edgeBinding, kgraph)
{
  return trapiBindingToKobj(edgeBinding, 'edges', kgraph);
}

function trapiNodeBindingToTrapiKnode(nodeBinding, kgraph)
{
  return trapiBindingToKobj(nodeBinding, 'nodes', kgraph);
}

function getBindingId(bindings, key)
{
  return cmn.jsObjGet(cmn.jsObjGet(bindings, key), 'id');
}

function flattenBindings(bindings)
{
  return bindings.reduce((binding, ids) =>
  {
    return ids.concat(binding.map(obj => { return cmn.jsObjGet(obj, 'id'); }));
  },
  []);
}

function kedgeSubject(kedge)
{
  return cmn.jsObjGet(kedge, 'subject');
}

function kedgeObject(kedge)
{
  return cmn.jsObjGet(kedge, 'object');
}

function kedgePredicate(kedge)
{
  return cmn.jsObjGet(kedge, 'predicate');
}

function makeRgraph(rnodes, redges, kgraph)
{
  return {
    'nodes': rnodes,
    'edges': redges.filter(redge =>
             {
               const kedge = trapiEdgeBindingToTrapiKedge(redge, kgraph);
               return isBiolinkPredicate(kedgePredicate(kedge));
             }
  };
}

function isRedgeInverted(redge, object, kgraph)
{
  const kgedge = trapiEdgeBindingToTrapiKedge(redge, kgraph);
  return object === kedgeSubject(kedge);
}

function trapiResultToRgraph(trapiResult, kgraph)
{
  return makeRgraph(flattenBindings(cmn.jsObjGet(trapiResult, 'node_bindings')),
                    flattenBindings(cmn.jsObjGet(trapiResult, 'edge_bindings')),
                    kgraph);
}

function rnodeToKey(rnode, kgraph)
{
  return rnode;
}

function redgeToKey(redge, kgraph, doInvert = false)
{
  const kedge = trapiEdgeBindingToTrapiKedge(redge, kgraph);
  const ksubject = kedgeSubject(kedge);
  const kpredicate = kedgePredicate(kedge);
  const kobject = kedgeObject(kedge);
  if (doInvert)
  {
    return pathToKey([kobject, invertBiolinkPredicate(kpredicate), ksubject]);
  }

  return pathToKey([ksubject, kpredicate, kobject]);
}


function creativeAnswersToSummary (qid, answers)
{
  const nodeRules = makeSummarizeRules(); // TODO
  const edgeRules = makeSummarizeRules(); // TODO
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
                          nodeRules(trapiNodeBindingToTrapiKnode(rnode, kgraph)));
    }

    function summarizeRedge(redge, kgraph)
    {
      return cmn.makePair(redgeToKey(redge, kgraph),
                          edgeRules(trapiEdgeBindingToTrapiKedge(redge, kgraph)));
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
      rgraph.nodes.map(node => { return summarizeRnode(rnode, kgraph); }),
      rgraph.edges.map(edge => { return summarizeRedge(redge, kgraph); })
    );
  }
}
