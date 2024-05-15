'use strict'

import * as cmn from './common.mjs';
import * as bl from './biolink-model.mjs';

let SUBJECT_KEY = null;
let OBJECT_KEY = null;

const QUERY_TYPE = {
  CHEMICAL_GENE: 0,
  CHEMICAL_DISEASE: 1,
  GENE_CHEMICAL: 2
}

export function loadTrapi(trapiConfig) {
  SUBJECT_KEY = trapiConfig.query_subject_key
  OBJECT_KEY = trapiConfig.query_object_key
}

export function queryToTrapiQuery(query) {
  function buildTrapiQueryGraph(subject, object, predicate, direction) {
    function nodeToQgNode(node) {
      const qgNode = {};
      qgNode['categories'] = [bl.tagBiolink(node.category)];
      if (node.id) {
        qgNode['ids'] = [node.id];
      }

      return qgNode;
    }

    const qgNodes = {};
    qgNodes[SUBJECT_KEY] = nodeToQgNode(subject);
    qgNodes[OBJECT_KEY] = nodeToQgNode(object);

    const qgEdge = {
      'subject': SUBJECT_KEY,
      'object': OBJECT_KEY,
      'predicates': [bl.tagBiolink(predicate)],
      'knowledge_type': 'inferred',
    };

    if (direction) {
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

  function diseaseToTrapiQgraph(disease) {
    return buildTrapiQueryGraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Disease', 'id': disease},
      'treats',
      null);
  }

  function geneToTrapiQgraph(gene, direction) {
    return buildTrapiQuerygraph(
      {'category': 'ChemicalEntity'},
      {'category': 'Gene', 'id': gene},
      'affects',
      direction);
  }

  function chemicalToTrapiQgraph(chemical, direction) {
    return buildTrapiQueryGraph(
      {'category': 'ChemicalEntity', 'id': chemical},
      {'category': 'Gene'},
      'affects',
      direction);
  }

  if (!cmn.isObj(query)) {
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

  return makeTrapiMessage(qg, null);
}

export function nodeIdsToTrapiMessage(nodeIds) {
  return makeTrapiMessage(null, makeKnowledgeGraphFromNodeIds(nodeIds));
}

export function getQueryGraph(trapiMessage) {
  return cmn.jsonGetFromKpath(trapiMessage, ['message', 'query_graph'], false);
}

/*
 * Determine the query template type based on an answer.
 *
 * @param {object} answer - The answer to determine the query type from.
 *
 * @returns {number} - The query type.
 */
export function messageToQueryType(message) {
  const qg = getQueryGraph(message);
  if (!qg)
  {
    return false;
  }

  const [SUBJECT_KEY, OBJECT_KEY] = messageToEndpoints(message);

  const subCategory = cmn.jsonGetFromKpath(qg, ['nodes', SUBJECT_KEY, 'categories'], false)[0];
  const objCategory = cmn.jsonGetFromKpath(qg, ['nodes', OBJECT_KEY, 'categories'], false)[0];
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

/*
 * Determine which keys correspond to the start and end points of all graphs
 *
 * @param {object} message - The message to determine the start and end keys from.
 *
 * @returns {string[]} - The keys corresponding to the start and end points
 */
export function messageToEndpoints(message) {
  const qg = getQueryGraph(message);
  const startIsObject = cmn.jsonGetFromKpath(qg, ['nodes', SUBJECT_KEY, 'ids'], false);
  if (startIsObject) {
    return [OBJECT_KEY, SUBJECT_KEY];
  }

  return [SUBJECT_KEY, OBJECT_KEY];
}

export function isChemicalGeneQuery(queryType) {
  return queryType === QUERY_TYPE.CHEMICAL_GENE;
}

export function isChemicalDiseaseQuery(queryType) {
  return queryType === QUERY_TYPE.CHEMICAL_DISEASE;
}

export function isGeneChemicalQuery(queryType) {
  return queryType === QUERY_TYPE.GENE_CHEMICAL;
}

export function isValidQuery(queryType) {
  return Object.values(QUERY_TYPE).includes(queryType);
}

/*
 * Create a minimal TRAPI message from a collection of node CURIEs
 *
 * @param {Array} nodeIds - Array of CURIEs
 * @returns {Object} - TRAPI message
 */
function makeKnowledgeGraphFromNodeIds(nodeIds) {
  const nodes = {};
  nodeIds.forEach(id => {
    if (bl.isValidCurie(id)) {
      nodes[id] = {};
    }
  });

  return {
    edges: {},
    nodes: nodes
  };
}

function makeTrapiMessage(queryGraph, knowledgeGraph) {
  const message = {};
  if (queryGraph) {
    message['query_graph'] = queryGraph;
  }

  if (knowledgeGraph) {
    message['knowledge_graph'] = knowledgeGraph;
  }

  return { 'message': message };
}

