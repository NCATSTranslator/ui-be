'use strict'

import * as cmn from '../lib/common.mjs';
import { makeMetadataObject } from '../lib/trapi.mjs';

export async function postQuery(query)
{
  return makeQueryState('success',
                        cmn.jsonGetFromKpath(query,
                                             ['message',
                                              'query_graph',
                                              'nodes',
                                              'disease',
                                              'ids'])[0]);
}

export async function pullQueryStatus(qid)
{
  if (cmn.jsonHasKey(curiesToPaths, qid))
  {
    return makeQueryState('success', makeMetadataObject(qid, ['mock']));
  }

  return makeQueryState('error', `No key ${qid}`);
}

export async function pullQueryAnswers(qid)
{
  const paths = cmn.jsonGet(curiesToPaths, qid, false);
  if (paths)
  {
    const answers = await Promise.all(paths.map((path) =>
      {
        return cmn.readJson(`../${path}`);
      }));

    return makeQueryState('success', answers.map((answer) =>
      {
        return {
          'message': cmn.jsonGetFromKpath(answer, ['fields', 'data', 'message']),
          'agent': 'mock'
        };
      }));
  }

  return makeQueryState('error', `No key ${qid}`);
}

const curiesToPaths = await cmn.readJson('./mock/data/ars/curie-to-answer-table.json');
function makeQueryState(status, data)
{
  return {
    'status': status,
    'data': data
  };
}
