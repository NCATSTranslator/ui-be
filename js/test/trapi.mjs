'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import { readJson } from '../common.mjs';
import * as trapi from '../trapi.mjs';

describe('makeMetadataObject', () =>
  {
    it('Should return a metadata object when given a qid and agents', () =>
      {
        const mo = trapi.makeMetadataObject('123', ['a', 'b']);
        assert.deepEqual(mo, {'qid': '123', 'aras': ['a', 'b']});
      });

    it('Should throw if not given a qid or agents', () =>
      {
        assert.throws(() => { return trapi.makeMetadataObject('123'); });
        assert.throws(() => { return trapi.makeMetadataObject(undefined, ['a']); });
      });

    it('Should throw if the qid is not a string', () =>
      {
        assert.throws(() => { return trapi.makeMetadataObject(123, ['a']); });
      });

    it('Should throw if the agents are not an array', () =>
      {
        assert.throws(() => { return trapi.makeMetadatObject('123', 6); });
      });
  });

describe('diseaseToCreativeQuery', () =>
  {
    it('Should generate a valid TRAPI creative query given a disease object', () =>
      {
        const curie = 'AWESOME:123';
        const query = {
          'message': {
            'query_graph': {
              'nodes': {
                'drug': {
                  'categories': ['biolink:ChemicalEntity']
                },
                'disease': {
                  'ids': [curie],
                  'categories': ['biolink:Disease']
                }
              },
              'edges': {
                'treats': {
                  'subject': 'drug',
                  'object': 'disease',
                  'predicates': ['biolink:treats'],
                  'knowledge_type': 'inferred'
                }
              }
            }
          }
        };
        const diseaseObject = {'disease': curie};

        assert.deepEqual(trapi.diseaseToCreativeQuery(diseaseObject), query);
      });

    it('Should throw if the disease object is malformed', () =>
      {
        assert.throws(() => { return trapi.diseaseToCreativeQuery({}); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery({'abc': 'AWESOME:123'}); });
      });

    it('Should throw if the disease object is not an object', () =>
      {
        assert.throws(() => { return trapi.diseaseToCreativeQuery(undefined); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery('AWESOME:123'); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery(['disease', 'AWESOME:123']); });
        assert.throws(() => { return trapi.diseaseToCreativeQuery('{"disease": "AWESOME:123"}'); });
      });
  });

describe('creativeAnswersToSummary', () =>
  {
    it('Should return an empty summary for an empty answers array', async () =>
      {
        const emptySummary = await readJson('test/data/trapi/empty-summary.json');
        assert.deepEqual(trapi.creativeAnswersToSummary('AWESOME:123', []), emptySummary);
      });
  });
