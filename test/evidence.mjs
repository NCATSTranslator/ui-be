import { describe, it } from 'node:test';
import * as assert from 'assert';
import { isValidId, idToTypeAndUrl } from '../evidence.mjs';

describe('isValidId', () =>
  {
    it('Should return True when the ID has a valid prefix (PMID, NCT, clinicaltrialsNCT, DOI)', () =>
      {
        assert.ok(isValidId('PMID:123'));
        assert.ok(isValidId('NCT123'));
        assert.ok(isValidId('clinicaltrialsNCT123'));
        assert.ok(isValidId('DOI:123'));
      });

    it('Should return False when the ID has an invalid prefix', () =>
      {
        assert.strictEqual(isValidId('Not a valid ID'), false);
      });
  });

describe('idToTypeAndUrl', () =>
  {
    it('Should split a PMID', () =>
      {
        assert.deepEqual(idToTypeAndUrl('PMID:123'), ['PMID', 'https://pubmed.ncbi.nlm.nih.gov/123']);
      });

    it('Should split an NCT', () =>
      {
        assert.deepEqual(idToTypeAndUrl('NCT123'), ['NCT', 'https://clinicaltrials.gov/ct2/show/NCT123']);
      });

    it('Should split a DOI', () =>
      {
        assert.deepEqual(idToTypeAndUrl('DOI:123'), ['DOI', 'https://www.doi.org/123']);
      });

    it('Should split an aragorn NCT', () =>
      {
        assert.deepEqual(idToTypeAndUrl('clinicaltrialsNCT123'),
                           ['NCT', 'https://clinicaltrials.gov/ct2/show/NCT123']);
      });

    it('Should have an unknown URL if the ID has a valid prefix but no ID', () =>
      {
        assert.deepEqual(idToTypeAndUrl('PMID:'), ['PMID', 'Unknown']);
      });

    it('Should have an unknown type and URL if the ID is unrecognizable', () =>
      {
        assert.deepEqual(idToTypeAndUrl('What?'), ['Unknown', 'Unknown']);
      });
  });
