'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import * as bl from '../biolink-model.mjs';

describe('tagBiolink tests', () =>
{
  it('Should tag with a biolink tag', () =>
  {
    assert.strictEqual(bl.tagBiolink('predicate'),
                       'biolink:predicate');
  });

  it('Should normalize with a biolink tag', () =>
  {
    assert.strictEqual(bl.tagBiolink('a predicate'),
                       'biolink:a_predicate');
  });
});

describe('isBiolinkPredicate tests', () =>
{
  describe('Should be a valid biolink predicate', () =>
  {
    it('Is a pre-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('biolink:treats'));
    });

    it('Is a normalized pre-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('biolink:treated_by'));
    });

    it('Is a non-normalized pre-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('biolink:treated by'));
    });

    it('Is a non-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('treats'));
    });

    it('Is a normalized non-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('treated_by'));
    });

    it('Is a non-normalized non-tagged predicate', () =>
    {
      assert.ok(bl.isBiolinkPredicate('treated by'));
    });
  });

  describe('Should not be a valid biolink predicate', () =>
  {
    it('Is empty', () =>
    {
      assert.strictEqual(bl.isBiolinkPredicate(''),
                         false);
    });

    it('Is gibberish', () =>
    {
      assert.strictEqual(bl.isBiolinkPredicate('askldjfha'),
                         false);
    });

    it('Looks like a biolink predicate', () =>
    {
      assert.strictEqual(bl.isBiolinkPredicate('biolink:normalized_google_distance'),
                         false);
    });
  });
});
