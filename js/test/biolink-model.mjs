'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import * as bl from '../biolink-model.mjs';

describe('tagBiolink', () =>
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

describe('isBiolinkPredicate', () =>
{
  it('Should report True when the input is a valid biolink predicate', () =>
  {
    assert.ok(bl.isBiolinkPredicate('biolink:treats'));
    assert.ok(bl.isBiolinkPredicate('biolink:treated_by'));
    assert.ok(bl.isBiolinkPredicate('biolink:treated by'));
    assert.ok(bl.isBiolinkPredicate('treats'));
    assert.ok(bl.isBiolinkPredicate('treated_by'));
    assert.ok(bl.isBiolinkPredicate('treated by'));
  });

  it('Should report False when the input is not a valid biolink predicate', () =>
  {
      assert.strictEqual(bl.isBiolinkPredicate(''),
                         false);
      assert.strictEqual(bl.isBiolinkPredicate('askldjfha'),
                         false);
      assert.strictEqual(bl.isBiolinkPredicate('biolink:normalized_google_distance'),
                         false);
  });
});
