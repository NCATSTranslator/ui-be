'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import * as cmn from '../common.mjs';

function getTestFile(filename)
{
  return `test/test-data/readJson/${filename}`;
}

describe('readJson', () =>
  {
    it('Should read a valid JSON file', async () =>
      {
        const testObj = await cmn.readJson(getTestFile('valid.json'));
        assert.deepStrictEqual(testObj, {'valid': true});
      });

    it('Should reject on an invalid JSON file', () =>
      {
        assert.rejects(cmn.readJson(getTestFile('invalid.json')))
      });

    it('Should reject on a missing JSON file', () =>
      {
        assert.rejects(cmn.readJson(getTestFile('missing.json')));
      });
  });

describe('deepCopy', () =>
  {
    it('Should perform a deep copy', () =>
      {
        const testObj = {'x': {'y': 2}};
        let copiedObj = cmn.deepCopy(testObj);
        assert.deepStrictEqual(testObj, copiedObj);

        // Ensure the copy is deep
        copiedObj['x']['z'] = 3;
        assert.notDeepStrictEqual(testObj, copiedObj);
      });
  });

describe('identity', () =>
  {
    it('Should return what is passed in', () =>
      {
        assert.strictEqual(cmn.identity(1), 1);
        assert.strictEqual(cmn.identity('abc'), 'abc');
        assert.strictEqual(cmn.identity(null), null);
        assert.strictEqual(cmn.identity(undefined), undefined);

        let testArray = [1,2,3];
        let sameArray = cmn.identity(testArray);
        assert.strictEqual(testArray, sameArray);
        // Ensure that the arrays are the same entity
        testArray.push(4);
        assert.strictEqual(testArray, sameArray);

        let testObj = {'x': {'y': 2}};
        let sameObj = cmn.identity(testObj);
        assert.deepStrictEqual(testObj, sameObj);
        // Ensure that the objects are the same entity
        testObj['x']['z'] = 4;
        assert.deepStrictEqual(testObj, sameObj);

        let testFunc = () => { return 1 };
        let sameFunc = cmn.identity(testFunc);
        assert.deepStrictEqual(testFunc, sameFunc);
        assert.strictEqual(testFunc(), sameFunc());
        // Ensure that the functions are the same entity
        sameFunc.x = 2;
        assert.deepStrictEqual(testFunc, sameFunc);
        assert.strictEqual(testFunc(), sameFunc());
      });
  });

describe('makePair', () =>
  {
    it('Should return a pair', () =>
      {
        const testPair = cmn.makePair(1, {'x': {'y': 2}});
        assert.strictEqual(testPair.first(), 1);
        assert.deepStrictEqual(testPair.second(), {'x': {'y': 2}});
      });

    it('Should return a named pair', () =>
      {
        const testPair = cmn.makePair(1, {'x': {'y': 2}}, 'x', 'y');
        assert.strictEqual(testPair.x(), 1);
        assert.deepStrictEqual(testPair.y(), {'x': {'y': 2}});
      });
  });

describe('isArray', () =>
  {
    it('Should return True if its argument is an array', () =>
      {
        assert.ok(cmn.isArray([]));
        assert.ok(cmn.isArray([1, 2, 3]));
      });

    it('Should return False if its argument is not an array', () =>
      {
        assert.strictEqual(cmn.isArray(1), false);
        assert.strictEqual(cmn.isArray('a'), false);
        assert.strictEqual(cmn.isArray({'a': 1}), false);
        assert.strictEqual(cmn.isArray(null), false);
        assert.strictEqual(cmn.isArray(undefined), false);
        assert.strictEqual(cmn.isArray(NaN), false);
      });
  });

describe('isArrayEmpty', () =>
  {
    it('Should return True if the argument is an array and empty', () =>
      {
        assert.ok(cmn.isArrayEmpty([]));
      });

    it('Should return False if the argument is an array and non-empty', () =>
      {
        assert.strictEqual(cmn.isArrayEmpty([1]), false);
      });

    it('Should throw an exception if the argument is not an array', () =>
      {
        assert.throws(() => { return cmn.isArrayEmpty(1); });
        assert.throws(() => { return cmn.isArrayEmpty('a'); });
        assert.throws(() => { return cmn.isArrayEmpty({'a': 1}); });
        assert.throws(() => { return cmn.isArrayEmpty(null); });
        assert.throws(() => { return cmn.isArrayEmpty(undefined); });
        assert.throws(() => { return cmn.isArrayEmpty(NaN); });
      });
  });

describe('setUnion', () =>
  {
    it('Should return an empty set when performing a union on no sets', () =>
      {
        assert.deepStrictEqual(cmn.setUnion([]), new Set());
      });

    it('Should return a copy of a set if there is only one set to union', () =>
      {
        const testSet = new Set([1, 2, 3]);
        const unionedSet = cmn.setUnion([testSet]);
        assert.deepStrictEqual(unionedSet, testSet);
        assert.notStrictEqual(unionedSet, testSet);
      });

    it('Should union multiple sets', () =>
      {
        assert.deepStrictEqual(cmn.setUnion([new Set([1, 2, 3]),
                                             new Set([2, 3, 4]),
                                             new Set([3, 4, 5])]),
                               new Set([1, 2, 3, 4, 5]));
      });
  });

describe('isObj', () =>
  {
    it('Should return true if its argument is an object', () =>
      {
        assert.ok(cmn.isObj({}));
        assert.ok(cmn.isObj({'a': 1}));
      });

    it('Should return false if its argument is not an object', () =>
      {
        assert.strictEqual(cmn.isObj(1), false);
        assert.strictEqual(cmn.isObj('a'), false);
        assert.strictEqual(cmn.isObj([]), false);
        assert.strictEqual(cmn.isObj(undefined), false);
        assert.strictEqual(cmn.isObj(null), false);
        assert.strictEqual(cmn.isObj(NaN), false);
      });
  });

describe('isObjEmpty', () =>
  {
    it('Should return true if its argument is an object and has no keys', () =>
      {
        assert.ok(cmn.isObjEmpty({}));
        assert.ok(cmn.isObjEmpty(new Object()));
      });

    it('Should return false if its argument is an object and has keys', () =>
      {
        assert.strictEqual(cmn.isObjEmpty({'a': 1}), false);
      });

    it('Should throw if its argument is not an object', () =>
      {
        assert.throws(() => { return cmn.isObjEmpty(1) });
        assert.throws(() => { return cmn.isObjEmpty('a') });
        assert.throws(() => { return cmn.isObjEmpty([]) });
        assert.throws(() => { return cmn.isObjEmpty(undefined) });
        assert.throws(() => { return cmn.isObjEmpty(null) });
        assert.throws(() => { return cmn.isObjEmpty(NaN) });
      });
  });
