'use strict'

import { describe, it } from 'node:test';
import * as assert from 'assert';
import * as cmn from '../common.mjs';

function getTestFile(filename)
{
  return `test/data/readJson/${filename}`;
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

describe('isString', () =>
  {
    it('Should return True if its argument is a string', () =>
      {
        assert.ok(cmn.isString(''));
        assert.ok(cmn.isString('123'));
        assert.ok(cmn.isString(new String('')));
        assert.ok(cmn.isString(new String('123')));
      });

    it('Should return False if its argument is not a string', () =>
      {
        assert.strictEqual(cmn.isString(1), false);
        assert.strictEqual(cmn.isString([]), false);
        assert.strictEqual(cmn.isString({'a': 1}), false);
        assert.strictEqual(cmn.isString(null), false);
        assert.strictEqual(cmn.isString(undefined), false);
        assert.strictEqual(cmn.isString(NaN), false);
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

describe('jsonHasKey', () =>
  {
    const testObj = {'a': {'b': 2}};
    it('Should return True if the key is in the top level object', () =>
      {
        assert.ok(cmn.jsonHasKey(testObj, 'a'));
      });

    it('Should return False if the top level object does not contain the key', () =>
      {
        assert.strictEqual(cmn.jsonHasKey(testObj, 'b'), false);
      });
  });

describe('jsonGet', () =>
  {
    const testObj = {'a': {'b': 1}};
    it('Should return the value in the top level object given a key', () =>
      {
        assert.strictEqual(cmn.jsonGet(testObj, 'a'), testObj['a']);
      });

    it('Should return a default value if provided and the key is not found in the top level object', () =>
      {
        const fallback = [1, 2, 3];
        assert.strictEqual(cmn.jsonGet(testObj, 'b', fallback), fallback);
      });

    it('Should throw an error if the key is not found in the top level object and no default is provided', () =>
      {
        assert.throws(() => { return cmn.jsonGet(testObj, 'b'); });
      });
  });

describe('jsonSet', () =>
  {
    it('Should be able to create a new key-value pair in the top level object', () =>
      {
        const testObj = {'c': 3};
        const val = {'b': 1};
        const actual = cmn.jsonSet(testObj, 'a', val);
        assert.strictEqual(actual, testObj);
        assert.strictEqual(cmn.jsonGet(testObj, 'a'), val);
      });

    it('Should be able to update an existing key-value pair in the top level object', () =>
      {
        const testObj = {'c': 3};
        const val = {'b': 1};
        const actual = cmn.jsonSet(testObj, 'c', val);
        assert.strictEqual(actual, testObj);
        assert.strictEqual(cmn.jsonGet(testObj, 'c'), val);
      });
  });

describe('jsonMultiSet', () =>
  {
    it('Should be able to update and create multiple new key-value pairs in the top level object', () =>
      {
        const previousValue = 3;
        const testObj = {'c': previousValue};
        const vals = [['v', 1], ['c', 2], ['b', {'a': 2}]];
        const actual = cmn.jsonMultiSet(testObj, vals);
        assert.strictEqual(actual, testObj);
        vals.forEach((val) => {
          assert.strictEqual(cmn.jsonGet(testObj, val[0]), val[1]);
        });
        assert.notEqual(cmn.jsonGet(testObj, 'c'), previousValue);
      });
  });

describe('jsonSetDefaultAndGet', () =>
  {
    const fallback = [1, 2, 3];
    it('Should return the value in the top level object given a key', () =>
      {
        const testObj = {'a': {'b': 1}};
        assert.strictEqual(cmn.jsonSetDefaultAndGet(testObj, 'a', fallback), testObj['a']);
      });

    it('Should set the key in the top level object to the fallback value if the key is not set', () =>
      {
        const testObj = {'a': {'b': 1}};
        assert.strictEqual(cmn.jsonSetDefaultAndGet(testObj, 'b', fallback), fallback);
      });

    it('Should throw if no fallback is provided or it is undefined', () =>
      {
        const testObj = {'a': {'b': 1}};
        assert.throws(() => { return cmn.jsonSetDefaultAndGet(testObj, 'a'); });
        assert.throws(() => { return cmn.jsonSetDefaultAndGet(testObj, 'a', undefined); });
      });
  });

describe('jsonGetFromKpath', () =>
  {
    const testObj = {'a': {'b': {'c': 1}}};
    const fallback = [1, 2];
    it('Should return the value in the object given a key path', () =>
      {
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, []), testObj);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a']), testObj['a']);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a', 'b']), testObj['a']['b']);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a', 'b', 'c']), testObj['a']['b']['c']);
      });

    it('Should return a fallback if provided and the key path is not valid', () =>
      {
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a', 'c'], fallback), fallback);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a', 'b', 'c', 'd'], fallback), fallback);
      });

    it('Should throw if no fallback is provided and the key path is not valid', () =>
      {
        assert.throws(() => { return cmn.jsonGetFromKpath(testObj, ['a', 'c']); });
        assert.throws(() => { return cmn.jsonGetFromKpath(testObj, ['a', 'b', 'c', 'd']); });
      });
  });

describe('jsonSetFromKpath', () =>
  {
    const newValue = {'d': 10};
    const kpath = ['a', 'b', 'c'];
    it('Should update a key given a key path', () =>
      {
        const testObj = {'a': {'b': {'c': 1}}};
        const actual = cmn.jsonSetFromKpath(testObj, kpath, newValue);
        assert.strictEqual(testObj, actual);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, kpath), newValue);
      });

    it('Should create the keypath if it does not exist in the object', () =>
      {
        const testObj = new Object();
        const actual = cmn.jsonSetFromKpath(testObj, kpath, newValue);
        console.log(testObj);
        console.log(actual);
        assert.strictEqual(testObj, actual);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, kpath), newValue);
      });
  });

describe('jsonUpdate', () =>
  {
    it('Should perform a transformation on a key', () =>
      {
        const testObj = {'a': {'b': 1}};
        const actual = cmn.jsonUpdate(testObj, 'a', (obj) => { return cmn.jsonSet(obj, 'c', 5) });
        assert.strictEqual(testObj, actual);
        assert.strictEqual(cmn.jsonGetFromKpath(testObj, ['a', 'c']), 5);
      });
  });
