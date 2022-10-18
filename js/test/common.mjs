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

// TODO
//describe('makePair', () =>
//{
//  if('Should return a pair', () =>
//  {
//    const testPair = cmn.makePair(1, {'x': {'y': 2}});
//    assert.strictEqual(
//  }
//
//}
