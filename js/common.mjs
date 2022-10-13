'use strict'

function identity(x)
{
  return x;
}

function makePair(x, y, xLabel = false, yLabel = false)
{
  const firstId = xLabel || 'first';
  const secondLabel = yLabel || 'second';
  let pair = {};
  pair[firstId] = () => { return x };
  pair[secondLabel] = () => { return y; };
  return pair;
}

function isArray(v)
{
  return Array.isArray(v);
}

function isArrayEmpty(a)
{
  return a.length === 0;
}

function jsonHasKey(obj, key)
{
  return obj[key] !== undefined
}

function jsonGet(obj, key, fallback = null)
{
  const v = obj[key];
  if (v !== undefined)
  {
    return v
  }

  if (fallback !== null)
  {
    return fallback
  }

  throw ReferenceError(`Key: ${key} not found and no default provided`, 'common.mjs');
}

function jsonSet(obj, key, v)
{
  obj[key] = v;
  return obj;
}

function jsonGetFromKpath(obj, kpath, fallback = false)
{
  let currentObj = obj;
  kpath.forEach(k =>
  {
    if (currentObj)
    {
      currentObj = jsonGet(currentObj, k);
    }
    else
    {
      return fallback;
    }
  });

  return currentObj;
}

function jsonSetFromKpath(obj, kpath, v)
{
  if (isArrayEmpty(kpath))
  {
    return obj;
  }

  let finalKey = kpath[kpath.length-1];
  let partialKpath = kpath.slice(0, -1);
  let currentObj = obj;
  partialKpath.forEach(k =>
  {
    let nextObj = jsonGet(currentObj, k);
    if (!nextObj)
    {
      nextObj = {};
      jsonSet(currentObj, k, nextObj);
    }

    currentObj = nextObj;
  });

  jsonSet(currentObj, finalKey, v);
  return obj;
}

function biolinkTag(str)
{
  return `biolink:${str}`;
}
