'use strict'

function deepCopy(o)
{
  return JSON.parse(JSON.stringify(o));
}

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

function isObjEmpty(o)
{
  return Object.keys(o).length === 0;
}

function jsonHasKey(obj, key)
{
  return obj[key] !== undefined
}

function jsonGet(obj, key, fallback = undefined)
{
  const v = obj[key];
  if (v !== undefined)
  {
    return v
  }

  if (fallback !== undefined)
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

function jsonMultiSet(obj, kvps)
{
  kvps.forEach((kvp) =>
  {
    [key, v] = kvp;
    jsonSet(obj, key, v);
  });

  return obj;
}

function jsonSetDefaultAndGet(obj, key, fallback)
{
  let v = obj[key];
  if (v !== undefined)
  {
    return v;
  }

  obj[key] = fallback;
  return fallback;
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

function jsonUpdate(obj, key, update)
{
  jsonSet(obj, key, update(jsonGet(obj, key)));
}

function biolinkTag(str)
{
  return `biolink:${str}`;
}
