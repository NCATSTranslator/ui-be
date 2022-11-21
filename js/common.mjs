'use strict'

import * as fs from 'fs';

export async function readJson(path)
{
  const content = await fs.promises.readFile(path);
  return JSON.parse(content);
}

export function deepCopy(o)
{
  // TODO: Inefficient
  return JSON.parse(JSON.stringify(o));
}

export function identity(x)
{
  return x;
}

export function makePair(x, y, xLabel = false, yLabel = false)
{
  const firstId = xLabel || 'first';
  const secondLabel = yLabel || 'second';
  let pair = {};
  pair[firstId] = () => { return x };
  pair[secondLabel] = () => { return y; };
  return pair;
}

export function isString(v)
{
  return typeof v === 'string' || v instanceof String;
}

export function isArray(v)
{
  return Array.isArray(v);
}

export function isArrayEmpty(a)
{
  if (isArray(a))
  {
    return a.length === 0;
  }

  throw new TypeError(`Expected array got ${a}`);
}

export function setUnion(sets)
{
  return sets.reduce((unionedSet, set) =>
    {
      [...set.keys()].forEach((key) =>
        {
          unionedSet.add(key);
        });

      return unionedSet;
    },
    new Set());
}

export function isObj(o)
{
  return typeof o === 'object' &&
         o !== null &&
         !isArray(o);
}

export function isObjEmpty(o)
{
  if (isObj(o))
  {
    return Object.keys(o).length === 0;
  }

  throw new TypeError(`Expected object got ${o}`);
}

export function jsonHasKey(obj, key)
{
  return obj[key] !== undefined
}

export function jsonGet(obj, key, fallback = undefined)
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

  throw new ReferenceError(`Key: '${key}' not found and no default provided`, 'common.mjs');
}

export function jsonSet(obj, key, v)
{
  obj[key] = v;
  return obj;
}

export function jsonMultiSet(obj, kvps)
{
  kvps.forEach((kvp) =>
  {
    const [key, v] = kvp;
    jsonSet(obj, key, v);
  });

  return obj;
}

export function jsonSetDefaultAndGet(obj, key, fallback)
{
  if (fallback === undefined)
  {
    throw new TypeError('fallback must be provided and not undefined');
  }

  let v = obj[key];
  if (v !== undefined)
  {
    return v;
  }

  obj[key] = fallback;
  return fallback;
}

export function jsonGetFromKpath(obj, kpath, fallback = undefined)
{
  let currentObj = obj;
  kpath.forEach(k =>
  {
    if (currentObj)
    {
      currentObj = jsonGet(currentObj, k, fallback);
    }
    else
    {
      return fallback;
    }
  });

  return currentObj;
}

export function jsonSetFromKpath(obj, kpath, v)
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
    let nextObj = jsonGet(currentObj, k, false);
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

export function jsonUpdate(obj, key, update)
{
  return jsonSet(obj, key, update(jsonGet(obj, key)));
}
