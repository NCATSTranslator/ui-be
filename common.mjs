'use strict'

import * as fs from 'fs';

export const mimeJson = 'application/json';

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

export function capitalize(s)
{
  return `${s[0].toUpperCase()}${s.slice(1).toLowerCase()}`;
}

/* Different from above function in that for biolinkifying category names, we want to allow
 * internal case preservation for things like `RNA product'
 */
export function capitalizeFirstLetter(s)
{
  return `${s[0].toUpperCase()}${s.slice(1)}`;
}

export function titleize(s)
{
  s = s.toLowerCase();
  const exclusions = ['and', 'or', 'for'];
  const words = s.split(/\s+/).map((word) =>
    {
      if (!exclusions.includes(word))
      {
        return capitalize(word);
      }

      return word;
    });

  return words.join(' ');
}

export function identity(x)
{
  return x;
}

export function makePair(x, y, xLabel = false, yLabel = false)
{
  const firstId = xLabel || 'first';
  const secondLabel = yLabel || 'second';
  const pair = {};
  pair[firstId] = x;
  pair[secondLabel] = y;
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

export function setToObject(set)
{
  const obj = {};
  for (const k of set)
  {
    if (k !== undefined)
    {
      obj[k] = null;
    }
  }

  return obj;
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
  for (const k of kpath)
  {
    if (currentObj)
    {
      currentObj = jsonGet(currentObj, k, fallback);
    }
    else
    {
      return fallback;
    }
  }

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

export class ApplicationError extends Error
{
  constructor(message, httpCode) {
    super(message);
    this.name = this.constructor.name;
    this.httpCode = httpCode;
  }
}

export class ClientError extends ApplicationError
{
  constructor(message) {
    super(message, 400);
  }
}

export class ServerError extends ApplicationError
{
  constructor(message) {
    super(message, 500);
  }
}


export async function SendRecvJSON(url, method='GET', headers={}, body=null) {
  let options = {
      method: method,
      headers: {...headers}
  };
  options.headers['Content-type'] = 'application/json';
  if (body) {
      options.body = JSON.stringify(body);
  }
  let resp = await fetch(url, options);
  if (resp.ok) {
      return resp.json();
  } else {
      let errmsg = `ERROR: status: ${resp.status}; msg: '${resp.statusText}'`;
      throw new Error(errmsg);
  }
}

// Usage: await sleep(250);
export async function sleep(ms) {
  return new Promise((resolve) =>
    setTimeout(() => resolve(), ms)
  );
}

// Usage: await withTimeout(async () => sendRecvJSON(...), 1000);
// fun must be an asynch function.
export async function withTimeout(fun, ms)
{
  let timer;
  return Promise.race([
    fun(),
    new Promise((_r, rej) =>
      {
        return timer = setTimeout(rej, ms, new Error(`withTimeout exceeded timeout ${ms} ms.`));
      })]).finally(() => clearTimeout(timer));
}
