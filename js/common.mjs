'use strict'

function identity(x)
{
  return x;
}

function makePair(x, y)
{
  return {
    'first': x,
    'second': y
  };
}

function isArray(v)
{
  return Array.isArray(v);
}

function isArrayEmpty(a)
{
  return a.length === 0;
}

function jsObjGet(obj, key, fallback = false)
{
  return obj[key] || fallback;
}

function jsObjSet(obj, key, v)
{
  obj[key] = v;
  return obj;
}

function jsObjGetFromKpath(obj, kpath, fallback = false)
{
  let currentObj = obj;
  kpath.forEach(k =>
  {
    if (currentObj)
    {
      currentObj = jsObjGet(currentObj, k);
    }
    else
    {
      return fallback;
    }
  });

  return currentObj;
}

function jsObjSetFromKpath(obj, kpath, v)
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
    let nextObj = jsObjGet(currentObj, k);
    if (!nextObj)
    {
      nextObj = {};
      jsObjSet(currentObj, k, nextObj);
    }

    currentObj = nextObj;
  });

  jsObjSet(currentObj, finalKey, v);
  return obj;
}

function biolinkTag(str)
{
  return `biolink:${str}`;
}
