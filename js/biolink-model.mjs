'use strict'

import * as cmn from './common.mjs';

let BIOLINK_PREDICATES = null;
export async function loadBiolinkPredicates(biolinkVersion)
{
  const biolinkModel = await cmn.readJson(`../assets/biolink-model/${biolinkVersion}/biolink-model.json`);
  const slots = cmn.jsonGet(biolinkModel, 'slots');
  BIOLINK_PREDICATES = makeBlPredicates(slots);
}

export function tagBiolink(str)
{
  return biolinkifyPredicate(str);
}

export function isBiolinkPredicate(s)
{
  return BIOLINK_PREDICATES[sanitizeBiolinkElement(s)] !== undefined;
}

export function sanitizeBiolinkElement(pred)
{
  return pred.replaceAll('_', ' ').replaceAll('biolink:', '');
}

export function invertBiolinkPredicate(pred, biolinkify = false)
{
  const p = sanitizeBiolinkElement(pred);
  const biolinkPredicate = cmn.jsonGet(BIOLINK_PREDICATES, p, false);
  if (biolinkPredicate)
  {
    if (biolinkify)
    {
      return biolinkifyPredicate(biolinkPredicate.inverse);
    }

    return biolinkPredicate.inverse;
  }

  throw InvalidPredicateError(p);
}

function InvalidPredicateError(predicate)
{
  const error = new Error(`Expected a valid biolink predicate. Got: ${predicate}`, 'biolink-model.mjs');
}
InvalidPredicateError.prototype = Object.create(Error.prototype);

function getParent(pred, record)
{
  if (pred === 'related to')
  {
    return false;
  }

  return cmn.jsonGet(record, 'is_a', false);
}

function isCanonical(pred, record)
{
  return !!cmn.jsonGetFromKpath(record, ['annotations', 'canonical_predicate'], false);
}

function getSymmetric(pred, record)
{
  return cmn.jsonGet(record, 'symmetric', false);
}

function isSymmetric(pred, record)
{
  return !!getSymmetric(pred, record);
}

function isDeprecated(pred, record)
{
  return cmn.jsonGet(record, 'deprecated', false);
}

function getInverse(pred, record)
{
  if (isSymmetric(pred, record))
  {
    return pred;
  }

  return cmn.jsonGet(record, 'inverse', false);
}

function distanceFromRelatedTo(slots, predicate)
{
  for (let level = 0; ;level += 1)
  {
    if (predicate === 'related to')
    {
      return level;
    }

    let predicateObj = cmn.jsonGet(slots, predicate, false);
    if (!predicateObj)
    {
      return -1;
    }

    predicate = cmn.jsonGet(predicateObj, 'is_a', false);
  }
}

function makeBlPredicate(predicate, record, rank)
{
  return {
    'parent': getParent(predicate, record),
    'isCanonical': isCanonical(predicate, record),
    'isSymmetric': isSymmetric(predicate, record),
    'isDeprecated': isDeprecated(predicate, record),
    'inverse': getInverse(predicate, record),
    'rank': rank,
  };
}

function makeBlPredicates(slots)
{
  let blPreds = {};
  Object.keys(slots).forEach((pred) =>
    {
      const rank = distanceFromRelatedTo(slots, pred);
      if (rank >= 0)
      {
        const record = slots[pred];
        blPreds[pred] = makeBlPredicate(pred, record, rank);
      }
    });

  Object.keys(blPreds).forEach((pred) =>
    {
      const record = blPreds[pred];
      const inversePred = record.inverse;
      if (inversePred)
      {
        let inverseRecord = cmn.jsonGet(blPreds, inversePred);
        inverseRecord.inverse = pred;
        blPreds[inversePred] = inverseRecord;
      }
    });

  return blPreds;
}

function biolinkifyPredicate(pred)
{
  let s = pred.replaceAll(' ', '_');
  if (s.startsWith('biolink:'))
  {
    return s;
  }

  return `biolink:${s}`;
}

function getBiolinkPredicateData(pred)
{
  return cmn.jsonGet(BIOLINK_PREDICATES, sanitizeBiolinkElement(pred), false);
}

function isBiolinkPredicateMoreSpecific(pred1, pred2)
{
  const p1 = sanitizeBiolinkElement(pred1);
  const p2 = sanitizeBiolinkElement(pred2);
  const biolinkPredicate1 = getBiolinkPredicateData(p1);
  const biolinkPredicate2 = getBiolinkPredicateData(p2);

  if (!biolinkPredicate1)
  {
    throw InvalidPredicateError(p1);
  }
  else if (!biolinkPredicate2)
  {
    throw InvalidPredicateError(p2);
  }
  else
  {
    return pred1.rank > pred2.rank;
  }
}

function sortBiolinkPredicates(preds)
{
  return sort(pres, isBiolinkPredicateMoreSpecific);
}
