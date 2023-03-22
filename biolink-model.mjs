'use strict'

import * as cmn from './common.mjs';

let BIOLINK_PREDICATES = null;
let BIOLINK_CLASSES = null;
let INFORES_CATALOG = null;
let DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = null;
let PREFIX_CATALOG = null;

export async function loadBiolink(biolinkVersion, supportDeprecatedPredicates, inforesCatalog, prefixCatalog) {
  const biolinkModel = await cmn.readJson(`./assets/biolink-model/${biolinkVersion}/biolink-model.json`);
  const slots = cmn.jsonGet(biolinkModel, 'slots');
  const classes = cmn.jsonGet(biolinkModel, 'classes');
  BIOLINK_PREDICATES = makeBlPredicates(slots);
  BIOLINK_CLASSES = makeBlClasses(classes);
  INFORES_CATALOG = await cmn.readJson(`./assets/biolink-model/common/${inforesCatalog}`);
  PREFIX_CATALOG = await cmn.readJson(`./assets/biolink-model/common/${prefixCatalog}`);
  if (supportDeprecatedPredicates) {
    DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = await cmn.readJson(`./assets/biolink-model/${biolinkVersion}/deprecated-predicate-mapping.json`);
  }
}

export function tagBiolink(str) {
  return biolinkifyPredicate(str);
}

export function isDeprecatedPredicate(s) {
  return !!DEPRECATED_TO_QUALIFIED_PREDICATE_MAP &&
         DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[sanitizeBiolinkPredicate(s)] !== undefined;
}

export function isBiolinkPredicate(s) {
  const sanitizedPredicate = sanitizeBiolinkPredicate(s);
  return BIOLINK_PREDICATES[sanitizedPredicate] !== undefined ||
         isDeprecatedPredicate(sanitizedPredicate);
}

export function sanitizeBiolinkPredicate(pred) {
  return pred.replaceAll('_', ' ').replaceAll('biolink:', '');
}

export function invertBiolinkPredicate(pred, biolinkify = false) {
  const p = sanitizeBiolinkPredicate(pred);
  const biolinkPredicate = cmn.jsonGet(BIOLINK_PREDICATES, p, false);
  if (biolinkPredicate) {
    if (biolinkify) {
      return biolinkifyPredicate(biolinkPredicate.inverse);
    }
    return biolinkPredicate.inverse;
  }
  throw InvalidPredicateError(p);
}

export function deprecatedPredicateToPredicateAndQualifiers(predicate) {
  const qualifiedPredicate = DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[predicate];
  return [qualifiedPredicate.predicate, qualifiedPredicate];
}

export function inforesToProvenance(infores) {
  const provenance = INFORES_CATALOG[infores];
  if (!provenance) {
    return false;
  }
  return provenance;
}

export function curieToUrl(curie) {
  const [curiePrefix, curieId] = curie.split(':');
  const url = PREFIX_CATALOG[curiePrefix];
  if (!url) {
    return false;
  }

  return `${url}${curieId}`;
}

function InvalidPredicateError(predicate) {
  const error = new Error(`Expected a valid biolink predicate. Got: ${predicate}`, 'biolink-model.mjs');
}
InvalidPredicateError.prototype = Object.create(Error.prototype);

function InvalidClassError(blClass) {
  const error = new Error(`Expected a valid biolink class. Got: ${blClass}`, 'biolink-model.mjs');
}
InvalidClassError.prototype = Object.create(Error.prototype);

function getParent(pred, record) {
  if (pred === 'related to') {
    return false;
  }

  return cmn.jsonGet(record, 'is_a', false);
}

function isCanonical(pred, record) {
  return !!cmn.jsonGetFromKpath(record, ['annotations', 'canonical_predicate'], false);
}

function getSymmetric(pred, record) {
  return cmn.jsonGet(record, 'symmetric', false);
}

function isSymmetric(pred, record) {
  return !!getSymmetric(pred, record);
}

function isDeprecated(pred, record) {
  return cmn.jsonGet(record, 'deprecated', false);
}

function getInverse(pred, record) {
  if (isSymmetric(pred, record)) {
    return pred;
  }

  return cmn.jsonGet(record, 'inverse', false);
}

function distanceFromRelatedTo(slots, predicate) {
  for (let level = 0; ;level += 1) {
    if (predicate === 'related to') {
      return level;
    }

    let predicateObj = cmn.jsonGet(slots, predicate, false);
    if (!predicateObj) {
      return -1;
    }

    predicate = cmn.jsonGet(predicateObj, 'is_a', false);
  }
}

function makeBlPredicate(predicate, record, rank) {
  return {
    'parent': getParent(predicate, record),
    'isCanonical': isCanonical(predicate, record),
    'isSymmetric': isSymmetric(predicate, record),
    'isDeprecated': isDeprecated(predicate, record),
    'inverse': getInverse(predicate, record),
    'rank': rank,
  };
}

function makeBlPredicates(slots) {
  let blPreds = {};
  Object.keys(slots).forEach((pred) => {
      const rank = distanceFromRelatedTo(slots, pred);
      if (rank >= 0) {
        const record = slots[pred];
        blPreds[pred] = makeBlPredicate(pred, record, rank);
      }
    });

  Object.keys(blPreds).forEach((pred) => {
      const record = blPreds[pred];
      const inversePred = record.inverse;
      if (inversePred) {
        let inverseRecord = cmn.jsonGet(blPreds, inversePred);
        inverseRecord.inverse = pred;
        blPreds[inversePred] = inverseRecord;
      }
    });

  return blPreds;
}

function biolinkifyPredicate(pred) {
  let s = pred.replaceAll(' ', '_');
  if (s.startsWith('biolink:')) {
    return s;
  }

  return `biolink:${s}`;
}

function getBiolinkPredicateData(pred) {
  return cmn.jsonGet(BIOLINK_PREDICATES, sanitizeBiolinkPredicate(pred), false);
}

function isBiolinkPredicateMoreSpecific(pred1, pred2) {
  const p1 = sanitizeBiolinkPredicate(pred1);
  const p2 = sanitizeBiolinkPredicate(pred2);
  const biolinkPredicate1 = getBiolinkPredicateData(p1);
  const biolinkPredicate2 = getBiolinkPredicateData(p2);

  if (!biolinkPredicate1) {
    throw InvalidPredicateError(p1);
  }
  else if (!biolinkPredicate2) {
    throw InvalidPredicateError(p2);
  }
  else {
    return pred1.rank > pred2.rank;
  }
}

function sortBiolinkPredicates(preds) {
  return sort(pres, isBiolinkPredicateMoreSpecific);
}

/* Biolink category-related functions
 * Categories are entities within 'classes' in the biolink model.
 * The nodes we are most interested in are ones that descend from the class "entity", whose description states:
 * 'Root Biolink Model class for all things and informational relationships, real or imagined.'
 * There are a number of top-level classes in the 'classes' field that are not descended from 'entity',
 * and we don't do anything very special with them besides treating them as rank 0 items, (i.e. least
 * specific). Some of these are classes that say `mixin: true` or have `mixins: [ 'outcome' ]`, and
 * some are other things we haven't investigated deeply.
 */
export function biolinkifyClass(s) {
  const str = s.split(' ').map((e) => cmn.capitalizeFirstLetter(e)).join('');
  return `biolink:${str}`;
}

export function classRank(target, classes) {
  if (!classes.hasOwnProperty(target)) {
    throw new Error(`${target} is not a Biolink class`);
  }
  let obj = classes[target];
  if (obj.is_a !== null) {
    return 1 + classRank(obj.is_a, classes);
  } else {
    return 0;
  }
}

function makeBlClass(classData) {
  let retval = {};
  retval.is_a = cmn.jsonGet(classData, 'is_a', null);
  if (retval.is_a) {
    retval.is_a = biolinkifyClass(retval.is_a);
  }
  retval.description = cmn.jsonGet(classData, 'description', null);
  return retval;
}

function makeBlClasses(classes) {
  let retval = {};
  Object.entries(classes).forEach((e) => {
    /* Unlike predicates, it's easier to convert the file data into biolink format
     * than to safely split the biolink values into space-separated words :-/ */
    let name = biolinkifyClass(e[0]);
    let value = e[1];
    let data = makeBlClass(value);
    retval[name] = data;
  });
  // Assign ranks to each node
  Object.entries(retval).forEach((e) => {
    let name = e[0];
    let value = e[1];
    value.rank = classRank(name, retval);
  });
  return retval;
}

// Inputs are expected to be in biolink form: `biolink:RNAProductIsoform`
export function isBiolinkClassMoreSpecific(class1, class2) {
  let d1 = cmn.jsonGet(BIOLINK_CLASSES, class1, false);
  let d2 = cmn.jsonGet(BIOLINK_CLASSES, class2, false);

  if (!d1) {
    throw InvalidClassError(class1);
  } else if (!d2) {
    throw InvalidClassError(class2);
  } else {
    return d1.rank > d2.rank;
  }
}
