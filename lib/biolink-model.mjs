'use strict'

import { logger } from './logger.mjs';
import * as cmn from './common.mjs';

let BIOLINK_PREDICATES = null;
let BIOLINK_CLASSES = null;
let INFORES_CATALOG = null;
let DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = null;
let PREFIX_CATALOG = null;
let PREFIX_EXCLUDE_LIST = null;

export async function loadBiolink(biolinkConfig) {
  const biolinkVersion = biolinkConfig.version;
  const supportDeprecatedPreds = biolinkConfig.support_deprecated_predicates;
  const inforesCatalog = biolinkConfig.infores_catalog;
  const prefixCatalog = biolinkConfig.prefix_catalog;
  const biolinkModel = await cmn.readJson(`./assets/biolink-model/${biolinkVersion}/biolink-model.json`);
  const slots = cmn.jsonGet(biolinkModel, 'slots');
  const classes = cmn.jsonGet(biolinkModel, 'classes');
  BIOLINK_PREDICATES = Object.freeze(makeBlPreds(slots));
  BIOLINK_CLASSES = Object.freeze(makeBlClasses(classes));
  INFORES_CATALOG = Object.freeze(await cmn.readJson(`./assets/biolink-model/common/${inforesCatalog}`));
  PREFIX_CATALOG = Object.freeze(await cmn.readJson(`./assets/biolink-model/common/${prefixCatalog.path}`));
  PREFIX_EXCLUDE_LIST = Object.freeze(prefixCatalog.exclude);
  if (supportDeprecatedPreds) {
    DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = Object.freeze(await cmn.readJson(`./assets/biolink-model/${biolinkVersion}/deprecated-predicate-mapping.json`));
  }
}

export function tagBiolink(str) {
  return biolinkifyPred(str);
}

export function isDeprecatedPred(s) {
  return !!DEPRECATED_TO_QUALIFIED_PREDICATE_MAP &&
         DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[sanitizeBiolinkItem(s)] !== undefined;
}

export function isBiolinkPred(s) {
  const sanitizedPred = sanitizeBiolinkItem(s);
  return BIOLINK_PREDICATES[sanitizedPred] !== undefined ||
         isDeprecatedPred(sanitizedPred);
}

export function get_predicate_description(predicate) {
  const sanitized_predicate = sanitizeBiolinkItem(predicate);
  const predicate_record = BIOLINK_PREDICATES[sanitized_predicate];
  return predicate_record.description || null;
}

export function sanitizeBiolinkItem(pred) {
  return pred.replaceAll('_', ' ').replaceAll('biolink:', '');
}

export function invertBiolinkPred(pred, biolinkify = false) {
  const p = sanitizeBiolinkItem(pred);
  const biolinkPred= cmn.jsonGet(BIOLINK_PREDICATES, p, false);
  if (biolinkPred) {
    if (biolinkify) {
      return biolinkifyPred(biolinkPred.inverse);
    }
    return biolinkPred.inverse;
  }
  throw InvalidPredicateError(p);
}

export function deprecatedPredToPredAndQualifiers(pred) {
  const qualifiedPred = DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[pred];
  return [qualifiedPred.predicate, qualifiedPred];
}

export function inforesToProvenance(infores) {
  const provenance = INFORES_CATALOG[infores.id];
  if (!provenance) {
    return false;
  }
  provenance.infores = infores.id;
  if (!cmn.is_missing(infores.url)) {
    provenance.url = infores.url;
  }
  return provenance;
}

export function splitCurie(curie) {
  return curie.split(':');
}

export function getCuriePrefix(curie) {
  return splitCurie(curie)[0];
}

export function isValidCurie(curie) {
  if (typeof(curie) !== 'string') return false;
  const curieParts = splitCurie(curie);
  const prefix = getCuriePrefix(curie);
  return curieParts.length > 1 && !PREFIX_EXCLUDE_LIST.includes(prefix) && PREFIX_CATALOG[prefix] !== undefined;
}

export function curieToNormalizedUrl(curie, curies) {
  function curieGreaterThan(curie1, curie2) {
    function getPrefixRank(curie) {
      if (!isValidCurie(curie)) return -1;
      const ranking = ['UMLS', 'MESH', 'EFO'];
      const prefix = getCuriePrefix(curie);
      const prefixRanking = ranking.indexOf(prefix);
      if (prefixRanking === -1) return ranking.length;
      return prefixRanking;
    }

    const prefix1Ranking = getPrefixRank(curie1);
    const prefix2Ranking = getPrefixRank(curie2);
    if (prefix1Ranking === prefix2Ranking) return 0;
    if (prefix1Ranking > prefix2Ranking) return -1;
    return 1;
  }

  if (!curie.startsWith('UMLS')) {
    return curieToUrl(curie);
  }

  curies.sort(curieGreaterThan);
  return curieToUrl(curies[0]);
}

export function curieToUrl(curie) {
  const [curiePrefix, curieId] = curie.split(':');
  const url = PREFIX_CATALOG[curiePrefix];
  if (!url) {
    return false;
  }

  return `${url}${curieId}`;
}

export function predToUrl(pred) {
  const p = pred.replaceAll(' ', '_');
  return `https://biolink.github.io/biolink-model/${p}/`
}

function InvalidPredicateError(pred) {
  const error = new Error(`Expected a valid biolink predicate. Got: ${pred}`, 'biolink-model.mjs');
}
InvalidPredicateError.prototype = Object.create(Error.prototype);

function InvalidClassError(blClass) {
  const error = new Error(`Expected a valid biolink class. Got: ${blClass}`, 'biolink-model.mjs');
}
InvalidClassError.prototype = Object.create(Error.prototype);

function _get_parent(pred, record) {
  if (pred === 'related to') {
    return false;
  }

  return cmn.jsonGet(record, 'is_a', false);
}

function _is_canonical(pred, record) {
  return !!cmn.jsonGetFromKpath(record, ['annotations', 'canonical_predicate'], false);
}

function getSymmetric(pred, record) {
  return cmn.jsonGet(record, 'symmetric', false);
}

function _is_symmetric(pred, record) {
  return !!getSymmetric(pred, record);
}

function _is_deprecated(pred, record) {
  return cmn.jsonGet(record, 'deprecated', false);
}

function _get_inverse(pred, record) {
  if (_is_symmetric(pred, record)) {
    return pred;
  }

  return cmn.jsonGet(record, 'inverse', false);
}

function _get_description(pred, record) {
  return cmn.jsonGet(record, 'description', false);
}

function distanceFromRelatedTo(slots, pred) {
  for (let level = 0; ;level += 1) {
    if (pred === 'related to') {
      return level;
    }

    let predObj = cmn.jsonGet(slots, pred, false);
    if (!predObj) {
      return -1;
    }

    pred = cmn.jsonGet(predObj, 'is_a', false);
  }
}

function _make_biolink_pred(pred, record, rank) {
  return {
    'parent': _get_parent(pred, record),
    'isCanonical': _is_canonical(pred, record),
    'isSymmetric': _is_symmetric(pred, record),
    'isDeprecated': _is_deprecated(pred, record),
    'inverse': _get_inverse(pred, record),
    'description': _get_description(pred, record),
    'rank': rank,
  };
}

function makeBlPreds(slots) {
  let blPreds = {};
  Object.keys(slots).forEach((pred) => {
      const rank = distanceFromRelatedTo(slots, pred);
      if (rank >= 0) {
        const record = slots[pred];
        blPreds[pred] = _make_biolink_pred(pred, record, rank);
      }
    });

  Object.keys(blPreds).forEach((pred) => {
      const record = blPreds[pred];
      const inversePred = record.inverse;
      if (inversePred) {
        const inverseRecord = cmn.jsonGet(blPreds, inversePred);
        inverseRecord.inverse = pred;
        blPreds[inversePred] = inverseRecord;
        if (!record.description) {
          record.description = inverseRecord.description;
        }
      }
    });

  return blPreds;
}

function biolinkifyPred(pred) {
  let s = pred.replaceAll(' ', '_');
  if (s.startsWith('biolink:')) {
    return s;
  }

  return `biolink:${s}`;
}

function getBiolinkPredData(pred) {
  return cmn.jsonGet(BIOLINK_PREDICATES, sanitizeBiolinkItem(pred), false);
}

function isBiolinkPredMoreSpecific(pred1, pred2) {
  const p1 = sanitizeBiolinkItem(pred1);
  const p2 = sanitizeBiolinkItem(pred2);
  const biolinkPred1 = getBiolinkPredData(p1);
  const biolinkPred2 = getBiolinkPredData(p2);

  if (!biolinkPred1) {
    throw InvalidPredicateError(p1);
  }
  else if (!biolinkPred2) {
    throw InvalidPredicateError(p2);
  }
  else {
    return pred1.rank > pred2.rank;
  }
}

function sortBiolinkPreds(preds) {
  return sort(pres, isBiolinkPredMoreSpecific);
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

export function biolinkClassCmpFn(classA, classB) {
  let d1 = cmn.jsonGet(BIOLINK_CLASSES, classA, false);
  let d2 = cmn.jsonGet(BIOLINK_CLASSES, classB, false);
  //logger.info(`d1: ${d1.rank}; d2: ${d2.rank}`);
  if (!d1) {
    logger.error(`Expected a valid biolink class. Got: ${classA}`);
    d1 = { rank: 0 };
  } else if (!d2) {
    logger.error(`Expected a valid biolink class. Got: ${classB}`);
    d2 = { rank: 0 };
  }

  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort#description
  return d2.rank - d1.rank;
}
