export {
  genQualifiedPred,
  getMostSpecificPred,
  isPredInverted
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi.mjs';
import * as bl from '../biolink-model.mjs';

/**
 * Generates the qualified predicate for a kedge.
 *
 * @param {object} kedge - The knowledge edge to generate the qualified predicate for.
 * @param {boolean} invert - Whether or not to invert the predicate.
 *
 * @returns {string} - The qualified predicate.
 */
function genQualifiedPred(kedge, invert = false) {
  function genQualification(type, qualifiers, prefixes) {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // TODO: Add biolink constants for qualifiers
    // The ordering of qualifierKeys impacts the final qualified predicate
    const qualifierKeys = ['form or variant', 'direction', 'aspect', 'part', 'derivative'];
    const qualifierValues = qualifierKeys
      .map(key => cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false))

    let qualification = '';
    qualifierValues.forEach((qv, i) => {
      if (qv) {
        if (qualification) {
          qualification += ' '
        }

        if (prefixes[i]) {
          qualification += `${prefixes[i]} `;
        }

        qualification += qv;
      }
    });

    return qualification;
  }

  function genSubQualification(qualifiers, directionPrefix = false) {
    return genQualification('subject', qualifiers, directionPrefix);
  }

  function genObjQualification(qualifiers, directionPrefix = false) {
    return genQualification('object', qualifiers, directionPrefix);
  }

  function combinePredParts(prefix, pred, suffix) {
    if (prefix) {
      prefix += ' ';
    }

    if (suffix) {
      suffix = ` ${suffix} of`;
    }

    const qualifiedPred = `${prefix}${pred}${suffix}`;
    return qualifiedPred;
  }

  function genSpecialPred(pred, qualifiers, invert) {
    const objDirectionQualifier = qualifiers['object direction qualifier'];
    if (pred === 'regulates' &&
          (objDirectionQualifier === 'upregulated' ||
           objDirectionQualifier === 'downregulated')) {
      if (invert) {
        return `is ${objDirectionQualifier} by`;
      }

      return objDirectionQualifier.replace('ed', 'es');
    }

    return false;
  }

  let pred = bl.sanitizeBiolinkItem(trapi.get_predicate(kedge));
  let qualifiers = _get_qualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPred(pred)) {
    [pred, qualifiers] = bl.deprecatedPredToPredAndQualifiers(pred);
  }

  // If we don't have any qualifiers, treat it like biolink v2
  if (!qualifiers) {
    if (invert) {
      pred = bl.invertBiolinkPred(pred);
    }

    return pred;
  }

  pred = bl.sanitizeBiolinkItem(getMostSpecificPred(kedge));
  const specialCase = genSpecialPred(pred, qualifiers, invert);
  if (specialCase) {
    return specialCase;
  }

  const subPrefixes = ['of a', 'has', false, 'of the', false];
  const objPrefixes = ['a', false, false, 'of the', false];
  if (invert) {
    const subQualification = genSubQualification(qualifiers, objPrefixes);
    const objQualification = genObjQualification(qualifiers, subPrefixes);
    return combinePredParts(objQualification, bl.invertBiolinkPred(pred), subQualification);
  }

  const subQualification = genSubQualification(qualifiers, subPrefixes);
  const objQualification = genObjQualification(qualifiers, objPrefixes);
  return combinePredParts(subQualification, pred, objQualification);
}

/**
 * Get the most specific predicate available from a kedge.
 *
 * @param {object} kedge - The knowledge edge to extract the predicate from.
 *
 * @returns {string} - The most specific predicate available.
 */
// TODO: Add biolink constants for qualifier keys
function getMostSpecificPred(kedge) {
  const qualifiers = _get_qualifiers(kedge);
  if (!qualifiers) {
    return trapi.get_predicate(kedge);
  }

  return qualifiers['qualified predicate'] || trapi.get_predicate(kedge);
}

function isPredInverted(redge, subject, kgraph) {
  const kedge = trapi.get_kedge(redge, kgraph);
  return subject === trapi.get_object(kedge);
}

/**
 * Convert the array of qualifiers of a knowledge edge into an object.
 *
 * @param {object} kedge - The knowledge edge to extract qualifiers from.
 *
 * @returns {object} - The qualifiers of the knowledge edge or null if there is an error.
 */
function _get_qualifiers(kedge) {
  try {
    const trapi_qualifiers = trapi.get_qualifiers(kedge);
    const qualifiers = {};
    trapi_qualifiers.forEach((qualifier) => {
      const qualifier_id = bl.sanitizeBiolinkItem(trapi.get_qualifier_id(qualifier));
      const qualifier_val = bl.sanitizeBiolinkItem(trapi.get_qualifier_val(qualifier));
      qualifiers[qualifier_id] = qualifier_val;
    });
    return qualifiers;
  } catch (err) {
    return null;
  }
}
