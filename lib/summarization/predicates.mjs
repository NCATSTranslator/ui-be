export {
  gen_qualified_predicate,
  get_most_specific_predicate,
  is_predicate_inverted
}

import * as cmn from "../common.mjs";
import * as trapi from "../trapi/core.mjs";
import * as bl from "../biolink-model.mjs";

/**
 * Generates the qualified predicate for a kedge.
 *
 * @param {object} kedge - The knowledge edge to generate the qualified predicate for.
 * @param {boolean} invert - Whether or not to invert the predicate.
 *
 * @returns {string} - The qualified predicate.
 */
function gen_qualified_predicate(kedge, invert = false) {
  let predicate = bl.sanitizeBiolinkItem(trapi.get_predicate(kedge));
  let qualifiers = _get_qualifiers(kedge);
  if (!qualifiers && bl.isDeprecatedPred(predicate)) {
    [predicate, qualifiers] = bl.deprecatedPredToPredAndQualifiers(predicate);
  }
  // If we don"t have any qualifiers, treat it like biolink v2 and just return
  // the base predicate
  if (!qualifiers) {
    if (invert) return bl.invertBiolinkPred(predicate);
    return predicate;
  }
  predicate = bl.sanitizeBiolinkItem(get_most_specific_predicate(kedge));
  const special_case = _gen_special_predicate(predicate, qualifiers, invert);
  if (special_case) {
    return special_case;
  }
  const subject_prefixes = ["of a", "has", false, "of the", false];
  const object_prefixes = ["a", false, false, "of the", false];
  if (invert) {
    const subject_qualification = _gen_subject_qualification(qualifiers, object_prefixes);
    const object_qualification = _gen_object_qualification(qualifiers, subject_prefixes);
    return _combine_predicate_parts(object_qualification, bl.invertBiolinkPred(predicate), subject_qualification);
  }

  const subject_qualification = _gen_subject_qualification(qualifiers, subject_prefixes);
  const object_qualification = _gen_object_qualification(qualifiers, object_prefixes);
  return _combine_predicate_parts(subject_qualification, predicate, object_qualification);

  function _gen_qualification(type, qualifiers, prefixes) {
    // TODO: How do part and derivative qualifiers interact? Correct ordering?
    // TODO: How to handle the context qualifier?
    // TODO: Make more robust to biolink qualifier changes.
    // TODO: Add biolink constants for qualifiers
    // The ordering of qualifierKeys impacts the final qualified predicate
    const qualifier_keys = ["form or variant", "direction", "aspect", "part", "derivative"];
    const qualifier_values = qualifier_keys
      .map(key => cmn.jsonGet(qualifiers, `${type} ${key} qualifier`, false))

    let qualification = "";
    qualifier_values.forEach((qv, i) => {
      if (qv) {
        if (qualification) {
          qualification += " "
        }
        if (prefixes[i]) {
          qualification += `${prefixes[i]} `;
        }
        qualification += qv;
      }
    });

    return qualification;
  }

  function _gen_subject_qualification(qualifiers, direction_prefix = false) {
    return _gen_qualification("subject", qualifiers, direction_prefix);
  }

  function _gen_object_qualification(qualifiers, direction_prefix = false) {
    return _gen_qualification("object", qualifiers, direction_prefix);
  }

  function _combine_predicate_parts(prefix, predicate, suffix) {
    if (prefix) {
      prefix += " ";
    }
    if (suffix) {
      suffix = ` ${suffix} of`;
    }
    const qualified_predicate = `${prefix}${predicate}${suffix}`;
    return qualified_predicate;
  }

  function _gen_special_predicate(predicate, qualifiers, invert) {
    const object_direction_qualifier = qualifiers["object direction qualifier"];
    if (predicate === "regulates"
        && (object_direction_qualifier === "upregulated"
            || object_direction_qualifier === "downregulated")) {
      if (invert) return `is ${object_direction_qualifier} by`;
      return object_direction_qualifier.replace("ed", "es");
    }
    return false;
  }
}

/**
 * Get the most specific predicate available from a kedge.
 *
 * @param {object} kedge - The knowledge edge to extract the predicate from.
 *
 * @returns {string} - The most specific predicate available.
 */
// TODO: Add biolink constants for qualifier keys
function get_most_specific_predicate(kedge) {
  const qualifiers = _get_qualifiers(kedge);
  if (!qualifiers) return trapi.get_predicate(kedge);
  return qualifiers["qualified predicate"] || trapi.get_predicate(kedge);
}

function is_predicate_inverted(redge, subject, kgraph) {
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
    if (cmn.is_array_empty(trapi_qualifiers)) return null;
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
