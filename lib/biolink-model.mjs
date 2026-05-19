'use strict'

import { logger } from './logger.mjs';
import * as cmn from './common.mjs';

let BIOLINK_PREDICATES = null;
let BIOLINK_CLASSES = null;
let INFORES_CATALOG = null;
let DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = null;
let PREFIX_CATALOG = null;
let PREFIX_EXCLUDE_LIST = null;

export async function load_biolink(biolink_config) {
  const biolink_version = biolink_config.version;
  const support_deprecated_preds = biolink_config.support_deprecated_predicates;
  const infores_catalog = biolink_config.infores_catalog;
  const prefix_catalog = biolink_config.prefix_catalog;
  const biolink_model = await cmn.read_json(`./assets/biolink-model/${biolink_version}/biolink-model.json`);
  const slots = cmn.json_get(biolink_model, 'slots');
  const classes = cmn.json_get(biolink_model, 'classes');
  BIOLINK_PREDICATES = Object.freeze(_make_bl_preds(slots));
  BIOLINK_CLASSES = Object.freeze(_make_bl_classes(classes));
  INFORES_CATALOG = Object.freeze(await cmn.read_json(`./assets/biolink-model/common/${infores_catalog}`));
  PREFIX_CATALOG = Object.freeze(await cmn.read_json(`./assets/biolink-model/common/${prefix_catalog.path}`));
  PREFIX_EXCLUDE_LIST = Object.freeze(prefix_catalog.exclude);
  if (support_deprecated_preds) {
    DEPRECATED_TO_QUALIFIED_PREDICATE_MAP = Object.freeze(await cmn.read_json(`./assets/biolink-model/${biolink_version}/deprecated-predicate-mapping.json`));
  }
}

export function tag_biolink(str) {
  return _biolinkify_pred(str);
}

export function is_deprecated_pred(s) {
  return !!DEPRECATED_TO_QUALIFIED_PREDICATE_MAP &&
         DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[sanitize_biolink_item(s)] !== undefined;
}

export function is_biolink_pred(s) {
  const sanitized_pred = sanitize_biolink_item(s);
  return BIOLINK_PREDICATES[sanitized_pred] !== undefined ||
         is_deprecated_pred(sanitized_pred);
}

export function get_predicate_description(predicate) {
  const sanitized_predicate = sanitize_biolink_item(predicate);
  const predicate_record = BIOLINK_PREDICATES[sanitized_predicate];
  return predicate_record.description || null;
}

export function get_node_type_description(node_type) {
  const node_class = BIOLINK_CLASSES[node_type];
  if (cmn.is_missing(node_class)) {
    return null;
  }
  return node_class.description;
}

export function sanitize_biolink_item(pred) {
  return pred.replaceAll('_', ' ').replaceAll('biolink:', '');
}

export function invert_biolink_pred(pred, biolinkify = false) {
  const p = sanitize_biolink_item(pred);
  const biolink_pred = cmn.json_get(BIOLINK_PREDICATES, p, false);
  if (biolink_pred) {
    if (biolinkify) {
      return _biolinkify_pred(biolink_pred.inverse);
    }
    return biolink_pred.inverse;
  }
  throw InvalidPredicateError(p);
}

export function deprecated_pred_to_pred_and_qualifiers(pred) {
  const qualified_pred = DEPRECATED_TO_QUALIFIED_PREDICATE_MAP[pred];
  return [qualified_pred.predicate, qualified_pred];
}

export function infores_to_provenance(infores) {
  return INFORES_CATALOG[infores] ?? null;
}

export function source_to_provenance(source) {
  const provenance = infores_to_provenance(source.id);
  if (cmn.is_missing(provenance)) return false;
  provenance.infores = source.id;
  if (!cmn.is_array_empty(source.records)) {
    provenance.url = source.records[0];
  }
  return provenance;
}

export function split_curie(curie) {
  return curie.split(':');
}

export function get_curie_prefix(curie) {
  return split_curie(curie)[0];
}

export function is_valid_curie(curie) {
  if (typeof(curie) !== 'string') return false;
  const curie_parts = split_curie(curie);
  const prefix = get_curie_prefix(curie);
  return curie_parts.length > 1 && !PREFIX_EXCLUDE_LIST.includes(prefix) && PREFIX_CATALOG[prefix] !== undefined;
}

export function curie_to_normalized_url(curie, curies) {
  function curie_greater_than(curie1, curie2) {
    function get_prefix_rank(curie) {
      if (!is_valid_curie(curie)) return -1;
      const ranking = ['UMLS', 'MESH', 'EFO'];
      const prefix = get_curie_prefix(curie);
      const prefix_ranking = ranking.indexOf(prefix);
      if (prefix_ranking === -1) return ranking.length;
      return prefix_ranking;
    }

    const prefix1_ranking = get_prefix_rank(curie1);
    const prefix2_ranking = get_prefix_rank(curie2);
    if (prefix1_ranking === prefix2_ranking) return 0;
    if (prefix1_ranking > prefix2_ranking) return -1;
    return 1;
  }

  if (!curie.startsWith('UMLS')) {
    return curie_to_url(curie);
  }

  curies.sort(curie_greater_than);
  return curie_to_url(curies[0]);
}

export function curie_to_url(curie) {
  const [curie_prefix, curie_id] = curie.split(':');
  const url = PREFIX_CATALOG[curie_prefix];
  if (!url) {
    return false;
  }

  return `${url}${curie_id}`;
}

export function pred_to_url(pred) {
  const p = pred.replaceAll(' ', '_');
  return `https://biolink.github.io/biolink-model/${p}/`
}

function InvalidPredicateError(pred) {
  const error = new Error(`Expected a valid biolink predicate. Got: ${pred}`, 'biolink-model.mjs');
}
InvalidPredicateError.prototype = Object.create(Error.prototype);

function InvalidClassError(bl_class) {
  const error = new Error(`Expected a valid biolink class. Got: ${bl_class}`, 'biolink-model.mjs');
}
InvalidClassError.prototype = Object.create(Error.prototype);

function _get_parent(pred, record) {
  if (pred === 'related to') {
    return false;
  }

  return cmn.json_get(record, 'is_a', false);
}

function _is_canonical(pred, record) {
  return !!cmn.json_get_from_kpath(record, ['annotations', 'canonical_predicate'], false);
}

function _get_symmetric(pred, record) {
  return cmn.json_get(record, 'symmetric', false);
}

function _is_symmetric(pred, record) {
  return !!_get_symmetric(pred, record);
}

function _is_deprecated(pred, record) {
  return cmn.json_get(record, 'deprecated', false);
}

function _get_inverse(pred, record) {
  if (_is_symmetric(pred, record)) {
    return pred;
  }

  return cmn.json_get(record, 'inverse', false);
}

function _get_description(pred, record) {
  return cmn.json_get(record, 'description', false);
}

function _distance_from_related_to(slots, pred) {
  for (let level = 0; ;level += 1) {
    if (pred === 'related to') {
      return level;
    }

    let pred_obj = cmn.json_get(slots, pred, false);
    if (!pred_obj) {
      return -1;
    }

    pred = cmn.json_get(pred_obj, 'is_a', false);
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

function _make_bl_preds(slots) {
  let bl_preds = {};
  Object.keys(slots).forEach((pred) => {
      const rank = _distance_from_related_to(slots, pred);
      if (rank >= 0) {
        const record = slots[pred];
        bl_preds[pred] = _make_biolink_pred(pred, record, rank);
      }
    });

  Object.keys(bl_preds).forEach((pred) => {
      const record = bl_preds[pred];
      const inverse_pred = record.inverse;
      if (inverse_pred) {
        const inverse_record = cmn.json_get(bl_preds, inverse_pred);
        inverse_record.inverse = pred;
        bl_preds[inverse_pred] = inverse_record;
        if (!record.description) {
          record.description = inverse_record.description;
        }
      }
    });

  return bl_preds;
}

function _biolinkify_pred(pred) {
  let s = pred.replaceAll(' ', '_');
  if (s.startsWith('biolink:')) {
    return s;
  }

  return `biolink:${s}`;
}

function _get_biolink_pred_data(pred) {
  return cmn.json_get(BIOLINK_PREDICATES, sanitize_biolink_item(pred), false);
}

function _is_biolink_pred_more_specific(pred1, pred2) {
  const p1 = sanitize_biolink_item(pred1);
  const p2 = sanitize_biolink_item(pred2);
  const biolink_pred1 = _get_biolink_pred_data(p1);
  const biolink_pred2 = _get_biolink_pred_data(p2);

  if (!biolink_pred1) {
    throw InvalidPredicateError(p1);
  }
  else if (!biolink_pred2) {
    throw InvalidPredicateError(p2);
  }
  else {
    return pred1.rank > pred2.rank;
  }
}

function _sort_biolink_preds(preds) {
  return sort(pres, _is_biolink_pred_more_specific);
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
export function biolinkify_class(s) {
  const str = s.split(' ').map((e) => cmn.capitalize_first_letter(e)).join('');
  return `biolink:${str}`;
}

export function class_rank(target, classes) {
  if (!classes.hasOwnProperty(target)) {
    throw new Error(`${target} is not a Biolink class`);
  }
  let obj = classes[target];
  if (obj.is_a !== null) {
    return 1 + class_rank(obj.is_a, classes);
  } else {
    return 0;
  }
}

function _make_bl_class(class_data) {
  let retval = {};
  retval.is_a = cmn.json_get(class_data, 'is_a', null);
  if (retval.is_a) {
    retval.is_a = biolinkify_class(retval.is_a);
  }
  retval.description = cmn.json_get(class_data, 'description', null);
  return retval;
}

function _make_bl_classes(classes) {
  let retval = {};
  Object.entries(classes).forEach((e) => {
    /* Unlike predicates, it's easier to convert the file data into biolink format
     * than to safely split the biolink values into space-separated words :-/ */
    let name = biolinkify_class(e[0]);
    let value = e[1];
    let data = _make_bl_class(value);
    retval[name] = data;
  });
  // Assign ranks to each node
  Object.entries(retval).forEach((e) => {
    let name = e[0];
    let value = e[1];
    value.rank = class_rank(name, retval);
  });
  return retval;
}

export function biolink_class_cmp_fn(class_a, class_b) {
  let d1 = cmn.json_get(BIOLINK_CLASSES, class_a, false);
  let d2 = cmn.json_get(BIOLINK_CLASSES, class_b, false);
  //logger.info(`d1: ${d1.rank}; d2: ${d2.rank}`);
  if (!d1) {
    logger.error(`Expected a valid biolink class. Got: ${class_a}`);
    d1 = { rank: 0 };
  } else if (!d2) {
    logger.error(`Expected a valid biolink class. Got: ${class_b}`);
    d2 = { rank: 0 };
  }

  // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/sort#description
  return d2.rank - d1.rank;
}

export class MissingInforesError extends Error {
  constructor(infores) {
    super(`Missing infores: ${infores}`);
    this.infores = infores;
  }
}
