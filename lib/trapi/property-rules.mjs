export {
  make_rule_transform_property,
  make_rule_get_property,
  make_rule_transform_and_aggregate_property,
  make_rule_aggregate_property,
  make_rule_transform_attribute_value,
  make_rule_collect_attribute_values,
  make_rule_map_attributes
}

import * as cmn from '../common.mjs';
import * as trapi from './core.mjs';

function make_rule_transform_property({source_key, transform}) {
  const default_value = null;
  return _make_property_rule({
    key: source_key,
    transform: transform,
    update: (target, property) => {
      const current = cmn.jsonSetDefaultAndGet(target, source_key, default_value);
      if (cmn.is_missing(current) && !cmn.is_missing(property)) {
        cmn.jsonSet(target, source_key, property);
      }
      return target;
    },
    default_value: default_value
  });
}

/* Constructs a rule for extracting a property from a Graph Element and placing it in the accumulator using the same key.
 *
 * @param {string} key - The key to extract from a Graph Element and place into the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_get_property(source_key) {
  return make_rule_transform_property({
    source_key: source_key,
    transform: (source, key) => cmn.jsonGet(source, key)
  });
}

/* Constructs a rule for extracting a property from a Graph Element, transforming it, and aggregating it in the accumulator using the same key.
 *
 * @param {string} key - The key to extract from a Graph Element and aggregate in the accumulator.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_transform_and_aggregate_property({source_key, target_key, transform}) {
  return _make_property_rule({
    key: source_key,
    transform: transform,
    update: (target, property) => {
      const current_value = cmn.jsonSetDefaultAndGet(target, target_key, []);
      if (!cmn.is_missing(property)) {
        current_value.push(...cmn.coerce_array(property));
      }
      return target;
    },
    default_value: []
  });
}

/* Constructs a rule for extracting a property from a Graph Element and aggregating it in the accumulator.
 *
 * @param {string} key - The key to extract from a Graph Element and aggregate in the accumulator.
 * @param {string[]} kpath - The path to the property in the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_aggregate_property({source_key, target_key}) {
  return make_rule_transform_and_aggregate_property({
    source_key: source_key,
    target_key: target_key,
    transform: (obj) => { return cmn.jsonGet(obj, source_key); }
  });
}

/* Constructs a rule for renaming and transforming an attribute from a Graph Element and placing it into an accumulator.
 *
 * @param {string} attributeId - The Type ID of the attribute to rename and transform.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_transform_attribute_value({attr_id, target_key, transform, update, default_value}) {
  if (cmn.is_missing(default_value)) {
    default_value = null;
  }
  if (cmn.is_missing(update)) {
    update = (target, attr) => {
      const current = cmn.jsonSetDefaultAndGet(target, target_key, default_value);
      if (cmn.is_missing(current) && !cmn.is_missing(attr)) {
        cmn.jsonSet(target, target_key, attr);
      }
      return target;
    };
  }
  return _make_property_rule({
    key: attr_id,
    transform: (source, attr_id) => {
      const attr_itr = new trapi.AttributeIterator(source);
      const attr = attr_itr.find_one(attr_id);
      if (cmn.is_missing(attr)) return default_value;
      const value = trapi.get_attr_val(attr);
      if (cmn.is_missing(value)) return default_value;
      return transform(value);
    },
    update: update,
    default_value: default_value
  });
}

/* Constructs a rule for aggregating and transforming attributes from a Graph Element and placing them into an accumulator.
 *
 * @param {string[]} attributeIds - The Type IDs of the attributes to aggregate and transform.
 * @param {string[]} kpath - The path to the property in the accumulator.
 * @param {function} transform - The transformation to apply to the extracted value.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_map_attributes({attr_ids, target_key, transform, update}) {
  if (cmn.is_missing(update)) {
    update = (target, attrs) => {
      const current = cmn.jsonSetDefaultAndGet(target, target_key, []);
      current.push(...attrs);
      return target;
    }
  }
  return _make_property_rule({
    key: attr_ids,
    transform: (source, attr_ids, context) => {
      const attr_itr = new trapi.AttributeIterator(source);
      const attrs = attr_itr.find_all(attr_ids);
      return attrs.map(attr => transform(attr, context)).flat();
    },
    update: update,
    default_value: []
  });
}

/* Constructs a rule for aggregating an attribute from a Graph Element and placing it into an accumulator.
 *
 * @param {string[]} attributeIds - The Type IDs of the attributes to aggregate.
 * @param {string} accKey - The key to aggregate the attributes under in the accumulator.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_collect_attribute_values(kwargs) {
  return make_rule_map_attributes({
    ...kwargs,
    transform: attr => trapi.get_attr_val(attr)
  });
}

/* Constructs a rule on how to extract a property from a TRAPI object. There are 3 different stages to an extraction rule:
 * 1. Definition: This is what this function does.
 * 2. Application: The rule can by applied to source object with some context which will produce a transformer function.
 * 3. Transformation: Once the target object is known, the actual transformation can be applied to modify the target object.
 *
 * @param {string} key - The key to extract from an object.
 * @param {function} transform - The transformation to apply to the extracted value.
 * @param {function} update - How to update the accumulator with the extracted value.
 * @param {object} default_value - The default value to use if the extraction fails.
 *
 * @returns {function} - The extraction rule.
 */
function _make_property_rule({key, transform, update, default_value}) {
  return (source, context) => {
    return (target) => {
      try {
        const value = transform(source, key, context);
        return update(target, value);
      } catch (error) {
        if (context !== undefined) {
          const agent_errors = cmn.jsonSetDefaultAndGet(context.errors, context.agent, []);
          agent_errors.push(error.message);
        }
        return update(target, default_value);
      }
    }
  }
}
