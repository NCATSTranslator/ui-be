export {
  make_rule_group_publications_by_knowledge_level,
  make_rule_collect_publication_supporting_text,
  make_rule_collect_semmed_sentences,
  make_rule_collect_clinical_trial_metadata,
  make_rule_tag_attribute
}

import {logger} from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as trapi from '../trapi/core.mjs';
import * as trapi_rules from '../trapi/property-rules.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';
import * as taglib from '../taglib.mjs';

function make_rule_group_publications_by_knowledge_level() {
  const attr_ids = ['supporting_document', 'Publication', 'publications', 'publication'];
  return trapi_rules.make_rule_map_attributes({
    attr_ids: attr_ids.map(bl.tagBiolink),
    transform: (attr, edge_context) => {
      const pids = trapi.get_attr_val(attr);
      return pids.map(pid => {
        return {
          id: ev.sanitize(pid),
          src: edge_context.provenance,
          knowledge_level: edge_context.knowledge_level
        }
      });
    },
    update: (edge, publications) => {
      const current = cmn.jsonSetDefaultAndGet(edge, trapi.CONSTANTS.ATTRIBUTES.ID.PUBLICATIONS, {});
      for (const publication of publications) {
        const group = cmn.jsonSetDefaultAndGet(current, publication.knowledge_level, []);
        group.push({id: publication.id, src: publication.src});
      }
      return edge;
    }
  });
}

/* A special rule for extracting supporting text for a publication.
 *
 * @returns {function} - The extraction rule.
 */
function make_rule_collect_publication_supporting_text() {
  return trapi_rules.make_rule_map_attributes({
    attr_ids: [bl.tagBiolink('has_supporting_study_result')],
    transform: __process_supporting_study_result,
    update: _merge_records_list(trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT)
  });

  function __process_supporting_study_result(supporting_study_result) {
    //TODO: Make these biolink
    const publication_id = bl.tagBiolink('publications');
    const text_id = bl.tagBiolink('supporting_text');
    const subject_location_id = bl.tagBiolink('subject_location_in_text');
    const object_location_id = bl.tagBiolink('object_location_in_text');
    //END TODO
    const supporting_text_records = {};
    const text_record = _make_text_record();
    const attr_itr = new trapi.AttributeIterator(supporting_study_result);
    while (attr_itr.has_next()) {
      const attr = attr_itr.next();
      const attr_id = trapi.get_attr_id(attr);
      const attr_value = trapi.get_attr_val(attr);
      switch (attr_id) {
        case publication_id:
          supporting_text_records[attr_value] = text_record;
          break;
        case text_id:
          text_record.text = attr_value;
          break;
        case subject_location_id:
          text_record.subject = __parse_token_index(attr_value);
          break;
        case object_location_id:
          text_record.object = __parse_token_index(attr_value);
          break;
      }
    }
    return supporting_text_records;
  }

  function __parse_token_index(token) {
    const range = token.split('|').map(t => parseInt(t.trim()));
    range[0] = range[0] + 1;
    return range;
  }
}

function make_rule_collect_semmed_sentences() {
  return trapi_rules.make_rule_map_attributes({
    attr_ids: ['bts:sentence', bl.tagBiolink('supporting_text')],
    transform: (attr) => {
      const semmed_records = trapi.get_attr_val(attr);
      if (typeof semmed_records !== 'object') {
        logger.warn(`Unhandled attribute being processed: ${attr}`);
        return {};
      }
      const text_records = {};
      for (const [pid, record] of Object.entries(semmed_records)) {
        text_records[pid] = _make_text_record(cmn.jsonGet(record, 'sentence'));
      }
      return text_records;
    },
    update: _merge_records_list(trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT)
  });
}

function make_rule_collect_clinical_trial_metadata() {
  return trapi_rules.make_rule_map_attributes({
    attr_ids: [bl.tagBiolink('has_supporting_studies')],
    transform: __process_supporting_studies,
    update: _merge_records_list(trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS)
  });

  function __process_supporting_studies(supporting_studies) {
    const trials = trapi.get_attr_val(supporting_studies);
    const trial_ids = Object.keys(trials);
    const trial_records = {};
    for (let i = 0; i < trial_ids.length; i++) {
      const trial_id = trial_ids[i];
      if (!ev.is_clinical_trial(trial_id)) continue;
      const raw_trial_id = trial_id.split(":")[1];
      const trial = trials[trial_id];
      trial_records[raw_trial_id] = {
        phase: __process_clinical_trial_phase(trial),
        status: trial["clinical_trial_overall_status"],
        child: trial["clinical_trial_age_stage"].includes("child"),
        start_date: trial["clinical_trial_start_date"],
        size: trial["clinical_trial_enrollment"],
        type: trial["clinical_trial_enrollment_type"] === "ACTUAL" ? "enrolled" : "anticipated",
        title: trial["name"]
      }
    }
    return trial_records;
  }

  function __process_clinical_trial_phase(trial) {
    const phase_string = trial["clinical_trial_phase"];
    if (cmn.is_missing(phase_string)) return null;
    if (phase_string.endsWith("1_to_2")) return 1.5;
    return parseInt(phase_string[phase_string.length-1]);
  }
}

function make_rule_tag_attribute({attr_id, gen_tag}) {
  return trapi_rules.make_rule_transform_attribute_value({
    attr_id: attr_id,
    transform: gen_tag,
    update: (obj, tags) => {
      if (cmn.is_missing(tags)) return obj;
      if (!cmn.is_array(tags)) {
        taglib.set_tag(obj, tags);
        return obj;
      }
      for (const tag of tags) {
        taglib.set_tag(obj, tag);
      }
      return obj;
    }
  });
}

function _make_text_record(text = null) {
  return {
    text: text,
    subject: null,
    object: null
  }
}

function _merge_records_list(target_key) {
  return (obj, records_list) => {
    const current_records = cmn.jsonSetDefaultAndGet(obj, target_key, {});
    for (const records of records_list) {
      for (const [record_id, record] of Object.entries(records)) {
        if (current_records[record_id] !== undefined) {
          logger.warn(`Unexpectedly found multiple records with the same ID: ${record_id}\n  Existing: ${JSON.stringify(current_records[record_id])}\n  New Record: ${JSON.stringify(record)}`);
        } else {
          current_records[record_id] = records[record_id];
        }
      }
    }
    return obj;
  }
}
