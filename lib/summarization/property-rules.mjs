export {
  make_rule_group_publications_by_knowledge_level,
  make_rule_collect_publication_supporting_text
}

import * as cmn from '../common.mjs';
import * as trapi from '../trapi/core.mjs';
import * as trapi_rules from '../trapi/property-rules.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';

function make_rule_group_publications_by_knowledge_level({attr_ids}) {
  return trapi_rules.make_rule_map_attributes({
    attr_ids: attr_ids.map(bl.tagBiolink),
    transform: (pid, edge_context) => {
      return {
        id: ev.sanitize(pid),
        src: edge_context.provenance,
        knowledge_level: edge_context.knowledge_level
      }
    },
    update: (edge, publications) => {
      const current = cmn.jsonSetDefaultAndGet(edge, _CONSTANTS.PUBLICATIONS, {});
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
  return trapi.make_rule_map_attributes({
    attr_ids: ['has_supporting_study_result'],
    transform: __process_supporting_study_result,
    update: (target, supporting_text_entries) => {
      const current = cmn.jsonSetDefaultAndGet(target, _CONSTANTS.SUPPORTING_TEXT, {});
      for (const supporting_text_entry of supporting_text_entries) {
        current[supporting_text_entry.id] = {
          text: supporting_text_entry.text,
          subject: supporting_text_entry.subject,
          object: supporting_text_entry.object
        }
      }
      return target;
    }
  });

  function __process_supporting_study_result(supporting_study_result) {
    //TODO: Make these biolink
    const publication_id = bl.tagBiolink('publications');
    const text_id = bl.tagBiolink('supporting_text');
    const subject_location_id = bl.tagBiolink('subject_location_in_text');
    const object_location_id = bl.tagBiolink('object_location_in_text');
    //END TODO
    const supporting_text_entry = {};
    const attr_itr = new trapi.AttributeIterator(supporting_study_result);
    while (attr_itr.has_next()) {
      const attr = attr_itr.next();
      const attr_id = trapi.get_attr_id(attr);
      const attr_value = trapi.get_attr_val(attr);
      switch (attr_id) {
        case publication_attr_id:
          supporting_text_entry.id = attr_value; break;
        case text_attr_id:
          supporting_text_entry.text = attr_value; break;
        case subject_location_id:
          supporting_text_entry.subject = __parse_token_index(attr_value); break;
        case object_location_id:
          supporting_text_entry.object = __parse_token_index(attr_value); break;
      }
    }
    return supporting_text_entry;
  }

  function __parse_token_index(token) {
    const range = token.split('|').map(t => parseInt(t.trim()));
    range[0] = range[0] + 1;
    return range;
  }
}

const _CONSTANTS = Object.freeze({
  PUBLICATIONS: 'publications',
  SUPPORTING_TEXT: 'supporting_text',
});
