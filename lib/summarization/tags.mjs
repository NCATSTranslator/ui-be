'use strict';

export {
  TAG_SCOPE,
  TAGS,
  is_result_tag,
  is_external_tag,
  is_fda_tag,
  make_predicate_tag,
  make_tdl_tag,
  make_node_type_tag,
  make_ara_tag,
  make_path_length_tag,
  promote_edge_tags_to_path,
  promote_start_node_tags_to_path,
  promote_internal_node_tags_to_path,
  decorate_clinical_tag,
  decorate_role_tags,
  decorate_otc_tag,
  decorate_indication_tag,
  decorate_max_phase_tag
}

import * as cmn from '#lib/common.mjs';
import * as trapi from '#lib/trapi/core.mjs';
import * as taglib from '#lib/taglib.mjs';

const TAG_SCOPE = Object.freeze({
  EDGE: 'e',
  NODE: 'n',
  PATH: 'p',
  RESULT: 'r'
});

const TAGS = Object.freeze({
  EDGE: {
    EVIDENCE: {
      CLINICAL: new taglib.Tag({id: `${TAG_SCOPE.EDGE}/ev/clinical`, name: 'Clinical Trials'}),
      PUBLICATIONS: new taglib.Tag({id: `${TAG_SCOPE.EDGE}/ev/publications`, name: 'Publications'}),
      ONTOLOGICAL: new taglib.Tag({id: `${TAG_SCOPE.EDGE}/ev/ontology`, name: 'Accepted Ontology'}),
      OTHER: new taglib.Tag({id: `${TAG_SCOPE.EDGE}/ev/other`, name: 'Other'})
    }
  },
  NODE: {
    FDA: {
      APPROVED: new taglib.Tag({id: `${TAG_SCOPE.NODE}/fda/4`, name: 'FDA Approved'}),
      NOT_APPROVED: new taglib.Tag({id: `${TAG_SCOPE.NODE}/fda/0`, name: 'Not FDA Approved'})
    },
    INDICATED: {
      YES: new taglib.Tag({id: `${TAG_SCOPE.NODE}/di/ind`, name: 'Has Been in Trial'}),
      NO: new taglib.Tag({id: `${TAG_SCOPE.NODE}/di/not`, name: 'Has Not Been in Trial'})
    },
    OTC: {
      OTC: new taglib.Tag({id: `${TAG_SCOPE.NODE}/otc/t`, name: 'Over the counter'}),
      PRESCRIPTION: new taglib.Tag({id: `${TAG_SCOPE.NODE}/otc/f`, name: 'Prescription only'}),
      DISCONTINUED: new taglib.Tag({id: `${TAG_SCOPE.NODE}/otc/d`, name: 'Discontinued'}),
      WITHDRAWN: new taglib.Tag({id: `${TAG_SCOPE.NODE}/otc/w`, name: 'Withdrawn'}),
      UNKNOWN: new taglib.Tag({id: `${TAG_SCOPE.NODE}/otc/o`, name: 'Other'})
    }
  },
  PATH: {
    TYPE: {
      DIRECT: new taglib.Tag({id: `${TAG_SCOPE.PATH}/pt/lkup`, name: 'Direct'}),
      INDIRECT: new taglib.Tag({id: `${TAG_SCOPE.PATH}/pt/inf`, name: 'Indirect'})
    }
  },
  RESULT: {
    CHEMICAL_CLASS: {
      DRUG: new taglib.Tag({id: `${TAG_SCOPE.RESULT}/cc/drug`, name: 'Drug'}),
      OTHER: new taglib.Tag({id: `${TAG_SCOPE.RESULT}/cc/other`, name: 'Other'})
    }
  }
});

function is_result_tag(tag) {
  return tag.id.startsWith(`${TAG_SCOPE.RESULT}/`);
}

function is_external_tag(tag) {
  return _EXTERNAL_FAMILIES.includes(_get_family(tag));
}

function is_fda_tag(tag) {
  return tag.id.startsWith(`${TAG_SCOPE.NODE}/fda`);
}

function make_predicate_tag(canonical_predicate) {
  return new taglib.Tag({
    id: `${TAG_SCOPE.EDGE}/pred/${canonical_predicate}`,
    name: canonical_predicate
  });
}

function make_tdl_tag(tdl) {
  return new taglib.Tag({id: `${TAG_SCOPE.NODE}/tdl/${tdl.toLowerCase()}`, name: tdl});
}

function make_node_type_tag(node_type) {
  return new taglib.Tag({id: `${TAG_SCOPE.PATH}/pc/${node_type}`, name: node_type});
}

function make_ara_tag(infores, name) {
  return new taglib.Tag({id: `${TAG_SCOPE.PATH}/ara/${infores}`, name: name});
}

function make_path_length_tag(edge_count) {
  const description = edge_count === 1 ? 'Connection' : 'Connections';
  return new taglib.Tag({
    id: `${TAG_SCOPE.PATH}/pt/${edge_count}`,
    name: `${edge_count} ${description}`
  });
}

function promote_edge_tags_to_path(edge, path) {
  for (const tag of taglib.get_tags(edge)) {
    taglib.set_tag(path, _promote(tag, TAG_SCOPE.PATH));
  }
  return path;
}

function promote_start_node_tags_to_path(node, path) {
  return _promote_external_tags(node, path, TAG_SCOPE.RESULT);
}

function promote_internal_node_tags_to_path(node, path) {
  return _promote_external_tags(node, path, TAG_SCOPE.PATH);
}

function decorate_clinical_tag(node) {
  const annotations = node.annotations.chemical;
  const fda_approval = annotations.approval;
  if (!cmn.is_missing(fda_approval)) {
    if (fda_approval === 4) {
      taglib.set_tag(node, TAGS.NODE.FDA.APPROVED);
    } else if (fda_approval === 0) {
      taglib.set_tag(node, TAGS.NODE.FDA.NOT_APPROVED);
    } else {
      taglib.set_tag(node,
        new taglib.Tag({
          id: `${TAG_SCOPE.NODE}/fda/${fda_approval}`,
          name: `Clinical Trial Phase ${fda_approval}`
        }));
    }
  }
  return node;
}

function decorate_role_tags(node) {
  const annotations = node.annotations.chemical;
  if (!cmn.is_missing(annotations.roles)) {
    for (const role of annotations.roles) {
      taglib.set_tag(node,
        new taglib.Tag({id: `${TAG_SCOPE.NODE}/role/${role.id}`, name: cmn.titleize(role.name)}));
    }
  }
  return node;
}

function decorate_otc_tag(node) {
  const annotations = node.annotations.chemical;
  const otc_code = annotations.otc_status?.code ?? null;
  switch(otc_code) {
    case 2: taglib.set_tag(node, TAGS.NODE.OTC.OTC); break;
    case 1: taglib.set_tag(node, TAGS.NODE.OTC.PRESCRIPTION); break;
    case 0: taglib.set_tag(node, TAGS.NODE.OTC.DISCONTINUED); break;
    case -2: taglib.set_tag(node, TAGS.NODE.OTC.WITHDRAWN); break;
    case -1:
    default: taglib.set_tag(node, TAGS.NODE.OTC.UNKNOWN);
  }
  return node;
}

function decorate_indication_tag(path, nodes) {
  // Consider the chemical indicated for the disease iff
  //   1. The chemical is marked as indicated for the disease
  //   2. The chemical has reached phase 4 approval from the FDA
  const path_start = nodes[path.start];
  const indications = path_start.annotations.chemical.indications ?? null;
  if (!cmn.is_missing(indications)
      && taglib.has_tag(path_start, TAGS.NODE.FDA.APPROVED)) {
    const indications_set = new Set(indications);
    const path_end = nodes[path.end];
    const path_end_mesh_ids = path_end.curies.filter(curie => curie.startsWith("MESH:"));
    for (let i = 0; i < path_end_mesh_ids.length; i++) {
      if (indications_set.has(path_end_mesh_ids[i])) {
        taglib.set_tag(path_start, TAGS.NODE.INDICATED.YES);
        return path_start;
      }
    }
  }
  taglib.set_tag(path_start, TAGS.NODE.INDICATED.NO);
  return path_start;
}

function decorate_max_phase_tag(path, nodes, query_type) {
  // Only generate this tag for non-gene/chemical queries
  if (!trapi.is_valid_query(query_type)
      || trapi.is_gene_chemical_query(query_type)) {
    return path;
  }
  const path_start = nodes[path.start];
  const fda_tags = taglib.get_tags(path_start).filter(is_fda_tag);
  let highest_fda_approval = 0;
  if (!cmn.is_array_empty(fda_tags)) {
    highest_fda_approval = Math.max(...fda_tags.map((tag) => {
      return parseInt(tag.id.split('/')[2]);
    }));
  }
  if (__is_drug(path_start, highest_fda_approval)) {
    taglib.set_tag(path, TAGS.RESULT.CHEMICAL_CLASS.DRUG);
  } else if (__is_clinical_phase(highest_fda_approval)) {
    taglib.set_tag(path, new taglib.Tag({
      id: `${TAG_SCOPE.RESULT}/cc/phase${highest_fda_approval}`,
      name: `Phase ${highest_fda_approval} Drug`
    }));
  } else {
    taglib.set_tag(path, TAGS.RESULT.CHEMICAL_CLASS.OTHER);
  }
  return path;

  function __is_drug(node, fda_level) {
    return fda_level === 4 || node.get_specific_type() === 'Drug';
  }

  function __is_clinical_phase(fda_level) {
    return fda_level > 0 && fda_level < 4;
  }
}

const _EXTERNAL_FAMILIES = Object.freeze(['cc', 'di', 'pc', 'pt', 'role', 'otc', 'tdl']);

function _get_family(tag) {
  return tag.id.split('/')[1];
}

function _promote(tag, scope) {
  const [_, ...rest] = tag.id.split('/');
  return new taglib.Tag({
    id: `${scope}/${rest.join('/')}`,
    name: tag.description.name,
    description: tag.description.description
  });
}

function _promote_external_tags(from_obj, path, scope) {
  for (const tag of taglib.get_tags(from_obj)) {
    if (!is_external_tag(tag)) continue;
    taglib.set_tag(path, _promote(tag, scope));
  }
  return path;
}
