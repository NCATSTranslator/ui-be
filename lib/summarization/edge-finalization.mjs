export {
  finalize_edges,
  SummaryPublication,
  _filter_redundant_provenance
}

import * as cmn from '../common.mjs';
import * as ev from '../evidence.mjs';
import * as bl from '../biolink-model.mjs';
import * as trapi from '../trapi/core.mjs';
import * as taglib from '../taglib.mjs';
import * as pred from './predicates.mjs';
import * as summary_tags from './tags.mjs';

function finalize_edges(edges, errors) {
  for (const eid of Object.keys(edges)) {
    const edge = edges[eid];
    const missing_reasons = _missing_property_reasons(edge);
    if (!cmn.is_array_empty(missing_reasons)) {
      console.error(`Found invalid edge ${eid}. Reasons: ${JSON.stringify(missing_reasons)}`);
      _record_edge_errors(edge, errors, missing_reasons);
      delete edges[eid];
      continue;
    }

    cmn.obj_remove_duplicates(edge);
    _dedupe_publications(edge);
    const infores_reasons = _invalid_infores_reasons(edge);
    if (!cmn.is_array_empty(infores_reasons)) {
      _record_edge_errors(edge, errors, infores_reasons);
      delete edges[eid];
      continue;
    }

    _tag_evidence(edge);
  }

  const publications = {};
  const trials = {};
  const provenance = {};
  for (const edge of Object.values(edges)) {
    _collect_publications(publications, edge);
    _collect_trials(trials, edge);
    _collect_provenance(provenance, edge);
    edge.knowledge_level = provenance[edge.provenance[0].infores].knowledge_level;
    _filter_redundant_provenance(edge);
    _attach_supporting_text(edge);
    edge.trials = Object.keys(cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, []));
    delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS];
    _finalize_predicate(edge);
  }

  return {
    edges: edges,
    publications: publications,
    trials: trials,
    provenance: provenance,
    errors: errors
  };
}

class SummaryPublication {
  constructor(type, url) {
    this.type = type;
    this.url = url;
    this.infores = new Set();
  }

  register_infores(infores) {
    this.infores.add(infores);
  }

  get_infores() {
    return [...this.infores];
  }

  to_obj() {
    return {
      type: this.type,
      url: this.url,
      infores: this.get_infores()
    }
  }
}

function _filter_redundant_provenance(edge) {
  const infores_with_records = new Set();
  for (const source of edge.provenance) {
    if (_has_records(source)) {
      infores_with_records.add(source.infores);
    }
  }
  edge.provenance = edge.provenance.filter(source => {
    return _has_records(source) || !infores_with_records.has(source.infores);
  });
  return edge;
}

function _has_records(source) {
  return !!source.records && source.records.length > 0;
}

function _edge_to_string(edge) {
  return `${edge.subject}-${edge.predicate}-${edge.object}`;
}

function _missing_property_reasons(edge) {
  const reasons = [];
  if (!edge.subject || !edge.object || !edge.predicate) {
    reasons.push(`Invalid edge found: ${_edge_to_string(edge)}`);
  }
  if (!edge.provenance || edge.provenance.length === 0) {
    reasons.push(`No provenance for edge: ${_edge_to_string(edge)}`);
  }
  return reasons;
}

function _invalid_infores_reasons(edge) {
  const reasons = [];
  for (const source of edge.provenance) {
    if (!bl.is_valid_infores(source.infores)) {
      reasons.push(`Found invalid infores: ${source.infores} on edge ${_edge_to_string(edge)}`);
    }
  }
  return reasons;
}

function _record_edge_errors(edge, errors, reasons) {
  const agent = edge.aras.length === 1 ? edge.aras[0] : 'unknown';
  const agent_errors = cmn.json_set_default_and_get(errors, agent, []);
  agent_errors.push(...reasons);
}

function _dedupe_publications(edge) {
  const publications = cmn.json_get(edge, 'publications', {});
  for (const kl of Object.keys(publications)) {
    const seen_ids = new Set();
    cmn.json_set(publications, kl, cmn.json_get(publications, kl, []).filter((publication) => {
      const is_new = !seen_ids.has(publication.id);
      if (ev.is_publication(publication.id)) {
        taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.PUBLICATIONS);
      }
      seen_ids.add(publication.id);
      return is_new;
    }));
  }
}

function _tag_evidence(edge) {
  if (!cmn.is_array_empty(Object.keys(edge.supporting_trials))) {
    taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.CLINICAL);
  }
  if (ev.is_ontological(edge.predicate)) {
    taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.ONTOLOGICAL);
  }
  taglib.set_tag(edge, summary_tags.TAGS.EDGE.EVIDENCE.OTHER);
}

function _collect_publications(publications, edge) {
  const edge_publications = cmn.json_get(edge, 'publications', {});
  for (const kl of Object.keys(edge_publications)) {
    for (const edge_publication of cmn.json_get(edge_publications, kl, [])) {
      const pid = edge_publication.id;
      let publication = cmn.json_get(publications, pid, null);
      if (null === publication) {
        const [type, url] = ev.id_to_type_and_url(pid);
        publication = new SummaryPublication(type, url);
        cmn.json_set(publications, pid, publication);
      }
      publication.register_infores(edge_publication.infores);
    }
  }
}

function _collect_trials(trials, edge) {
  const edge_trials = cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TRIALS, {});
  for (const ctid of Object.keys(edge_trials)) {
    const trial = edge_trials[ctid];
    trial.id = ctid;
    const [_, url] = ev.id_to_type_and_url(ctid);
    trial.url = url;
    trials[ctid] = trial;
  }
}

function _collect_provenance(provenance, edge) {
  for (const source of edge.provenance) {
    const infores = source.infores;
    const source_provenance = bl.infores_to_provenance(infores);
    if (!source_provenance || !cmn.is_missing(provenance[infores])) continue;
    provenance[infores] = source_provenance;
  }
}

function _attach_supporting_text(edge) {
  const edge_publications = cmn.json_get(edge, 'publications', {});
  const supporting_text = cmn.json_get(edge, trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT, {});
  for (const kl of Object.keys(edge_publications)) {
    edge_publications[kl] = edge_publications[kl].map((publication) => {
      return {
        id: publication.id,
        support: supporting_text[publication.id] ?? null,
        infores: publication.infores
      };
    });
  }
  delete edge[trapi.CONSTANTS.ATTRIBUTES.ID.SUPPORTING_TEXT];
}

function _finalize_predicate(edge) {
  const unqualified_predicate = pred.get_most_specific_predicate(edge);
  const canonical_predicate = pred.gen_canonical_qualified_predicate(edge);
  taglib.set_tag(edge, summary_tags.make_predicate_tag(canonical_predicate));
  cmn.json_set(edge, 'predicate', pred.gen_qualified_predicate(edge, edge.is_inverted()));
  cmn.json_set(edge, 'predicate_url', bl.pred_to_url(unqualified_predicate));
  cmn.json_set(edge, 'description', bl.get_predicate_description(unqualified_predicate));
  if (edge.is_inverted()) {
    const subject = edge.subject;
    edge.subject = edge.object;
    edge.object = subject;
  }
  delete edge['qualifiers'];
}
