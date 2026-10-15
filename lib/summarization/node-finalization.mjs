export {
  finalize_nodes
}

import { logger } from '../logger.mjs';
import * as cmn from '../common.mjs';
import * as bl from '../biolink-model.mjs';
import * as bta from '../biothings-annotation.mjs';
import * as taglib from '../taglib.mjs';
import * as summary_tags from './tags.mjs';

function finalize_nodes(nodes) {
  const trials_by_disease = _collect_trials_by_disease(nodes);
  for (const nid of Object.keys(nodes)) {
    const node = nodes[nid];
    node.curies.push(nid);
    cmn.obj_remove_duplicates(node);
    node.types.sort(bl.biolink_class_cmp_fn);
    _conflate_gene_and_protein(node);
    if (cmn.is_array_empty(node.names)) {
      node.names.push(nid);
    }

    const provenance = bl.curie_to_normalized_url(nid, node.curies);
    if (!provenance) {
      logger.warn(`WARNING: No provenance for CURIE: ${nid}`);
    } else {
      node.provenance.push(provenance);
    }

    if (bta.is_chemical(node)) {
      _narrow_trials_to_ids(node.annotations.chemical.clinical_trials);
    }
    if (bta.is_disease(node)) {
      _finalize_disease(node, trials_by_disease.get(nid));
    }
    if (bta.is_gene(node)) {
      _tag_tdls(node);
    }
    node.curies = cmn.distinct_array(node.curies);
    bta.inject_curies(node);
  }
  return nodes;
}

function _collect_trials_by_disease(nodes) {
  const trials_by_disease = new Map();
  for (const node of Object.values(nodes)) {
    const trials = node.annotations?.chemical?.clinical_trials?.value;
    if (cmn.is_missing(trials)) continue;
    for (const trial of trials) {
      for (const disease_id of trial.disease_ids) {
        if (!nodes.hasOwnProperty(disease_id)) continue;
        let trial_ids = trials_by_disease.get(disease_id);
        if (trial_ids === undefined) {
          trial_ids = new Set();
          trials_by_disease.set(disease_id, trial_ids);
        }
        trial_ids.add(trial.id);
      }
    }
  }
  return trials_by_disease;
}

function _conflate_gene_and_protein(node) {
  if (node.get_specific_type() !== "Protein") return;
  const gene_type = bl.tag_biolink("Gene");
  const gene_index = node.types.indexOf(gene_type);
  if (gene_index > -1) {
    node.types.splice(gene_index, 1);
    node.types.unshift(gene_type);
  }
}

function _narrow_trials_to_ids(clinical_trials) {
  if (cmn.is_missing(clinical_trials)) return;
  clinical_trials.value = clinical_trials.value.map(trial => trial.id);
}

function _finalize_disease(node, trial_ids) {
  const curie_entries = node.annotations?.disease?.curies?.value ?? [];
  node.curies.push(...curie_entries.map(entry => entry.curie));
  if (trial_ids !== undefined) {
    node.annotations.disease.clinical_trials = bta.make_clinical_trials_section([...trial_ids]);
  }
}

function _tag_tdls(node) {
  const tdls = node.annotations.gene.tdl?.value ?? [];
  for (const tdl of tdls) {
    taglib.set_tag(node, summary_tags.make_tdl_tag(tdl));
  }
}
