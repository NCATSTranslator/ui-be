"use strict";

import { logger } from "./logger.mjs";
import * as cmn from "#lib/common.mjs";

export function is_valid_id(id) {
  const [type, url] = id_to_type_and_url(id);
  const is_valid = !(cmn.is_missing(url) || cmn.is_missing(type));
  if (!is_valid) {
    logger.debug(`Invalid id: ${id}`);
  }
  return is_valid;
}

export function is_go_ref(id) {
  return "string" === typeof(id) && id.toLowerCase().startsWith("go_ref:");
}

export function sanitize(id) {
  if (id.startsWith("PMC:")) return id.replace(":", "");
  // Sometimes we get a clinicaltrials link instead of an id
  if (id.startsWith("https://clinicaltrials.gov/")) {
    // TODO: throws on a clinicaltrials.gov URL with no NCT id (.match() -> null),
    // which is reachable since is_valid_id passes such URLs through as "other".
    // Fall back to the original id (or null) instead of throwing.
    const nctId = id.match(/NCT\d{8}/)[0];
    return `clinicaltrials:${nctId}`;
  }
  if (id.startsWith("http:")) return id.replace("http:", "https:");
  return id;
}

export function is_clinical_trial(id) {
  return typeof(id) === "string" && id.toLowerCase().startsWith("clinicaltrials:");
}

export function is_publication(id) {
  if (typeof(id) !== "string") return false;
  const lower_id = id.toLowerCase();
  return lower_id.startsWith("pmid") || lower_id.startsWith("pmc");
}

export function is_ontological(predicate) {
  if (typeof(predicate) !== "string") return false;
  const lower_predicate = predicate.toLowerCase();
  return lower_predicate.includes("superclass") || lower_predicate.includes("subclass");
}

function _tagged_url(id, base) {
  const value = id.split(":")[1];
  return value ? `${base}/${value}` : null;
}

function _prefixed_url(id, prefix, base) {
  return id.slice(prefix.length) ? `${base}/${id}` : null;
}

function _pmcid_url(id) {
  const value = id.split(":")[1];
  return value
    ? `https://pmc.ncbi.nlm.nih.gov/articles/PMC${value}`
    : null;
}

function _dailymed_url(id) {
  const value = id.split(":")[1];
  return value
    ? `https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=${value}`
    : null;
}

const _ID_RESOLVERS = [
  {
    tag: "PMID",
    type: "PMID",
    to_url: (id) => _tagged_url(id, "https://www.ncbi.nlm.nih.gov/pubmed")
  },
  {
    tag: "PMCID",
    type: "PMC",
    to_url: (id) => _pmcid_url(id)
  },
  { 
    tag: "PMC",
    type: "PMC",
    to_url: (id) => _prefixed_url(id, "PMC", "https://pmc.ncbi.nlm.nih.gov/articles")
  },
  {
    tag: "clinicaltrials",
    type: "NCT",
    to_url: (id) => _tagged_url(id, "https://clinicaltrials.gov/ct2/show")
  },
  {
    tag: "NCT",
    type: "NCT",
    to_url: (id) => _prefixed_url(id, "NCT", "https://clinicaltrials.gov/ct2/show")
  },
  {
    tag: "DOI",
    type: "DOI",
    to_url: (id) => _tagged_url(id, "https://www.doi.org")
  },
  {
    tag: "CHEMBL.DOCUMENT",
    type: "CHEMBL",
    to_url: (id) => _tagged_url(id, "https://www.ebi.ac.uk/chembl/explore/document")
  },
  {
    tag: "ISBN",
    type: "ISBN",
    to_url: (id) => _tagged_url(id, "https://www.isbn-international.org/identifier")
  },
  {
    tag: "dailymed",
    type: "dailymed",
    to_url: (id) => _dailymed_url(id)
  },
];

export function id_to_type_and_url(id) {
  const lower_id = id.toLowerCase();
  for (const resolver of _ID_RESOLVERS) {
    if (lower_id.startsWith(resolver.tag.toLowerCase())) {
      const url = resolver.to_url(id);
      return url ? [resolver.type, url] : [null, null];
    }
  }
  if (lower_id.startsWith("http")) return ["other", id];
  return [null, null];
}
