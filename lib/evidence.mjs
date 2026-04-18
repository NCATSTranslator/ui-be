"use strict";

import { logger } from "./logger.mjs";
import * as cmn from "#lib/common.mjs";

export function is_valid_id(id) {
  const [url, type] = id_to_type_and_url(id);
  const is_valid = !(cmn.is_missing(url) || cmn.is_missing(type));
  if (!is_valid) {
    logger.error(`Invalid id: ${id}`);
  }
  return is_valid;
}

export function sanitize(id) {
  if (id.startsWith("PMC:")) {
    return id.replace(":", "");
  }
  // Sometimes we get a clinicaltrials link instead of an id
  if (id.startsWith("https://clinicaltrials.gov/")) {
    const nctId = id.match(/NCT\d{8}/)[0];
    return `clinicaltrials:${nctId}`;
  }
  if (id.startsWith("http:")) {
    return id.replace("http:", "https:");
  }
  return id;
}

export function is_clinical_trial(id) {
  return typeof(id) === "string" && id.startsWith("CLINICALTRIALS:");
}

export function is_publication(id) {
  return typeof(id) === "string" &&
    (id.startsWith("PMID") ||
     id.startsWith("PMC"));
}

export function is_ontological(predicate) {
  return predicate.includes("superclass") ||
         predicate.includes("subclass");
}

export function id_to_type_and_url(id) {
  if (_has_tag(id, "PMID")) {
    return ["PMID", _pmid_to_url(id)];
  } else if (_has_tag(id, "PMCID")) {
    return ["PMC", _pmcid_to_url(id)];
  } else if (_has_tag(id, "PMC")) {
    return ["PMC", _pmc_to_url(id)];
  } else if (_has_tag(id, "clinicaltrials")) {
    return ["NCT", _nctid_to_url(id)];
  } else if (_has_tag(id, "NCT")) {
    return ["NCT", _nct_to_url(id)];
  } else if (_has_tag(id, "DOI")) {
    return ["DOI", _doiid_to_url(id)];
  } else if (_has_tag(id, "CHEMBL.DOCUMENT")) {
    return ["CHEMBL", _chembl_to_url(id)];
  } else if (_has_tag(id, "ISBN")) {
    return ["ISBN", _isbn_to_url(id)];
  } else if (_is_url(id)) {
    return ["other", id];
  } else {
    return [null, null];
  }

  function _strip_tag(id) {
    const stripped_id = id.split(":");
    if (stripped_id.length > 1) {
      return stripped_id[1];
    }
    return false;
  }
  function _tagged_id_to_url(id, url) {
    const stripped_id = _strip_tag(id);
    if (!!stripped_id) {
      return `${url}/${stripped_id}`;
    }
    return false;
  }
  function _pmid_to_url(id) {
    return _tagged_id_to_url(id, "http://www.ncbi.nlm.nih.gov/pubmed");
  }
  function _pmcid_to_url(id) {
    return _tagged_id_to_url(id, "http://pmc.ncbi.nlm.nih.gov/articles/");
  }
  function _pmc_to_url(id) {
    return `https://pmc.ncbi.nlm.nih.gov/articles/${id}`;
  }
  function _nctid_to_url(id) {
    return _tagged_id_to_url(id, "https://clinicaltrials.gov/ct2/show");
  }
  function _nct_to_url(id) {
    return `https://clinicaltrials.gov/ct2/show/${id}`;
  }
  function _doiid_to_url(id) {
    return _tagged_id_to_url(id, "https://www.doi.org");
  }
  function _isbn_to_url(id) {
    return _tagged_id_to_url(id, "https://www.isbn-international.org/identifier");
  }
  function _chembl_to_url(id) {
    return _tagged_id_to_url(id, "https://www.ebi.ac.uk/chembl/explore/document");
  }
  function _has_tag(id, tag) {
    return id.startsWith(tag);
  }
  function _is_url(id) {
    return _has_tag(id, "http") || _has_tag(id, "https");
  }
}
