export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    is_valid_id: _test_is_valid_id(),
    is_go_ref: _test_is_go_ref(),
    sanitize: _test_sanitize(),
    is_clinical_trial: _test_is_clinical_trial(),
    is_publication: _test_is_publication(),
    is_ontological: _test_is_ontological(),
    id_to_type_and_url: _test_id_to_type_and_url()
  }
};

function _test_is_valid_id() {
  return test.make_function_test({
    "valid_curie_with_id": {
      "args": ["PMID:12345"],
      "expected": true
    },
    "valid_generic_url": {
      "args": ["https://example.com/article"],
      "expected": true
    },
    "unknown_prefix": {
      "args": ["foo:bar"],
      "expected": false
    },
    "known_prefix_missing_id": {
      "args": ["dailymed:"],
      "expected": false
    }
  });
}

function _test_is_go_ref() {
  return test.make_function_test({
    "lowercase_prefix": {
      "args": ["go_ref:0000024"],
      "expected": true
    },
    "uppercase_prefix_is_case_insensitive": {
      "args": ["GO_REF:0000024"],
      "expected": true
    },
    "different_prefix": {
      "args": ["PMID:12345"],
      "expected": false
    },
    "prefix_without_colon": {
      "args": ["go_ref"],
      "expected": false
    },
    "non_string_input": {
      "args": [12345],
      "expected": false
    }
  });
}

function _test_sanitize() {
  return test.make_function_test({
    "pmc_curie_drops_colon": {
      "args": ["PMC:12345"],
      "expected": "PMC12345"
    },
    "clinicaltrials_link_to_curie": {
      "args": ["https://clinicaltrials.gov/study/NCT12345678"],
      "expected": "clinicaltrials:NCT12345678"
    },
    "http_upgraded_to_https": {
      "args": ["http://www.example.com/x"],
      "expected": "https://www.example.com/x"
    },
    "unmatched_id_passes_through": {
      "args": ["PMID:999"],
      "expected": "PMID:999"
    },
    "clinicaltrials_link_without_nct_throws": {
      "args": ["https://clinicaltrials.gov/no-nct-here"],
      "expected": TypeError
    }
  });
}

function _test_is_clinical_trial() {
  return test.make_function_test({
    "uppercase_prefix": {
      "args": ["CLINICALTRIALS:NCT12345678"],
      "expected": true
    },
    "lowercase_prefix_is_case_insensitive": {
      "args": ["clinicaltrials:NCT12345678"],
      "expected": true
    },
    "mixed_case_prefix_is_case_insensitive": {
      "args": ["ClinicalTrials:NCT12345678"],
      "expected": true
    },
    "different_prefix": {
      "args": ["PMID:12345"],
      "expected": false
    },
    "non_string_input": {
      "args": [12345],
      "expected": false
    }
  });
}

function _test_is_publication() {
  return test.make_function_test({
    "pmid_curie": {
      "args": ["PMID:12345"],
      "expected": true
    },
    "pmc_id": {
      "args": ["PMC12345"],
      "expected": true
    },
    "lowercase_pmid_is_case_insensitive": {
      "args": ["pmid:12345"],
      "expected": true
    },
    "lowercase_pmc_is_case_insensitive": {
      "args": ["pmc12345"],
      "expected": true
    },
    "different_prefix": {
      "args": ["DOI:10.1/x"],
      "expected": false
    },
    "non_string_input": {
      "args": [12345],
      "expected": false
    },
    "undefined_input": {
      "args": [undefined],
      "expected": false
    },
    "null_input": {
      "args": [null],
      "expected": false
    }
  });
}

function _test_is_ontological() {
  return test.make_function_test({
    "superclass_predicate": {
      "args": ["biolink:superclass_of"],
      "expected": true
    },
    "subclass_predicate": {
      "args": ["biolink:subclass_of"],
      "expected": true
    },
    "uppercase_is_case_insensitive": {
      "args": ["biolink:SUPERCLASS_of"],
      "expected": true
    },
    "non_ontological_predicate": {
      "args": ["biolink:treats"],
      "expected": false
    },
    "non_string_input": {
      "args": [123],
      "expected": false
    },
    "undefined_input": {
      "args": [undefined],
      "expected": false
    },
    "null_input": {
      "args": [null],
      "expected": false
    }
  });
}

function _test_id_to_type_and_url() {
  return test.make_function_test({
    // --- Branches that resolve to a [type, url] pair ---
    "pmid": {
      "args": ["PMID:12345"],
      "expected": ["PMID", "https://www.ncbi.nlm.nih.gov/pubmed/12345"]
    },
    "pmcid": {
      "args": ["PMCID:7654321"],
      "expected": ["PMC", "https://pmc.ncbi.nlm.nih.gov/articles/PMC7654321"]
    },
    "pmc": {
      "args": ["PMC7654321"],
      "expected": ["PMC", "https://pmc.ncbi.nlm.nih.gov/articles/PMC7654321"]
    },
    "lowercase_pmid_is_case_insensitive": {
      "args": ["pmid:12345"],
      "expected": ["PMID", "https://www.ncbi.nlm.nih.gov/pubmed/12345"]
    },
    "uppercase_clinicaltrials_is_case_insensitive": {
      "args": ["CLINICALTRIALS:NCT12345678"],
      "expected": ["NCT", "https://clinicaltrials.gov/ct2/show/NCT12345678"]
    },
    "clinicaltrials": {
      "args": ["clinicaltrials:NCT12345678"],
      "expected": ["NCT", "https://clinicaltrials.gov/ct2/show/NCT12345678"]
    },
    "nct": {
      "args": ["NCT12345678"],
      "expected": ["NCT", "https://clinicaltrials.gov/ct2/show/NCT12345678"]
    },
    "doi": {
      "args": ["DOI:10.1000/xyz123"],
      "expected": ["DOI", "https://www.doi.org/10.1000/xyz123"]
    },
    "chembl_document": {
      "args": ["CHEMBL.DOCUMENT:CHEMBL1158643"],
      "expected": ["CHEMBL", "https://www.ebi.ac.uk/chembl/explore/document/CHEMBL1158643"]
    },
    "isbn": {
      "args": ["ISBN:9780470059029"],
      "expected": ["ISBN", "https://www.isbn-international.org/identifier/9780470059029"]
    },
    "dailymed_setid": {
      "args": ["dailymed:e1ada3ec-c2c0-4f43-9b4b-1234567890ab"],
      "expected": ["dailymed", "https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=e1ada3ec-c2c0-4f43-9b4b-1234567890ab"]
    },
    "https_url_is_other": {
      "args": ["https://example.com/article"],
      "expected": ["other", "https://example.com/article"]
    },
    "http_url_is_other": {
      "args": ["http://example.com/article"],
      "expected": ["other", "http://example.com/article"]
    },
    // --- Tag matched but no usable value -> [null, null] ---
    "pmc_prefix_only": {
      "args": ["PMC"],
      "expected": [null, null]
    },
    "nct_prefix_only": {
      "args": ["NCT"],
      "expected": [null, null]
    },
    "pmid_prefix_without_colon": {
      "args": ["PMID"],
      "expected": [null, null]
    },
    "pmid_empty_value": {
      "args": ["PMID:"],
      "expected": [null, null]
    },
    "pmcid_empty_value": {
      "args": ["PMCID:"],
      "expected": [null, null]
    },
    "clinicaltrials_empty_value": {
      "args": ["clinicaltrials:"],
      "expected": [null, null]
    },
    "doi_empty_value": {
      "args": ["DOI:"],
      "expected": [null, null]
    },
    "chembl_document_empty_value": {
      "args": ["CHEMBL.DOCUMENT:"],
      "expected": [null, null]
    },
    "isbn_empty_value": {
      "args": ["ISBN:"],
      "expected": [null, null]
    },
    "dailymed_missing_setid": {
      "args": ["dailymed:"],
      "expected": [null, null]
    },
    "dailymed_prefix_without_colon": {
      "args": ["dailymed"],
      "expected": [null, null]
    },
    // --- No matching tag and not a URL -> [null, null] ---
    "unknown_prefix": {
      "args": ["foo:bar"],
      "expected": [null, null]
    },
    "empty_string": {
      "args": [""],
      "expected": [null, null]
    }
  });
}
