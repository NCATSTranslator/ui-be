export { suite }

import * as test from "#test/lib/common.mjs";
import * as taglib from "#lib/taglib.mjs";

const suite = {
  tests: {
    make_rule_group_publications_by_knowledge_level: _test_make_rule_group_publications_by_knowledge_level(),
    make_rule_collect_publication_supporting_text: _test_make_rule_collect_publication_supporting_text(),
    make_rule_collect_clinical_trial_metadata: _test_make_rule_collect_clinical_trial_metadata(),
    make_rule_tag_attribute: _test_make_rule_tag_attribute()
  }
}

function _test_make_rule_group_publications_by_knowledge_level() {
  return test.make_function_test({
    "single-publication": {
      "args": [],
      "expected": {
        "publications": {
          "test-kl": [
            {"id": "PMID:123", "src": "test-source"}
          ]
        }
      },
      "context": {
        "source": {
          "attributes": [
            {"attribute_type_id": "biolink:publications", "value": ["PMID:123"]}
          ],
          "__context__": {"provenance": "test-source", "knowledge_level": "test-kl"}
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "multi-publication": {
      "args": [],
      "expected": {
        "publications": {
          "test-kl-1": [
            {"id": "PMID:1", "src": "test-source-1"},
            {"id": "PMID:2", "src": "test-source-1"},
            {"id": "PMID:3", "src": "test-source-2"}
          ],
          "test-kl-2": [
            {"id": "PMID:4", "src": "test-source-1"},
            {"id": "PMID:1", "src": "test-source-3"}
          ]
        }
      },
      "context": {
        "source": [
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:1", "PMID:2"]}
            ],
            "__context__": {"provenance": "test-source-1", "knowledge_level": "test-kl-1"}
          },
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:4"]}
            ],
            "__context__": {"provenance": "test-source-1", "knowledge_level": "test-kl-2"}
          },
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:3"]}
            ],
            "__context__": {"provenance": "test-source-2", "knowledge_level": "test-kl-1"}
          },
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:1"]}
            ],
            "__context__": {"provenance": "test-source-3", "knowledge_level": "test-kl-2"}
          }
        ],
        "target": {}
      },
      post: test.apply_rule
    },
    "duplicate-publication": {
      "args": [],
      "expected": {
        "publications": {
          "test-kl": [
            {"id": "PMID:1", "src": "test-source"},
            {"id": "PMID:1", "src": "test-source"}
          ]
        }
      },
      "context": {
        "source": [
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:1"]}
            ],
            "__context__": {"provenance": "test-source", "knowledge_level": "test-kl"}
          },
          {
            "attributes": [
              {"attribute_type_id": "biolink:publications", "value": ["PMID:1"]}
            ],
            "__context__": {"provenance": "test-source", "knowledge_level": "test-kl"}
          }
        ],
        "target": {}
      },
      post: test.apply_rule
    },
    "no-publications": {
      "args": [],
      "expected": {
        "publications": {}
      },
      "context": {
        "source": {},
        "target": {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_collect_publication_supporting_text() {
  const long_text = "The existing publications do not confirm the connection between fatigue syndrome and duration of treatment and type of medications administered (except amantadine in the study of Martinez-Martin et al., as the percentage of patients with fatigue syndrome treated with amantadine was significantly lower).";
  return test.make_function_test({
    "single-sentence": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMC:4746390": {
            "text": long_text,
            "subject": [269, 278],
            "object": [239, 245]
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_studies",
              "value": {
                "STUDY:1": {
                  "id": "STUDY:1",
                  "category": "biolink:Study",
                  "has_study_results": {
                    "RESULT:1": {
                      "id": "RESULT:1",
                      "xref": ["PMC:4746390"],
                      "supporting_text": [long_text],
                      "subject_location_in_text": [268, 278],
                      "object_location_in_text": [238, 245]
                    }
                  }
                }
              }
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "multi-sentence": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMC:4746390": {
            "text": long_text,
            "subject": [269, 278],
            "object": [239, 245]
          },
          "PMC:123": {
            "text": "ok",
            "subject": [1, 1],
            "object": [2, 2]
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_studies",
              "value": {
                "STUDY:1": {
                  "id": "STUDY:1",
                  "category": "biolink:Study",
                  "has_study_results": {
                    "RESULT:1": {
                      "id": "RESULT:1",
                      "xref": ["PMC:4746390"],
                      "supporting_text": [long_text],
                      "subject_location_in_text": [268, 278],
                      "object_location_in_text": [238, 245]
                    },
                    "RESULT:2": {
                      "id": "RESULT:2",
                      "xref": ["PMC:123"],
                      "supporting_text": ["ok"],
                      "subject_location_in_text": [0, 1],
                      "object_location_in_text": [1, 2]
                    }
                  }
                }
              }
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "non-study-category-skipped": {
      "args": [],
      "expected": {
        "supporting_text": {}
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_studies",
              "value": {
                "CLINICALTRIALS:NCT1": {
                  "id": "CLINICALTRIALS:NCT1",
                  "category": ["biolink:ClinicalTrial"],
                  "has_study_results": {}
                }
              }
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "no-attributes": {
      "args": [],
      "expected": {
        "supporting_text": {}
      },
      "context": {
        "source": {
          "attributes": []
        },
        "target": {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_collect_clinical_trial_metadata() {
  return test.make_function_test({
    "single-trial": {
      "args": [],
      "expected": {
        "supporting_trials": {
          "NCT02040909": {
            "phase": 1,
            "status": "TERMINATED",
            "child": true,
            "start_date": "2014-07",
            "size": 91,
            "type": "enrolled",
            "title": "Optimizing Propofol Dosing for (Preterm) Newborn Infants That Need Endotracheal Intubation"
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_studies",
              "value": {
                "CLINICALTRIALS:NCT02040909": {
                  "id": "CLINICALTRIALS:NCT02040909",
                  "category": [
                    "biolink:ClinicalTrial"
                  ],
                  "name": "Optimizing Propofol Dosing for (Preterm) Newborn Infants That Need Endotracheal Intubation",
                  "clinical_trial_phase": "clinical_trial_phase_1",
                  "clinical_trial_primary_purpose": "TREATMENT",
                  "clinical_trial_intervention_model": "SINGLE_GROUP",
                  "clinical_trial_overall_status": "TERMINATED",
                  "clinical_trial_enrollment_type": "ACTUAL",
                  "clinical_trial_start_date": "2014-07",
                  "clinical_trial_enrollment": 91,
                  "clinical_trial_age_stage": [
                    "child"
                  ],
                  "clinical_trial_age_range": "up to 28 days",
                  "clinical_trial_tested_intervention": "yes"
                }
              }
            },
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_tag_attribute() {
  return test.make_function_test({
    valid_attribute: {
      args: [{
        attr_id: "test-id",
        gen_tag: _gen_test_tag
      }],
      expected: taglib.set_tag(
        _make_empty_taggable_test_obj(),
        _gen_test_tag("1")
      ),
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-id",
              value: "1"
            }
          ]
        },
        target: _make_empty_taggable_test_obj()
      },
      post: test.apply_rule
    },
    no_valid_attribute: {
      args: [{
        attr_id: "test-id",
        gen_tag: _gen_test_tag
      }],
      expected: _make_empty_taggable_test_obj(),
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-id-a",
              value: "1"
            }
          ]
        },
        target: _make_empty_taggable_test_obj()
      },
      post: test.apply_rule
    },
    no_attributes: {
      args: [{
        attr_id: "test-id",
        gen_tag: _gen_test_tag
      }],
      expected: _make_empty_taggable_test_obj(),
      context: {
        source: {},
        target: _make_empty_taggable_test_obj()
      },
      post: test.apply_rule
    }
  });

  function _gen_test_tag(val) {
    return new taglib.Tag({
      id: val,
      name: `Value ${val}`
    });
  }

  function _make_empty_taggable_test_obj() {
    return taglib.make_taggable({});
  }
}

