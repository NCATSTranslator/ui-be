export { suite }

import * as cmn from "#lib/common.mjs";
import * as test from "#test/lib/common.mjs";
import * as taglib from "#lib/taglib.mjs";

const suite = {
  tests: {
    make_rule_group_publications_by_knowledge_level: _test_make_rule_group_publications_by_knowledge_level(),
    make_rule_collect_publication_supporting_text: _test_make_rule_collect_publication_supporting_text(),
    make_rule_collect_semmed_sentences: _test_make_rule_collect_semmed_sentences()


,
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
              {"attribute_type_id": "biolink:supporting_document", "value": ["PMID:1", "PMID:2"]}
            ],
            "__context__": {"provenance": "test-source-1", "knowledge_level": "test-kl-1"}
          },
          {
            "attributes": [
              {"attribute_type_id": "biolink:Publication", "value": ["PMID:4"]}
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
              {"attribute_type_id": "biolink:publication", "value": ["PMID:1"]}
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
  return test.make_function_test({
    "single-sentence": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMC:4746390": {
            "text": "The existing publications do not confirm the connection between fatigue syndrome and duration of treatment and type of medications administered (except amantadine in the study of Martinez-Martin et al., as the percentage of patients with fatigue syndrome treated with amantadine was significantly lower).",
            "subject": [269,278],
            "object": [239,245]
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_study_result",
              "attributes": [
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:supporting_text",
                  "value": "The existing publications do not confirm the connection between fatigue syndrome and duration of treatment and type of medications administered (except amantadine in the study of Martinez-Martin et al., as the percentage of patients with fatigue syndrome treated with amantadine was significantly lower).",
                  "value_type_id": "EDAM:data_3671"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:publications",
                  "value": "PMC:4746390",
                  "value_type_id": "biolink:Uriorcurie",
                  "value_url": "https://pubmed.ncbi.nlm.nih.gov/PMC4746390/"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:supporting_text_located_in",
                  "value": "DISCUSS",
                  "value_type_id": "IAO_0000314"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:extraction_confidence_score",
                  "value": 0.99937505,
                  "value_type_id": "EDAM:data_1772"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:subject_location_in_text",
                  "value": "268|278",
                  "value_type_id": "SIO:001056"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted ",
                  "attribute_type_id": "biolink:object_location_in_text",
                  "value": "238|245",
                  "value_type_id": "SIO:001056"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:supporting_document_year",
                  "value": 2016,
                  "value_type_id": "UO:0000036"
                }
              ]
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "multi-sentence": {
      "args": [
        {"attr_ids": ["biolink:has_supporting_study_result"]}
      ],
      "expected": {
        "supporting_text": {
          "PMC:4746390": {
            "text": "The existing publications do not confirm the connection between fatigue syndrome and duration of treatment and type of medications administered (except amantadine in the study of Martinez-Martin et al., as the percentage of patients with fatigue syndrome treated with amantadine was significantly lower).",
            "subject": [269,278],
            "object": [239,245]
          },
          "PMC:123": {
            "text": "ok",
            "subject": [1,1],
            "object": [2,2]
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:has_supporting_study_result",
              "attributes": [
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:supporting_text",
                  "value": "The existing publications do not confirm the connection between fatigue syndrome and duration of treatment and type of medications administered (except amantadine in the study of Martinez-Martin et al., as the percentage of patients with fatigue syndrome treated with amantadine was significantly lower).",
                  "value_type_id": "EDAM:data_3671"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:publications",
                  "value": "PMC:4746390",
                  "value_type_id": "biolink:Uriorcurie",
                  "value_url": "https://pubmed.ncbi.nlm.nih.gov/PMC4746390/"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:supporting_text_located_in",
                  "value": "DISCUSS",
                  "value_type_id": "IAO_0000314"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:extraction_confidence_score",
                  "value": 0.99937505,
                  "value_type_id": "EDAM:data_1772"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:subject_location_in_text",
                  "value": "268|278",
                  "value_type_id": "SIO:001056"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted ",
                  "attribute_type_id": "biolink:object_location_in_text",
                  "value": "238|245",
                  "value_type_id": "SIO:001056"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:supporting_document_year",
                  "value": 2016,
                  "value_type_id": "UO:0000036"
                }
              ]
            },
            {
              "attribute_type_id": "biolink:has_supporting_study_result",
              "attributes": [
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:supporting_text",
                  "value": "ok",
                  "value_type_id": "EDAM:data_3671"
                },
                {
                  "attribute_source": "infores:pubmed",
                  "attribute_type_id": "biolink:publications",
                  "value": "PMC:123",
                  "value_type_id": "biolink:Uriorcurie",
                  "value_url": "https://pubmed.ncbi.nlm.nih.gov/PMC4746390/"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted",
                  "attribute_type_id": "biolink:subject_location_in_text",
                  "value": "0|1",
                  "value_type_id": "SIO:001056"
                },
                {
                  "attribute_source": "infores:text-mining-provider-targeted ",
                  "attribute_type_id": "biolink:object_location_in_text",
                  "value": "1|2",
                  "value_type_id": "SIO:001056"
                }
              ]
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "no-attributes": {
      "args": [
        {"attr_ids": ["biolink:has_supporting_study_result"]}
      ],
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

function _test_make_rule_collect_semmed_sentences() {
  return test.make_function_test({
    "single-sentence": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMID:10525314": {
            "text": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
            "subject": null,
            "object": null
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:supporting_text",
              "original_attribute_name": null,
              "value": {
                "PMID:10525314": {
                  "object score": "1000",
                  "publication date": "1999 Oct",
                  "sentence": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
                  "subject score": "1000"
                }
              }
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "multi-sentence-single-source": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMID:10525314": {
            "text": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
            "subject": null,
            "object": null
          },
          "PMID:11081773": {
            "text": "These results suggest that skeletal muscle TNF-alpha is linked to insulin resistance and hypertension and that angiotensin II may be one of the factors that regulate skeletal muscle TNF-alpha.",
            "subject": null,
            "object": null
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:supporting_text",
              "original_attribute_name": null,
              "value": {
                "PMID:10525314": {
                  "object score": "1000",
                  "publication date": "1999 Oct",
                  "sentence": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
                  "subject score": "1000"
                },
                "PMID:11081773": {
                  "object score": "901",
                  "publication date": "2000 Nov",
                  "sentence": "These results suggest that skeletal muscle TNF-alpha is linked to insulin resistance and hypertension and that angiotensin II may be one of the factors that regulate skeletal muscle TNF-alpha.",
                  "subject score": "1000"
                }
              }
            }
          ]
        },
        "target": {}
      },
      post: test.apply_rule
    },
    "multi-sentence-multi-source": {
      "args": [],
      "expected": {
        "supporting_text": {
          "PMID:10525314": {
            "text": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
            "subject": null,
            "object": null
          },
          "PMID:11081773": {
            "text": "These results suggest that skeletal muscle TNF-alpha is linked to insulin resistance and hypertension and that angiotensin II may be one of the factors that regulate skeletal muscle TNF-alpha.",
            "subject": null,
            "object": null
          }
        }
      },
      "context": {
        "source": [{
          "attributes": [
            {
              "attribute_type_id": "biolink:supporting_text",
              "original_attribute_name": null,
              "value": {
                "PMID:10525314": {
                  "object score": "1000",
                  "publication date": "1999 Oct",
                  "sentence": "This study aimed to examine whether lipopolysaccharide (LPS)-induced increase in tumour necrosis factor alpha (TNF-alpha) and interleukin 6 (IL-6) gene transcription was regulated by beta-adrenoceptor activation and whether TNF-alpha and IL-6 gene transcription was regulated by angiotensin II in rat renal resident macrophage cells.",
                  "subject score": "1000"
                }
              }
            }
          ]
        },
          {
            "attributes": [
              {
                "attribute_type_id": "biolink:supporting_text",
                "original_attribute_name": null,
                "value": {
                  "PMID:11081773": {
                    "object score": "901",
                    "publication date": "2000 Nov",
                    "sentence": "These results suggest that skeletal muscle TNF-alpha is linked to insulin resistance and hypertension and that angiotensin II may be one of the factors that regulate skeletal muscle TNF-alpha.",
                    "subject score": "1000"
                  }
                }
              }
            ]
          }],
        "target": {}
      },
      post: test.apply_rule
    },
    "no-sentences": {
      "args": [],
      "expected": {
        "supporting_text": {}
      },
      "context": {
        "source": {},
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
          "NCT03279861": {
            "phase": 4,
            "status": "WITHDRAWN",
            "child": false,
            "start_date": "2017-11",
            "size": 0,
            "type": "enrolled",
            "title": "Sacubitril-valsartan Versus Usual Anti-hypertensives in LVAD"
          }
        }
      },
      "context": {
        "source": {
          "attributes": [
            {
              "attribute_type_id": "biolink:supporting_study",
              "attributes": [
                {
                  "attribute_type_id": "clinical_trial_phase",
                  "value": 4,
                  "value_type_id": "biolink:ResearchPhaseEnum"
                },
                {
                  "attribute_type_id": "tested",
                  "value": "unsure"
                },
                {
                  "attribute_type_id": "primary_purpose",
                  "value": "TREATMENT"
                },
                {
                  "attribute_type_id": "intervention_model",
                  "value": "CROSSOVER"
                },
                {
                  "attribute_type_id": "time_perspective",
                  "value": "nan"
                },
                {
                  "attribute_type_id": "clinical_trial_status",
                  "value": "WITHDRAWN"
                },
                {
                  "attribute_type_id": "start_date",
                  "value": "2017-11"
                },
                {
                  "attribute_type_id": "study_size",
                  "value": 0,
                  "value_type_id": "metatype:Integer"
                },
                {
                  "attribute_type_id": "enrollment_type",
                  "value": "ACTUAL"
                },
                {
                  "attribute_type_id": "age_range",
                  "value": "18 years to 80 years"
                },
                {
                  "attribute_type_id": "child",
                  "value": false,
                  "value_type_id": "metatype:Boolean"
                },
                {
                  "attribute_type_id": "adult",
                  "value": true,
                  "value_type_id": "metatype:Boolean"},
                {
                  "attribute_type_id": "older_adult",
                  "value": true,
                  "value_type_id": "metatype:Boolean"
                },
                {
                  "attribute_type_id": "brief_title",
                  "value": "Sacubitril-valsartan Versus Usual Anti-hypertensives in LVAD",
                  "value_type_id": "metatype:String"
                }
              ],
              "value": "NCT03279861",
              "value_url": "https://clinicaltrials.gov/study/NCT03279861?tab=table"
            }
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





