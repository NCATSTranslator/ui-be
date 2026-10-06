export { suite }

import * as test from "#test/lib/common.mjs";
import { load_trapi, CONSTANTS, _QNode, _QEdge, _QEdgeQualifierSet, _QEdgeQualifier, _QPath, _QGraph } from "#lib/trapi/core.mjs";

const suite = {
  tests: {
    client_request_to_trapi_query: _test_client_request_to_trapi_query(),
    get_pk: _test_get_pk(),
    get_results: _test_get_results(),
    get_auxiliary_graphs: _test_get_auxiliary_graphs(),
    get_auxiliary_graph: _test_get_auxiliary_graph(),
    get_auxiliary_graph_edges: _test_get_auxiliary_graph_edges(),
    get_edge_bindings: _test_get_edge_bindings(),
    get_node_bindings: _test_get_node_bindings(),
    get_path_bindings: _test_get_path_bindings(),
    get_binding_id: _test_get_binding_id(),
    get_analyses: _test_get_analyses(),
    get_resource_id: _test_get_resource_id(),
    get_analysis_score: _test_get_analysis_score(),
    get_ordering_components: _test_get_ordering_components(),
    get_normalized_score: _test_get_normalized_score(),
    get_kgraph: _test_get_kgraph(),
    get_kedge: _test_get_kedge(),
    get_knode: _test_get_knode(),
    has_knode: _test_has_knode(),
    get_attrs: _test_get_attrs(),
    get_attr_id: _test_get_attr_id(),
    get_attr_val: _test_get_attr_val(),
    get_primary_source: _test_get_primary_source(),
    get_subject: _test_get_subject(),
    get_object: _test_get_object(),
    get_predicate: _test_get_predicate(),
    get_support_graphs: _test_get_support_graphs(),
    get_qualifiers: _test_get_qualifiers(),
    get_qualifier_id: _test_get_qualifier_id(),
    get_qualifier_val: _test_get_qualifier_val(),
    get_knowledge_level: _test_get_knowledge_level(),
    get_agent_type: _test_get_agent_type(),
    get_edge_type: _test_get_edge_type(),
    message_to_query_type: _test_message_to_query_type(),
    message_to_endpoints: _test_message_to_endpoints(),
    is_chemical_disease_query: _test_is_chemical_disease_query(),
    is_gene_chemical_query: _test_is_gene_chemical_query(),
    is_pathfinder_query: _test_is_pathfinder_query(),
    is_lookup_query: _test_is_lookup_query(),
    is_valid_query: _test_is_valid_query(),
    AttributeIterator: _test_AttributeIterator(),
    _Query: _test_Query(),
    _QNode: _test_QNode(),
    _QEdge: _test_QEdge(),
    _QEdgeQualifierSet: _test_QEdgeQualifierSet(),
    _QEdgeQualifier: _test_QEdgeQualifier(),
    _QPath: _test_QPath(),
    _QGraph: _test_QGraph(),
    _InvalidQualifiersError: _test_InvalidQualifiersError(),
    _MissingQueryGraphError: _test_MissingQueryGraphError()
  },
  skip: {
    load_trapi: true,
    AuxGraphNotFoundError: true,
    EdgeBindingNotFoundError: true,
    CONSTANTS: true
  }
};

function _test_client_request_to_trapi_query() {
  return test.make_function_test({
    "drug--treats-->disease": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "drug",
          "curie": "MONDO:123",
          "direction": null
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "ids": ["MONDO:123"],
                "categories": ["biolink:Disease"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "inferred",
                "predicates": ["biolink:treats"]
              }
            }
          }
        }
      }
    },
    "gene--increased_by-->chemical": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "gene",
          "curie": "CHEBI:123",
          "direction": "increased"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "ids": ["CHEBI:123"],
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "categories": ["biolink:Gene"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "inferred",
                "predicates": ["biolink:affects"],
                "qualifier_constraints": [
                  {
                    "qualifier_set": [
                      {
                        "qualifier_type_id": "biolink:qualified_predicate",
                        "qualifier_value": "biolink:causes"
                      },
                      {
                        "qualifier_type_id": "biolink:object_aspect_qualifier",
                        "qualifier_value": "activity_or_abundance"
                      },
                      {
                        "qualifier_type_id": "biolink:object_direction_qualifier",
                        "qualifier_value": "increased"
                      }
                    ]
                  }
                ]
              }
            }
          }
        }
      }
    },
    "gene--decreased_by-->chemical": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "gene",
          "curie": "CHEBI:123",
          "direction": "decreased"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "ids": ["CHEBI:123"],
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "categories": ["biolink:Gene"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "inferred",
                "predicates": ["biolink:affects"],
                "qualifier_constraints": [
                  {
                    "qualifier_set": [
                      {
                        "qualifier_type_id": "biolink:qualified_predicate",
                        "qualifier_value": "biolink:causes"
                      },
                      {
                        "qualifier_type_id": "biolink:object_aspect_qualifier",
                        "qualifier_value": "activity_or_abundance"
                      },
                      {
                        "qualifier_type_id": "biolink:object_direction_qualifier",
                        "qualifier_value": "decreased"
                      }
                    ]
                  }
                ]
              }
            }
          }
        }
      }
    },
    "chemical--increases-->gene": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "chemical",
          "curie": "NCBIGene:123",
          "direction": "increased"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "ids": ["NCBIGene:123"],
                "categories": ["biolink:Gene"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "inferred",
                "predicates": ["biolink:affects"],
                "qualifier_constraints": [
                  {
                    "qualifier_set": [
                      {
                        "qualifier_type_id": "biolink:qualified_predicate",
                        "qualifier_value": "biolink:causes"
                      },
                      {
                        "qualifier_type_id": "biolink:object_aspect_qualifier",
                        "qualifier_value": "activity_or_abundance"
                      },
                      {
                        "qualifier_type_id": "biolink:object_direction_qualifier",
                        "qualifier_value": "increased"
                      }
                    ]
                  }
                ]
              }
            }
          }
        }
      }
    },
    "chemical--decreases-->gene": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "chemical",
          "curie": "NCBIGene:123",
          "direction": "decreased"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "ids": ["NCBIGene:123"],
                "categories": ["biolink:Gene"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "inferred",
                "predicates": ["biolink:affects"],
                "qualifier_constraints": [
                  {
                    "qualifier_set": [
                      {
                        "qualifier_type_id": "biolink:qualified_predicate",
                        "qualifier_value": "biolink:causes"
                      },
                      {
                        "qualifier_type_id": "biolink:object_aspect_qualifier",
                        "qualifier_value": "activity_or_abundance"
                      },
                      {
                        "qualifier_type_id": "biolink:object_direction_qualifier",
                        "qualifier_value": "decreased"
                      }
                    ]
                  }
                ]
              }
            }
          }
        }
      }
    },
    "chemicals--related_to-->anything--related_to-->diseases": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "pathfinder",
          "subject": {
            "id": "CHEBI:31690",
            "category": "biolink:ChemicalEntity"
          },
          "object": {
            "id": "MONDO:0004784",
            "category": "biolink:Disease"
          }
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "ids": ["CHEBI:31690"],
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "ids": ["MONDO:0004784"],
                "categories": ["biolink:Disease"]
              }
            },
            "paths": {
              "*0": {
                "subject": "sn",
                "object": "on"
              }
            }
          }
        }
      }
    },
    "chemicals--related_to-->genes--related_to-->diseases": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "pathfinder",
          "subject": {
            "id": "CHEBI:31690",
            "category": "biolink:ChemicalEntity"
          },
          "object": {
            "id": "MONDO:0004784",
            "category": "biolink:Disease"
          },
          "constraint": "biolink:Gene"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "ids": ["CHEBI:31690"],
                "categories": ["biolink:ChemicalEntity"]
              },
              "on": {
                "ids": ["MONDO:0004784"],
                "categories": ["biolink:Disease"]
              }
            },
            "paths": {
              "*0": {
                "subject": "sn",
                "object": "on",
                "constraints": [
                  {
                    "intermediate_categories": ["biolink:Gene"]
                  }
                ]
              }
            }
          }
        }
      }
    },
    "lookup--related_to-->chemical": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "subject": {
            "id": "MONDO:123",
            "category": "Disease"
          },
          "object": {
            "category": "ChemicalEntity"
          },
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": {
        "message": {
          "query_graph": {
            "nodes": {
              "sn": {
                "ids": ["MONDO:123"],
                "categories": ["biolink:Disease"]
              },
              "on": {
                "categories": ["biolink:ChemicalEntity"]
              }
            },
            "edges": {
              "*": {
                "subject": "sn",
                "object": "on",
                "knowledge_type": "lookup",
                "predicates": ["biolink:related_to"]
              }
            }
          }
        }
      }
    },
    "lookup_missing_subject": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "object": {
            "category": "ChemicalEntity"
          },
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": TypeError
    },
    "lookup_missing_subject_id": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "subject": {
            "category": "Disease"
          },
          "object": {
            "category": "ChemicalEntity"
          },
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": TypeError
    },
    "lookup_missing_subject_category": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "subject": {
            "id": "MONDO:123"
          },
          "object": {
            "category": "ChemicalEntity"
          },
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": TypeError
    },
    "lookup_missing_object": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "subject": {
            "id": "MONDO:123",
            "category": "Disease"
          },
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": TypeError
    },
    "lookup_missing_object_category": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "type": "lookup",
          "subject": {
            "id": "MONDO:123",
            "category": "Disease"
          },
          "object": {},
          "node_one_label": "type 2 diabetes mellitus"
        }
      ],
      "expected": TypeError
    }
  });
}

function _test_get_pk() {
  return test.make_function_test({
    valid_message: {
      args: [{"pk": "123"}],
      expected: "123"
    },
    invalid_message: {
      args: [{"ok": "123"}],
      expected: false
    }
  });
}

function _test_get_results() {
  return test.make_function_test({
    "valid_results": {
      "args": [
        {
          "message": {
            "results": [
              {
                "node_bindings": {
                  "on": [
                    {
                      "id": "NCBIGene:6323",
                      "query_id": "NCBIGene:6323",
                      "attributes": []
                    }
                  ],
                  "sn": [
                    {
                      "id": "CHEBI:15420",
                      "query_id": null,
                      "attributes": []
                    }
                  ]
                },
                "analyses": [
                  {
                    "resource_id": "infores:unsecret-agent",
                    "edge_bindings": {
                      "t_edge": [
                        {
                          "id": "medik:creative_edge#56",
                          "attributes": []
                        },
                        {
                          "id": "medik:creative_edge#7",
                          "attributes": []
                        }
                      ]
                    },
                    "score": 1,
                    "support_graphs": null,
                    "scoring_method": null,
                    "attributes": null
                  }
                ],
                "ordering_components": {
                  "confidence": 1,
                  "clinical_evidence": 0,
                  "novelty": 0
                },
                "weighted_mean": 0.47619047619047616,
                "sugeno": 1,
                "rank": 2,
                "normalized_score": 100
              }
            ]
          }
        }
      ],
      "expected": [
        {
          "node_bindings": {
            "on": [
              {
                "id": "NCBIGene:6323",
                "query_id": "NCBIGene:6323",
                "attributes": []
              }
            ],
            "sn": [
              {
                "id": "CHEBI:15420",
                "query_id": null,
                "attributes": []
              }
            ]
          },
          "analyses": [
            {
              "resource_id": "infores:unsecret-agent",
              "edge_bindings": {
                "t_edge": [
                  {
                    "id": "medik:creative_edge#56",
                    "attributes": []
                  },
                  {
                    "id": "medik:creative_edge#7",
                    "attributes": []
                  }
                ]
              },
              "score": 1,
              "support_graphs": null,
              "scoring_method": null,
              "attributes": null
            }
          ],
          "ordering_components": {
            "confidence": 1,
            "clinical_evidence": 0,
            "novelty": 0
          },
          "weighted_mean": 0.47619047619047616,
          "sugeno": 1,
          "rank": 2,
          "normalized_score": 100
        }
      ]
    },
    "no_results": {
      "args": [
        {
          "message": {}
        }
      ],
      "expected": false
    }
  });
}

function _test_get_auxiliary_graphs() {
  return test.make_function_test({
    "valid_aux_graphs": {
      "args": [
        {
          "message": {
            "auxiliary_graphs": {
              "OMNICORP_support_graph_0": {
                "edges": [
                  "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
                ],
                "attributes": []
              },
              "OMNICORP_support_graph_1": {
                "edges": [
                  "42937cbf-c2a2-462e-8bb2-f1fc90d6b000"
                ],
                "attributes": []
              },
              "medik:auxiliary_graph#0": {
                "attributes": [],
                "edges": [
                  "medik:edge#1",
                  "medik:edge#2"
                ]
              }
            }
          }
        }
      ],
      "expected": {
        "OMNICORP_support_graph_0": {
          "edges": [
            "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
          ],
          "attributes": []
        },
        "OMNICORP_support_graph_1": {
          "edges": [
            "42937cbf-c2a2-462e-8bb2-f1fc90d6b000"
          ],
          "attributes": []
        },
        "medik:auxiliary_graph#0": {
          "attributes": [],
          "edges": [
            "medik:edge#1",
            "medik:edge#2"
          ]
        }
      }
    },
    "empty_aux_graphs": {
      "args": [
        {
          "message": {
            "auxiliary_graphs": {}
          }
        }
      ],
      "expected": {}
    },
    "no_aux_graphs": {
      "args": [
        {
          "message": {}
        }
      ],
      "expected": false
    }
  });
}

function _test_get_auxiliary_graph() {
  return test.make_function_test({
    "valid_aux_graph": {
      "args": [
        "OMNICORP_support_graph_0",
        {
          "OMNICORP_support_graph_0": {
            "edges": [
              "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
            ],
            "attributes": []
          }
        }
      ],
      "expected": {
        "edges": [
          "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
        ],
        "attributes": []
      }
    },
    "invalid_aux_graph": {
      "args": [
        "OMNICORP_support_graph_1",
        {
          "OMNICORP_support_graph_0": {
            "edges": [
              "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
            ],
            "attributes": []
          }
        }
      ],
      "expected": false
    }
  });
}

function _test_get_auxiliary_graph_edges() {
  return test.make_function_test({
    "valid_edges": {
      "args": [
        {
          "edges": [
            "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
          ],
          "attributes": []
        }
      ],
      "expected": [
        "b0f3be3d-d0fb-420c-9659-ee04db1a9ec9"
      ]
    },
    "no_edges": {
      "args": [
        {
          "edges": [],
          "attributes": []
        }
      ],
      "expected": []
    },
    "missing_edges": {
      "args": [
        {
          "attributes": []
        }
      ],
      "expected": []
    }
  });
}

function _test_get_edge_bindings() {
  return test.make_function_test({
    "valid_edge_bindings": {
      "args": [
        {
          "resource_id": "infores:unsecret-agent",
          "edge_bindings": {
            "t_edge": [
              {
                "id": "medik:creative_edge#56",
                "attributes": []
              },
              {
                "id": "medik:creative_edge#7",
                "attributes": []
              }
            ]
          },
          "score": 1,
          "support_graphs": null,
          "scoring_method": null,
          "attributes": null
        }
      ],
      "expected": {
        "t_edge": [
          {
            "id": "medik:creative_edge#56",
            "attributes": []
          },
          {
            "id": "medik:creative_edge#7",
            "attributes": []
          }
        ]
      }
    },
    "empty_edge_bindings": {
      "args": [
        {
          "resource_id": "infores:unsecret-agent",
          "edge_bindings": {},
          "score": 1,
          "support_graphs": null,
          "scoring_method": null,
          "attributes": null
        }
      ],
      "expected": {}
    },
    "missing_edge_bindings": {
      "args": [
        {
          "resource_id": "infores:unsecret-agent",
          "score": 1,
          "support_graphs": null,
          "scoring_method": null,
          "attributes": null
        }
      ],
      "expected": {}
    }
  });
}

function _test_get_node_bindings() {
  return test.make_function_test({
    "valid_node_bindings": {
      "args": [
        {
          "node_bindings": {
            "on": [
              {
                "id": "NCBIGene:6323",
                "query_id": "NCBIGene:6323",
                "attributes": []
              }
            ],
            "sn": [
              {
                "id": "CHEBI:15420",
                "query_id": null,
                "attributes": []
              }
            ]
          },
          "analyses": [
            {
              "resource_id": "infores:unsecret-agent",
              "edge_bindings": {
                "t_edge": [
                  {
                    "id": "medik:creative_edge#56",
                    "attributes": []
                  },
                  {
                    "id": "medik:creative_edge#7",
                    "attributes": []
                  }
                ]
              },
              "score": 1,
              "support_graphs": null,
              "scoring_method": null,
              "attributes": null
            }
          ],
          "ordering_components": {
            "confidence": 1,
            "clinical_evidence": 0,
            "novelty": 0
          },
          "weighted_mean": 0.47619047619047616,
          "sugeno": 1,
          "rank": 2,
          "normalized_score": 100
        },
        "sn"
      ],
      "expected": [
        {
          "id": "CHEBI:15420",
          "query_id": null,
          "attributes": []
        }
      ]
    },
    "no_node_bindings": {
      "args": [
        {
          "node_bindings": {},
          "analyses": [
            {
              "resource_id": "infores:unsecret-agent",
              "edge_bindings": {
                "t_edge": [
                  {
                    "id": "medik:creative_edge#56",
                    "attributes": []
                  },
                  {
                    "id": "medik:creative_edge#7",
                    "attributes": []
                  }
                ]
              },
              "score": 1,
              "support_graphs": null,
              "scoring_method": null,
              "attributes": null
            }
          ],
          "ordering_components": {
            "confidence": 1,
            "clinical_evidence": 0,
            "novelty": 0
          },
          "weighted_mean": 0.47619047619047616,
          "sugeno": 1,
          "rank": 2,
          "normalized_score": 100
        },
        "sn"
      ],
      "expected": []
    },
    "missing_node_bindings": {
      "args": [
        {
          "analyses": [
            {
              "resource_id": "infores:unsecret-agent",
              "edge_bindings": {
                "t_edge": [
                  {
                    "id": "medik:creative_edge#56",
                    "attributes": []
                  },
                  {
                    "id": "medik:creative_edge#7",
                    "attributes": []
                  }
                ]
              },
              "score": 1,
              "support_graphs": null,
              "scoring_method": null,
              "attributes": null
            }
          ],
          "ordering_components": {
            "confidence": 1,
            "clinical_evidence": 0,
            "novelty": 0
          },
          "weighted_mean": 0.47619047619047616,
          "sugeno": 1,
          "rank": 2,
          "normalized_score": 100
        },
        "sn"
      ],
      "expected": []
    }
  });
}

function _test_get_path_bindings() {
  const path_binding = {'test-path-binding': 123};
  return test.make_function_test({
    valid_path_bindings: {
      args: [{path_bindings: path_binding}],
      expected: path_binding
    },
    empty_path_bindings: {
      args: [{path_bindings: {}}],
      expected: {}
    },
    no_path_bindings: {
      args: [{}],
      expected: {}
    }
  });
}

function _test_get_binding_id() {
  return test.make_function_test({
    node_binding: {
      args: [{id: 'CHEBI:15420', attributes: []}],
      expected: 'CHEBI:15420'
    },
    edge_binding: {
      args: [{id: 'medik:creative_edge#56', attributes: []}],
      expected: 'medik:creative_edge#56'
    },
    missing_id: {
      args: [{attributes: []}],
      expected: ReferenceError
    }
  });
}

function _test_get_analyses() {
  const analysis_list = [{resource_id: 'infores:unsecret-agent', edge_bindings: {}}];
  return test.make_function_test({
    valid_analyses: {
      args: [{analyses: analysis_list}],
      expected: analysis_list
    },
    empty_analyses: {
      args: [{analyses: []}],
      expected: []
    },
    missing_analyses: {
      args: [{}],
      expected: ReferenceError
    }
  });
}

function _test_get_resource_id() {
  return test.make_function_test({
    valid_resource_id: {
      args: [{resource_id: 'infores:unsecret-agent'}],
      expected: 'infores:unsecret-agent'
    },
    missing_resource_id: {
      args: [{}],
      expected: false
    }
  });
}

function _test_get_analysis_score() {
  return test.make_function_test({
    valid_score: {
      args: [{score: 0.75}],
      expected: 0.75
    },
    zero_score: {
      args: [{score: 0}],
      expected: 0
    },
    missing_score: {
      args: [{}],
      expected: 0.0
    }
  });
}

function _test_get_ordering_components() {
  return test.make_function_test({
    valid_ordering_components: {
      args: [{ordering_components: {confidence: 1, novelty: 0.5, clinical_evidence: 0}}],
      expected: {confidence: 1, novelty: 0.5, clinical_evidence: 0}
    },
    missing_ordering_components: {
      args: [{}],
      expected: {confidence: 0, novelty: 0, clinical_evidence: 0}
    }
  });
}

function _test_get_normalized_score() {
  return test.make_function_test({
    valid_normalized_score: {
      args: [{normalized_score: 100}],
      expected: 100
    },
    missing_normalized_score: {
      args: [{}],
      expected: 0
    }
  });
}

function _test_get_kgraph() {
  return test.make_function_test({
    "valid_kgraph": {
      "args": [
        {
          "message": {
            "knowledge_graph": {
              "nodes": {
                "MONDO:123": {},
                "CHEBI:123": {}
              },
              "edges": {
                "test-edge": {
                  "subject": "CHEBI:123",
                  "object": "MONDO:123",
                  "predicate": "biolink:treats",
                  "attributes": []
                }
              }
            }
          }
        }
      ],
      "expected": {
        "nodes": {
          "MONDO:123": {},
          "CHEBI:123": {}
        },
        "edges": {
          "test-edge": {
            "subject": "CHEBI:123",
            "object": "MONDO:123",
            "predicate": "biolink:treats",
            "attributes": []
          }
        }
      }
    }
  });
}

function _test_get_kedge() {
  return test.make_function_test({
    "valid-kedge": {
      "args": [
        "test-edge",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": {
        "subject": "CHEBI:123",
        "object": "MONDO:123",
        "predicate": "biolink:treats",
        "attributes": []
      }
    },
    "invalid-kedge": {
      "args": [
        "test-invalid-edge",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": null
    },
    "missing-kedges": {
      "args": [
        "test-edge",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          }
        }
      ],
      "expected": null
    }
  });
}

function _test_get_knode() {
  return test.make_function_test({
    "valid_knode": {
      "args": [
        "MONDO:123",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": {}
    },
    "invalid_knode": {
      "args": [
        "MONDO:1234",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": null
    },
    "missing-knodes": {
      "args": [
        "MONDO:123",
        {
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": null
    }
  });
}

function _test_has_knode() {
  return test.make_function_test({
    "valid_knode": {
      "args": [
        "MONDO:123",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": true
    },
    "invalid_knode": {
      "args": [
        "MONDO:1234",
        {
          "nodes": {
            "MONDO:123": {},
            "CHEBI:123": {}
          },
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": false
    },
    "invalid_knode": {
      "args": [
        "MONDO:123",
        {
          "edges": {
            "test-edge": {
              "subject": "CHEBI:123",
              "object": "MONDO:123",
              "predicate": "biolink:treats",
              "attributes": []
            }
          }
        }
      ],
      "expected": false
    }
  });
}

function _test_get_attrs() {
  return test.make_function_test({
    "no_attributes": {
      "args": [
        {
          "attributes": []
        }
      ],
      "expected": []
    },
    "missing_attributes": {
      "args": [
        {}
      ],
      "expected": []
    },
    "valid_attributes": {
      "args": [
        {
          "categories": [
            "biolink:ChemicalEntityOrProteinOrPolypeptide",
            "biolink:NamedThing",
            "biolink:ChemicalEntity",
            "biolink:ChemicalOrDrugOrTreatment",
            "biolink:ChemicalEntityOrGeneOrGeneProduct",
            "biolink:PhysicalEssenceOrOccurrent",
            "biolink:MolecularEntity",
            "biolink:PhysicalEssence",
            "biolink:SmallMolecule"
          ],
          "name": "cangitoxin II",
          "attributes": [
            {
              "attribute_type_id": "biolink:has_count",
              "value": 0,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "omnicorp_article_count"
            },
            {
              "attribute_type_id": "biolink:same_as",
              "value": [
                "GTOPDB:7568"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "equivalent_identifiers"
            },
            {
              "value": 0,
              "value_url": null,
              "attributes": null,
              "description": null,
              "value_type_id": "EDAM:data_0006",
              "attribute_source": null,
              "attribute_type_id": "biolink:has_count",
              "original_attribute_name": "omnicorp_article_count"
            },
            {
              "value": [
                "GTOPDB:7568"
              ],
              "value_url": null,
              "attributes": null,
              "description": null,
              "value_type_id": "linkml:Uriorcurie",
              "attribute_source": null,
              "attribute_type_id": "biolink:same_as",
              "original_attribute_name": "equivalent_identifiers"
            }
          ]
        }
      ],
      "expected": [
        {
          "attribute_type_id": "biolink:has_count",
          "value": 0,
          "value_type_id": "EDAM:data_0006",
          "original_attribute_name": "omnicorp_article_count"
        },
        {
          "attribute_type_id": "biolink:same_as",
          "value": [
            "GTOPDB:7568"
          ],
          "value_type_id": "linkml:Uriorcurie",
          "original_attribute_name": "equivalent_identifiers"
        },
        {
          "value": 0,
          "value_url": null,
          "attributes": null,
          "description": null,
          "value_type_id": "EDAM:data_0006",
          "attribute_source": null,
          "attribute_type_id": "biolink:has_count",
          "original_attribute_name": "omnicorp_article_count"
        },
        {
          "value": [
            "GTOPDB:7568"
          ],
          "value_url": null,
          "attributes": null,
          "description": null,
          "value_type_id": "linkml:Uriorcurie",
          "attribute_source": null,
          "attribute_type_id": "biolink:same_as",
          "original_attribute_name": "equivalent_identifiers"
        }
      ]
    }
  });
}

function _test_get_attr_id() {
  return test.make_function_test({
    "valid_attr_id": {
      "args": [
        {
          "attribute_type_id": "biolink:has_count",
          "value": 0,
          "value_type_id": "EDAM:data_0006",
          "original_attribute_name": "omnicorp_article_count"
        }
      ],
      "expected": "biolink:has_count"
    }
  });
}

function _test_get_attr_val() {
  return test.make_function_test({
    "valid_attr_val": {
      "args": [
        {
          "attribute_type_id": "biolink:has_count",
          "value": 0,
          "value_type_id": "EDAM:data_0006",
          "original_attribute_name": "omnicorp_article_count"
        }
      ],
      "expected": 0
    }
  });
}

function _test_get_primary_source() {
  return test.make_function_test({
    "only_primary_source": {
      "args": [
        {
          "subject": "PUBCHEM.COMPOUND:91666633",
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "sources": [
            {
              "resource_id": "infores:gtopdb",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity"
            }
          ],
          "attributes": [
            {
              "attribute_type_id": "biolink:Attribute",
              "value": "pIC50",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity_parameter"
            },
            {
              "attribute_type_id": "biolink:knowledge_level",
              "value": "knowledge_assertion",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "knowledge_level"
            },
            {
              "attribute_type_id": "aragorn:endogenous",
              "value": false,
              "value_type_id": "xsd:boolean",
              "original_attribute_name": "endogenous"
            },
            {
              "attribute_type_id": "biolink:publications",
              "value": [
                "PMID:29737846"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "publications"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": false,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "primaryTarget"
            },
            {
              "attribute_type_id": "biolink:agent_type",
              "value": "manual_agent",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "agent_type"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": 7.349999904632568,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity"
            }
          ]
        }
      ],
      "expected": {
        "infores": "infores:gtopdb",
        "records": []
      }
    },
    "aggregator_sources": {
      "args": [
        {
          "subject": "PUBCHEM.COMPOUND:91666633",
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "sources": [
            {
              "resource_id": "infores:automat-gtopdb",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:gtopdb",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            },
            {
              "resource_id": "infores:automat-robokop",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:aragorn",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:automat-gtopdb"
              ]
            }
          ],
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity"
            }
          ],
          "attributes": [
            {
              "attribute_type_id": "biolink:Attribute",
              "value": "pIC50",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity_parameter"
            },
            {
              "attribute_type_id": "biolink:knowledge_level",
              "value": "knowledge_assertion",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "knowledge_level"
            },
            {
              "attribute_type_id": "aragorn:endogenous",
              "value": false,
              "value_type_id": "xsd:boolean",
              "original_attribute_name": "endogenous"
            },
            {
              "attribute_type_id": "biolink:publications",
              "value": [
                "PMID:29737846"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "publications"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": false,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "primaryTarget"
            },
            {
              "attribute_type_id": "biolink:agent_type",
              "value": "manual_agent",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "agent_type"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": 7.349999904632568,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity"
            }
          ]
        }
      ],
      "expected": {
        "infores": "infores:gtopdb",
        "records": []
      }
    }
  });
}

function _test_get_subject() {
  return test.make_function_test({
    "valid_subject": {
      "args": [
        {
          "subject": "PUBCHEM.COMPOUND:91666633",
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "sources": [
            {
              "resource_id": "infores:automat-gtopdb",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:gtopdb",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            },
            {
              "resource_id": "infores:automat-robokop",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:aragorn",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:automat-gtopdb"
              ]
            }
          ],
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity"
            }
          ],
          "attributes": [
            {
              "attribute_type_id": "biolink:Attribute",
              "value": "pIC50",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity_parameter"
            },
            {
              "attribute_type_id": "biolink:knowledge_level",
              "value": "knowledge_assertion",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "knowledge_level"
            },
            {
              "attribute_type_id": "aragorn:endogenous",
              "value": false,
              "value_type_id": "xsd:boolean",
              "original_attribute_name": "endogenous"
            },
            {
              "attribute_type_id": "biolink:publications",
              "value": [
                "PMID:29737846"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "publications"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": false,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "primaryTarget"
            },
            {
              "attribute_type_id": "biolink:agent_type",
              "value": "manual_agent",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "agent_type"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": 7.349999904632568,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity"
            }
          ]
        }
      ],
      "expected": "PUBCHEM.COMPOUND:91666633"
    }
  });
}

function _test_get_object() {
  return test.make_function_test({
    "valid_object": {
      "args": [
        {
          "subject": "PUBCHEM.COMPOUND:91666633",
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "sources": [
            {
              "resource_id": "infores:automat-gtopdb",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:gtopdb",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            },
            {
              "resource_id": "infores:automat-robokop",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:gtopdb"
              ]
            },
            {
              "resource_id": "infores:aragorn",
              "resource_role": "aggregator_knowledge_source",
              "upstream_resource_ids": [
                "infores:automat-gtopdb"
              ]
            }
          ],
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity"
            }
          ],
          "attributes": [
            {
              "attribute_type_id": "biolink:Attribute",
              "value": "pIC50",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity_parameter"
            },
            {
              "attribute_type_id": "biolink:knowledge_level",
              "value": "knowledge_assertion",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "knowledge_level"
            },
            {
              "attribute_type_id": "aragorn:endogenous",
              "value": false,
              "value_type_id": "xsd:boolean",
              "original_attribute_name": "endogenous"
            },
            {
              "attribute_type_id": "biolink:publications",
              "value": [
                "PMID:29737846"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "publications"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": false,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "primaryTarget"
            },
            {
              "attribute_type_id": "biolink:agent_type",
              "value": "manual_agent",
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "agent_type"
            },
            {
              "attribute_type_id": "biolink:Attribute",
              "value": 7.349999904632568,
              "value_type_id": "EDAM:data_0006",
              "original_attribute_name": "affinity"
            }
          ]
        }
      ],
      "expected": "NCBIGene:6323"
    }
  });
}

function _test_get_predicate() {
  return test.make_function_test({
    predicate_exists: {
      args: [{predicate: "test_predicate"}],
      expected: "test_predicate"
    },
    no_predicate: {
      args: [{}],
      expected: ReferenceError
    }
  });
}

function _test_get_support_graphs() {
  return test.make_function_test({
    "valid_support_graphs": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": [
        "medik:auxiliary_graph#0"
      ]
    },
    "no_support_graphs": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": []
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": []
    },
    "missing_support_graphs": {
      "args": [
        {
          "attributes": [
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": []
    }
  });
}

function _test_get_qualifiers() {
  return test.make_function_test({
    "valid_qualifiers": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": [
        {
          "qualifier_type_id": "biolink:qualified_predicate",
          "qualifier_value": "biolink:causes"
        },
        {
          "qualifier_type_id": "biolink:qualified_predicate",
          "qualifier_value": "biolink:causes"
        },
        {
          "qualifier_type_id": "biolink:object_aspect_qualifier",
          "qualifier_value": "activity_or_abundance"
        },
        {
          "qualifier_type_id": "biolink:object_direction_qualifier",
          "qualifier_value": "decreased"
        }
      ]
    },
    "no_qualifiers": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": []
    },
    "missing_qualifiers": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": []
    }
  });
}

function _test_get_qualifier_id() {
  return test.make_function_test({
    "valid_qualifier_id": {
      "args": [
        {
          "qualifier_type_id": "biolink:qualified_predicate",
          "qualifier_value": "biolink:causes"
        }
      ],
      "expected": "biolink:qualified_predicate"
    },
    "missing_qualifier_id": {
      "args": [
        {
          "qualifier_value": "biolink:causes"
        }
      ],
      "expected": false
    }
  });
}

function _test_get_qualifier_val() {
  return test.make_function_test({
    "valid_qualifier_id": {
      "args": [
        {
          "qualifier_type_id": "biolink:qualified_predicate",
          "qualifier_value": "biolink:causes"
        }
      ],
      "expected": "biolink:causes"
    }
  });
}

function _test_get_knowledge_level() {
  return test.make_function_test({
    "valid_knowledge_level": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": "prediction"
    },
    "missing_knowledge_level": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": null
    }
  });
}

function _test_get_agent_type() {
  return test.make_function_test({
    "valid_agent_type": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:agent_type",
              "value": "computational_model"
            },
            {
              "attribute_source": "infores:unsecret-agent",
              "attribute_type_id": "biolink:knowledge_level",
              "value": "prediction"
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": "computational_model"
    },
    "missing_agent_type": {
      "args": [
        {
          "attributes": [
            {
              "attribute_type_id": "biolink:support_graphs",
              "value": [
                "medik:auxiliary_graph#0"
              ]
            }
          ],
          "object": "NCBIGene:6323",
          "predicate": "biolink:affects",
          "qualifiers": [
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:qualified_predicate",
              "qualifier_value": "biolink:causes"
            },
            {
              "qualifier_type_id": "biolink:object_aspect_qualifier",
              "qualifier_value": "activity_or_abundance"
            },
            {
              "qualifier_type_id": "biolink:object_direction_qualifier",
              "qualifier_value": "decreased"
            }
          ],
          "sources": [
            {
              "resource_id": "infores:unsecret-agent",
              "resource_role": "primary_knowledge_source",
              "upstream_resource_ids": []
            }
          ],
          "subject": "CHEBI:6121"
        }
      ],
      "expected": null
    }
  });
}

function _test_get_edge_type() {
  return test.make_function_test({
    direct_edge: {
      args: [{
        attributes: [
          {
            attribute_type_id: "biolink:support_graphs",
            value: []
          }
        ]
      }],
      expected: CONSTANTS.GRAPH.EDGE.TYPE.DIRECT
    },
    indirect_edge: {
      args: [{
        attributes: [
          {
            attribute_type_id: "biolink:support_graphs",
            value: ["test-sgid"]
          }
        ]
      }],
      expected: CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT
    },
    no_support_graphs: {
      args: [{
        attributes: [
          {
            attribute_type_id: "test-attr-id",
            value: "test-attr-val"
          }
        ]
      }],
      expected: CONSTANTS.GRAPH.EDGE.TYPE.DIRECT
    },
    no_attributes: {
      args: [{}],
      expected: CONSTANTS.GRAPH.EDGE.TYPE.DIRECT
    },
  });
}

function _test_message_to_query_type() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "categories": [
                    "biolink:ChemicalEntity"
                  ],
                  "set_interpretation": "BATCH",
                  "constraints": []
                },
                "on": {
                  "ids": [
                    "NCBIGene:6323"
                  ],
                  "categories": [
                    "biolink:Gene"
                  ],
                  "set_interpretation": "BATCH",
                  "constraints": []
                }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "knowledge_type": "inferred",
                  "predicates": [
                    "biolink:affects"
                  ],
                  "attribute_constraints": [],
                  "qualifier_constraints": [
                    {
                      "qualifier_set": [
                        {
                          "qualifier_type_id": "biolink:qualified_predicate",
                          "qualifier_value": "biolink:causes"
                        },
                        {
                          "qualifier_type_id": "biolink:object_aspect_qualifier",
                          "qualifier_value": "activity_or_abundance"
                        },
                        {
                          "qualifier_type_id": "biolink:object_direction_qualifier",
                          "qualifier_value": "decreased"
                        }
                      ]
                    }
                  ]
                }
              }
            }
          }
        }
      ],
      "expected": 0
    },
    "chemical--treats->disease": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "categories": [
                    "biolink:ChemicalEntity"
                  ]
                },
                "on": {
                  "categories": [
                    "biolink:Disease"
                  ],
                  "ids": [
                    "MONDO:0008029"
                  ]
                }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": [
                    "biolink:treats"
                  ],
                  "knowledge_type": "inferred"
                }
              }
            }
          }
        }
      ],
      "expected": 1
    },
    "gene--affected_by->chemical": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": [
                    "PUBCHEM.COMPOUND:4946"
                  ],
                  "categories": [
                    "biolink:ChemicalEntity"
                  ],
                  "is_set": false,
                  "set_interpretation": "BATCH",
                  "set_id": null,
                  "constraints": [],
                  "option_group_id": null
                },
                "on": {
                  "ids": null,
                  "categories": [
                    "biolink:Gene"
                  ],
                  "is_set": false,
                  "set_interpretation": "BATCH",
                  "set_id": null,
                  "constraints": [],
                  "option_group_id": null
                }
              },
              "edges": {
                "t_edge": {
                  "knowledge_type": "inferred",
                  "predicates": [
                    "biolink:affects"
                  ],
                  "subject": "sn",
                  "object": "on",
                  "attribute_constraints": [],
                  "qualifier_constraints": [
                    {
                      "qualifier_set": [
                        {
                          "qualifier_type_id": "biolink:qualified_predicate",
                          "qualifier_value": "biolink:causes"
                        },
                        {
                          "qualifier_type_id": "biolink:object_aspect_qualifier",
                          "qualifier_value": "activity_or_abundance"
                        },
                        {
                          "qualifier_type_id": "biolink:object_direction_qualifier",
                          "qualifier_value": "increased"
                        }
                      ]
                    }
                  ],
                  "exclude": null,
                  "option_group_id": null
                }
              }
            }
          }
        }
      ],
      "expected": 2
    },
    "pathfinder": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": [
                    "CHEBI:5931"
                  ],
                  "categories": [
                    "biolink:ChemicalEntity"
                  ]
                },
                "on": {
                  "ids": [
                    "MONDO:0005015"
                  ],
                  "categories": [
                    "biolink:Disease"
                  ]
                }
              },
              "paths": {
                "p0": {
                  "subject": "sn",
                  "object": "on"
                }
              }
            }
          }
        }
      ],
      "expected": 3
    },
    "lookup_over_creative_category_pair": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": ["MONDO:123"],
                  "categories": ["biolink:Disease"]
                },
                "on": {
                  "categories": ["biolink:ChemicalEntity"]
                }
              },
              "edges": {
                "e0": {
                  "subject": "sn",
                  "object": "on",
                  "knowledge_type": "lookup",
                  "predicates": ["biolink:related_to"]
                }
              }
            }
          }
        }
      ],
      "expected": 4
    },
    "lookup_over_unsupported_category_pair": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": ["MONDO:123"],
                  "categories": ["biolink:Disease"]
                },
                "on": {
                  "categories": ["biolink:Disease"]
                }
              },
              "edges": {
                "e0": {
                  "subject": "sn",
                  "object": "on",
                  "knowledge_type": "lookup",
                  "predicates": ["biolink:related_to"]
                }
              }
            }
          }
        }
      ],
      "expected": 4
    },
    "omitted_knowledge_type_defaults_to_lookup": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "categories": ["biolink:ChemicalEntity"]
                },
                "on": {
                  "ids": ["NCBIGene:6323"],
                  "categories": ["biolink:Gene"]
                }
              },
              "edges": {
                "e0": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": ["biolink:affects"]
                }
              }
            }
          }
        }
      ],
      "expected": 4
    }
  });
}

function _test_message_to_endpoints() {
  return test.make_function_test({
    "chemical--affects->gene": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "categories": [
                    "biolink:ChemicalEntity"
                  ],
                  "set_interpretation": "BATCH",
                  "constraints": []
                },
                "on": {
                  "ids": [
                    "NCBIGene:6323"
                  ],
                  "categories": [
                    "biolink:Gene"
                  ],
                  "set_interpretation": "BATCH",
                  "constraints": []
                }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "knowledge_type": "inferred",
                  "predicates": [
                    "biolink:affects"
                  ],
                  "attribute_constraints": [],
                  "qualifier_constraints": [
                    {
                      "qualifier_set": [
                        {
                          "qualifier_type_id": "biolink:qualified_predicate",
                          "qualifier_value": "biolink:causes"
                        },
                        {
                          "qualifier_type_id": "biolink:object_aspect_qualifier",
                          "qualifier_value": "activity_or_abundance"
                        },
                        {
                          "qualifier_type_id": "biolink:object_direction_qualifier",
                          "qualifier_value": "decreased"
                        }
                      ]
                    }
                  ]
                }
              }
            }
          }
        }
      ],
      "expected": ["sn", "on"]
    },
    "chemical--treats->disease": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "categories": [
                    "biolink:ChemicalEntity"
                  ]
                },
                "on": {
                  "categories": [
                    "biolink:Disease"
                  ],
                  "ids": [
                    "MONDO:0008029"
                  ]
                }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": [
                    "biolink:treats"
                  ],
                  "knowledge_type": "inferred"
                }
              }
            }
          }
        }
      ],
      "expected": ["sn", "on"]
    },
    "gene--affected_by->chemical": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": [
                    "PUBCHEM.COMPOUND:4946"
                  ],
                  "categories": [
                    "biolink:ChemicalEntity"
                  ],
                  "is_set": false,
                  "set_interpretation": "BATCH",
                  "set_id": null,
                  "constraints": [],
                  "option_group_id": null
                },
                "on": {
                  "ids": null,
                  "categories": [
                    "biolink:Gene"
                  ],
                  "is_set": false,
                  "set_interpretation": "BATCH",
                  "set_id": null,
                  "constraints": [],
                  "option_group_id": null
                }
              },
              "edges": {
                "t_edge": {
                  "knowledge_type": "inferred",
                  "predicates": [
                    "biolink:affects"
                  ],
                  "subject": "sn",
                  "object": "on",
                  "attribute_constraints": [],
                  "qualifier_constraints": [
                    {
                      "qualifier_set": [
                        {
                          "qualifier_type_id": "biolink:qualified_predicate",
                          "qualifier_value": "biolink:causes"
                        },
                        {
                          "qualifier_type_id": "biolink:object_aspect_qualifier",
                          "qualifier_value": "activity_or_abundance"
                        },
                        {
                          "qualifier_type_id": "biolink:object_direction_qualifier",
                          "qualifier_value": "increased"
                        }
                      ]
                    }
                  ],
                  "exclude": null,
                  "option_group_id": null
                }
              }
            }
          }
        }
      ],
      "expected": ["on", "sn"]
    },
    "pathfinder": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": [
                    "CHEBI:5931"
                  ],
                  "categories": [
                    "biolink:SmallMolecule",
                    "biolink:ChemicalEntity"
                  ]
                },
                "on": {
                  "ids": [
                    "MONDO:0005015"
                  ],
                  "categories": [
                    "biolink:Disease"
                  ]
                }
              },
              "paths": {
                "p0": {
                  "subject": "sn",
                  "object": "on"
                }
              }
            }
          }
        }
      ],
      "expected": ["sn", "on"]
    },
    "lookup": {
      config_loader: () => load_trapi(_test_config),
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": {
                  "ids": ["MONDO:123"],
                  "categories": ["biolink:Disease"]
                },
                "on": {
                  "categories": ["biolink:ChemicalEntity"]
                }
              },
              "edges": {
                "e0": {
                  "subject": "sn",
                  "object": "on",
                  "knowledge_type": "lookup",
                  "predicates": ["biolink:related_to"]
                }
              }
            }
          }
        }
      ],
      "expected": ["on", "sn"]
    }
  });
}

function _test_is_chemical_disease_query() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        0
      ],
      "expected": false
    },
    "chemical--treats->disease": {
      "args": [
        1
      ],
      "expected": true
    },
    "gene--affected_by->chemical": {
      "args": [
        2
      ],
      "expected": false
    },
    "pathfinder": {
      "args": [
        3
      ],
      "expected": false
    }
  });
}

function _test_is_gene_chemical_query() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        0
      ],
      "expected": false
    },
    "chemical--treats->disease": {
      "args": [
        1
      ],
      "expected": false
    },
    "gene--affected_by->chemical": {
      "args": [
        2
      ],
      "expected": true
    },
    "pathfinder": {
      "args": [
        3
      ],
      "expected": false
    }
  });
}

function _test_is_pathfinder_query() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        0
      ],
      "expected": false
    },
    "chemical--treats->disease": {
      "args": [
        1
      ],
      "expected": false
    },
    "gene--affected_by->chemical": {
      "args": [
        2
      ],
      "expected": false
    },
    "pathfinder": {
      "args": [
        3
      ],
      "expected": true
    }
  });
}

function _test_is_lookup_query() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        0
      ],
      "expected": false
    },
    "chemical--treats->disease": {
      "args": [
        1
      ],
      "expected": false
    },
    "gene--affected_by->chemical": {
      "args": [
        2
      ],
      "expected": false
    },
    "pathfinder": {
      "args": [
        3
      ],
      "expected": false
    },
    "lookup": {
      "args": [
        4
      ],
      "expected": true
    }
  });
}

function _test_is_valid_query() {
  return test.make_function_test({
    "chemical--affects->gene": {
      "args": [
        0
      ],
      "expected": true
    },
    "chemical--treats->disease": {
      "args": [
        1
      ],
      "expected": true
    },
    "gene--affected_by->chemical": {
      "args": [
        2
      ],
      "expected": true
    },
    "pathfinder": {
      "args": [
        3
      ],
      "expected": true
    },
    "lookup": {
      "args": [
        4
      ],
      "expected": true
    },
    "invalid_template": {
      "args": [
        9999
      ],
      "expected": false
    }
  });
}

function _test_AttributeIterator() {
  return test.make_class_test({
    "empty_class_constructor": {
      "class_constructor": {
        "args": []
      },
      "steps": [
        {
          "method": "has_next",
          "args": [],
          "expected": false
        },
        {
          "method": "next",
          "args": [],
          "expected": RangeError
        },
        {
          "method": "find_one",
          "args": [
            ["test_attr_id"]
          ],
          "expected": null
        },
        {
          "method": "find_all",
          "args": [
            ["test_attr_id"]
          ],
          "expected": []
        }
      ]
    },
    "no_attributes": {
      "class_constructor": {
        "args": [[]]
      },
      "steps": [
        {
          "method": "has_next",
          "args": [],
          "expected": false
        },
        {
          "method": "next",
          "args": [],
          "expected": RangeError
        },
        {
          "method": "find_one",
          "args": [
            ["test_attr_id"]
          ],
          "expected": null
        },
        {
          "method": "find_all",
          "args": [
            ["test_attr_id"]
          ],
          "expected": []
        },
      ]
    },
    "finding_single_attributes": {
      "class_constructor": {
        "args": [
          {
            "attributes": [
              {
                "attribute_type_id": "biolink:has_count",
                "value": 0,
                "value_type_id": "EDAM:data_0006",
                "original_attribute_name": "omnicorp_article_count"
              },
              {
                "attribute_type_id": "biolink:same_as",
                "value": [
                  "GTOPDB:7568"
                ],
                "value_type_id": "linkml:Uriorcurie",
                "original_attribute_name": "equivalent_identifiers"
              },
              {
                "value": 0,
                "value_url": null,
                "attributes": null,
                "description": null,
                "value_type_id": "EDAM:data_0006",
                "attribute_source": null,
                "attribute_type_id": "biolink:has_count",
                "original_attribute_name": "omnicorp_article_count"
              },
              {
                "value": [
                  "GTOPDB:7568"
                ],
                "value_url": null,
                "attributes": null,
                "description": null,
                "value_type_id": "linkml:Uriorcurie",
                "attribute_source": null,
                "attribute_type_id": "biolink:same_as",
                "original_attribute_name": "equivalent_identifiers"
              }
            ]
          }
        ]
      },
      "steps": [
        {
          "method": "has_next",
          "args": [],
          "expected": true
        },
        {
          "method": "find_one",
          "args": [
            ["biolink:same_as"]
          ],
          "expected": {
            "attribute_type_id": "biolink:same_as",
            "value": [
              "GTOPDB:7568"
            ],
            "value_type_id": "linkml:Uriorcurie",
            "original_attribute_name": "equivalent_identifiers"
          }
        },
        {
          "method": "has_next",
          "args": [],
          "expected": true
        },
        {
          "method": "find_one",
          "args": [
            ["biolink:has_count"]
          ],
          "expected": {
            "value": 0,
            "value_url": null,
            "attributes": null,
            "description": null,
            "value_type_id": "EDAM:data_0006",
            "attribute_source": null,
            "attribute_type_id": "biolink:has_count",
            "original_attribute_name": "omnicorp_article_count"
          }
        },
        {
          "method": "has_next",
          "args": [],
          "expected": true
        },
        {
          "method": "find_one",
          "args": [
            ["biolink:has_count"]
          ],
          "expected": null
        },
        {
          "method": "has_next",
          "args": [],
          "expected": false
        }
      ]
    },
    "finding_all_attributes": {
      "class_constructor": {
        "args": [
          {
            "attributes": [
              {
                "attribute_type_id": "biolink:has_count",
                "value": 0,
                "value_type_id": "EDAM:data_0006",
                "original_attribute_name": "omnicorp_article_count"
              },
              {
                "attribute_type_id": "biolink:same_as",
                "value": [
                  "GTOPDB:7568"
                ],
                "value_type_id": "linkml:Uriorcurie",
                "original_attribute_name": "equivalent_identifiers"
              },
              {
                "value": 0,
                "value_url": null,
                "attributes": null,
                "description": null,
                "value_type_id": "EDAM:data_0006",
                "attribute_source": null,
                "attribute_type_id": "biolink:has_count",
                "original_attribute_name": "omnicorp_article_count"
              },
              {
                "value": [
                  "GTOPDB:7568"
                ],
                "value_url": null,
                "attributes": null,
                "description": null,
                "value_type_id": "linkml:Uriorcurie",
                "attribute_source": null,
                "attribute_type_id": "biolink:same_as",
                "original_attribute_name": "equivalent_identifiers"
              }
            ]
          }
        ]
      },
      "steps": [
        {
          "method": "find_all",
          "args": [
            ["biolink:same_as"]
          ],
          "expected": [
            {
              "attribute_type_id": "biolink:same_as",
              "value": [
                "GTOPDB:7568"
              ],
              "value_type_id": "linkml:Uriorcurie",
              "original_attribute_name": "equivalent_identifiers"
            },
            {
              "value": [
                "GTOPDB:7568"
              ],
              "value_url": null,
              "attributes": null,
              "description": null,
              "value_type_id": "linkml:Uriorcurie",
              "attribute_source": null,
              "attribute_type_id": "biolink:same_as",
              "original_attribute_name": "equivalent_identifiers"
            }
          ]
        },
        {
          "method": "has_next",
          "args": [],
          "expected": false
        }
      ]
    },
  });
}

const _test_config = {
  "query_subject_key": "sn",
  "query_object_key":  "on"
};

function _test_Query() {
  return test.make_class_test({
    "drug_request": {
      "class_constructor": {
        "args": [
          {
            "type": "drug",
            "curie": "MONDO:123",
            "direction": null
          }
        ],
        "expected": {
          "type": CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE,
          "curie": "MONDO:123",
          "direction": null,
          "subject": null,
          "object": null,
          "constraint": null
        }
      }
    },
    "gene_request": {
      "class_constructor": {
        "args": [
          {
            "type": "gene",
            "curie": "CHEBI:123",
            "direction": "increased"
          }
        ],
        "expected": {
          "type": CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL,
          "curie": "CHEBI:123",
          "direction": "increased",
          "subject": null,
          "object": null,
          "constraint": null
        }
      }
    },
    "chemical_request": {
      "class_constructor": {
        "args": [
          {
            "type": "chemical",
            "curie": "NCBIGene:123",
            "direction": "decreased"
          }
        ],
        "expected": {
          "type": CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE,
          "curie": "NCBIGene:123",
          "direction": "decreased",
          "subject": null,
          "object": null,
          "constraint": null
        }
      }
    },
    "pathfinder_request": {
      "class_constructor": {
        "args": [
          {
            "type": "pathfinder",
            "subject": { "id": "MONDO:123", "category": "Disease" },
            "object": { "id": "CHEBI:123", "category": "ChemicalEntity" },
            "constraint": "biolink:Gene"
          }
        ],
        "expected": {
          "type": CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER,
          "curie": null,
          "direction": null,
          "subject": { "id": "MONDO:123", "category": "Disease" },
          "object": { "id": "CHEBI:123", "category": "ChemicalEntity" },
          "constraint": "biolink:Gene"
        }
      }
    },
    "lookup_request": {
      "class_constructor": {
        "args": [
          {
            "type": "lookup",
            "subject": { "id": "MONDO:123", "category": "Disease" },
            "object": { "category": "ChemicalEntity" }
          }
        ],
        "expected": {
          "type": CONSTANTS.QGRAPH.TEMPLATE.LOOKUP,
          "curie": null,
          "direction": null,
          "subject": { "id": "MONDO:123", "category": "Disease" },
          "object": { "category": "ChemicalEntity" },
          "constraint": null
        }
      }
    },
    "request_not_an_object": {
      "class_constructor": {
        "args": [null],
        "expected": TypeError
      }
    },
    "request_is_an_array": {
      "class_constructor": {
        "args": [["drug"]],
        "expected": TypeError
      }
    },
    "unknown_type": {
      "class_constructor": {
        "args": [{ "type": "protein", "curie": "UniProtKB:P12345" }],
        "expected": RangeError
      }
    },
    "missing_type": {
      "class_constructor": {
        "args": [{ "curie": "MONDO:123" }],
        "expected": RangeError
      }
    }
  });
}

function _test_QNode() {
  return test.make_class_test({
    "with_curies": {
      "class_constructor": {
        "args": ["on", "Disease", ["MONDO:123"]],
        "expected": _expected_qnode_on_disease()
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "ids": ["MONDO:123"],
            "categories": ["biolink:Disease"]
          }
        },
        {
          "get": "binding",
          "expected": "on"
        }
      ]
    },
    "without_curies": {
      "class_constructor": {
        "args": ["sn", "ChemicalEntity"],
        "expected": _expected_qnode_sn_chemical()
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "categories": ["biolink:ChemicalEntity"]
          }
        }
      ]
    },
    "empty_curies": {
      "class_constructor": {
        "args": ["sn", "ChemicalEntity", []],
        "expected": _expected_qnode_sn_chemical()
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "categories": ["biolink:ChemicalEntity"]
          }
        }
      ]
    },
    "pretagged_category": {
      "class_constructor": {
        "args": ["sn", "biolink:ChemicalEntity"],
        "expected": _expected_qnode_sn_chemical()
      }
    },
    "missing_category": {
      "class_constructor": {
        "args": ["sn"],
        "expected": TypeError
      }
    },
    "null_category": {
      "class_constructor": {
        "args": ["sn", null, ["MONDO:123"]],
        "expected": TypeError
      }
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            "on",
            {
              "ids": ["MONDO:123"],
              "categories": ["biolink:Disease"]
            }
          ],
          "expected": _expected_qnode_on_disease()
        },
        {
          "method": "from_trapi",
          "args": [
            "sn",
            {
              "categories": ["biolink:ChemicalEntity"]
            }
          ],
          "expected": _expected_qnode_sn_chemical()
        },
        {
          "method": "from_trapi",
          "args": [
            "sn",
            {
              "ids": ["CHEBI:123"]
            }
          ],
          "expected": ReferenceError
        }
      ]
    }
  });
}

function _test_QEdge() {
  return test.make_class_test({
    "inferred_without_constraints": {
      "class_constructor": {
        "args": [_qnode_sn_chemical(), _qnode_on_disease(), "treats"],
        "expected": {
          "subject": _expected_qnode_sn_chemical(),
          "object": _expected_qnode_on_disease(),
          "predicates": ["biolink:treats"],
          "query_mode": "inferred"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on",
            "predicates": ["biolink:treats"],
            "knowledge_type": "inferred"
          }
        },
        {
          "method": "get_constraints",
          "args": [],
          "expected": undefined
        },
        {
          "method": "gen_binding",
          "args": [],
          "expected": "539ce0db"
        }
      ]
    },
    "pretagged_predicate": {
      "class_constructor": {
        "args": [_qnode_sn_chemical(), _qnode_on_disease(), "biolink:treats"],
        "expected": {
          "subject": _expected_qnode_sn_chemical(),
          "object": _expected_qnode_on_disease(),
          "predicates": ["biolink:treats"],
          "query_mode": "inferred"
        }
      },
      "steps": [
        {
          "method": "gen_binding",
          "args": [],
          "expected": "539ce0db"
        }
      ]
    },
    "lookup_mode": {
      "class_constructor": {
        "args": [_qnode_sn_disease(), _qnode_on_chemical(), "related_to", null, "lookup"],
        "expected": {
          "subject": _expected_qnode_sn_disease(),
          "object": _expected_qnode_on_chemical(),
          "predicates": ["biolink:related_to"],
          "query_mode": "lookup"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on",
            "predicates": ["biolink:related_to"],
            "knowledge_type": "lookup"
          }
        },
        {
          "method": "gen_binding",
          "args": [],
          "expected": "701bc289"
        }
      ]
    },
    "empty_constraints": {
      "class_constructor": {
        "args": [_qnode_sn_chemical(), _qnode_on_gene(), "affects", []],
        "expected": {
          "subject": _expected_qnode_sn_chemical(),
          "object": _expected_qnode_on_gene(),
          "predicates": ["biolink:affects"],
          "query_mode": "inferred"
        }
      },
      "steps": [
        {
          "method": "get_constraints",
          "args": [],
          "expected": undefined
        }
      ]
    },
    "with_constraints": {
      "class_constructor": {
        "args": [_qnode_sn_chemical(), _qnode_on_gene(), "affects", [_qualifier_set_increased()]],
        "expected": {
          "subject": _expected_qnode_sn_chemical(),
          "object": _expected_qnode_on_gene(),
          "predicates": ["biolink:affects"],
          "query_mode": "inferred",
          "qualifier_constraints": [_expected_qualifier_set_increased()]
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on",
            "predicates": ["biolink:affects"],
            "qualifier_constraints": [_expected_qualifier_set_increased()],
            "knowledge_type": "inferred"
          }
        },
        {
          "method": "get_constraints",
          "args": [],
          "expected": [_expected_qualifier_set_increased()]
        },
        {
          "method": "get_qualifiers",
          "args": [],
          "expected": [_expected_qualifier_set_increased()]
        }
      ]
    },
    "set_constraints_after_construction": {
      "class_constructor": {
        "args": [_qnode_sn_chemical(), _qnode_on_gene(), "affects"]
      },
      "steps": [
        {
          "method": "get_constraints",
          "args": [],
          "expected": undefined
        },
        {
          "method": "set_constraints",
          "args": [[_qualifier_set_increased()]],
          "expected": [_expected_qualifier_set_increased()]
        },
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on",
            "predicates": ["biolink:affects"],
            "qualifier_constraints": [_expected_qualifier_set_increased()],
            "knowledge_type": "inferred"
          }
        }
      ]
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            {
              "subject": "sn",
              "object": "on",
              "predicates": ["biolink:treats"],
              "knowledge_type": "inferred"
            },
            { "sn": _qnode_sn_chemical(), "on": _qnode_on_disease() }
          ],
          "expected": {
            "subject": _expected_qnode_sn_chemical(),
            "object": _expected_qnode_on_disease(),
            "predicates": ["biolink:treats"],
            "query_mode": "inferred"
          }
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "subject": "sn",
              "object": "on",
              "predicates": ["biolink:related_to"]
            },
            { "sn": _qnode_sn_disease(), "on": _qnode_on_chemical() }
          ],
          "expected": {
            "subject": _expected_qnode_sn_disease(),
            "object": _expected_qnode_on_chemical(),
            "predicates": ["biolink:related_to"],
            "query_mode": "lookup"
          }
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "subject": "sn",
              "object": "on",
              "predicates": ["biolink:treats"],
              "qualifier_constraints": [{ "attribute_constraint": {} }]
            },
            { "sn": _qnode_sn_chemical(), "on": _qnode_on_disease() }
          ],
          "expected": TypeError
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "subject": "sn",
              "object": "on"
            },
            { "sn": _qnode_sn_chemical(), "on": _qnode_on_disease() }
          ],
          "expected": ReferenceError
        }
      ]
    },
    "round_trip_without_constraints": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            new _QEdge(_qnode_sn_chemical(), _qnode_on_disease(), "treats").to_trapi(),
            { "sn": _qnode_sn_chemical(), "on": _qnode_on_disease() }
          ],
          "expected": {
            "subject": _expected_qnode_sn_chemical(),
            "object": _expected_qnode_on_disease(),
            "predicates": ["biolink:treats"],
            "query_mode": "inferred"
          }
        }
      ]
    },
    "round_trip_with_constraints": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            new _QEdge(_qnode_sn_chemical(), _qnode_on_gene(), "affects", [_qualifier_set_increased()]).to_trapi(),
            { "sn": _qnode_sn_chemical(), "on": _qnode_on_gene() }
          ],
          "expected": {
            "subject": _expected_qnode_sn_chemical(),
            "object": _expected_qnode_on_gene(),
            "predicates": ["biolink:affects"],
            "query_mode": "inferred",
            "qualifier_constraints": [_expected_qualifier_set_increased()]
          }
        }
      ]
    }
  });
}

function _test_QEdgeQualifierSet() {
  return test.make_class_test({
    "empty": {
      "class_constructor": {
        "args": [],
        "expected": { "qualifier_set": [] }
      },
      "steps": [
        {
          "method": "add",
          "args": [_qualifier_direction_increased()],
          "expected": undefined
        },
        {
          "get": "qualifier_set",
          "expected": [_expected_qualifier_direction_increased()]
        },
        {
          "method": "add",
          "args": [_qualifier_aspect_activity()],
          "expected": undefined
        },
        {
          "get": "qualifier_set",
          "expected": [
            _expected_qualifier_direction_increased(),
            _expected_qualifier_aspect_activity()
          ]
        }
      ]
    },
    "prefilled": {
      "class_constructor": {
        "args": [[_qualifier_predicate_causes(), _qualifier_aspect_activity()]],
        "expected": {
          "qualifier_set": [
            _expected_qualifier_predicate_causes(),
            _expected_qualifier_aspect_activity()
          ]
        }
      }
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            [
              _expected_qualifier_predicate_causes(),
              _expected_qualifier_aspect_activity(),
              _expected_qualifier_direction_increased()
            ]
          ],
          "expected": _expected_qualifier_set_increased()
        },
        {
          "method": "from_trapi",
          "args": [[]],
          "expected": { "qualifier_set": [] }
        }
      ]
    }
  });
}

function _test_QEdgeQualifier() {
  return test.make_class_test({
    "construct": {
      "class_constructor": {
        "args": ["biolink:qualified_predicate", "biolink:causes"],
        "expected": _expected_qualifier_predicate_causes()
      },
      "steps": [
        {
          "get": "qualifier_type_id",
          "expected": "biolink:qualified_predicate"
        },
        {
          "get": "qualifier_value",
          "expected": "biolink:causes"
        }
      ]
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [_expected_qualifier_direction_increased()],
          "expected": _expected_qualifier_direction_increased()
        },
        {
          "method": "from_trapi",
          "args": [{ "qualifier_type_id": "biolink:object_direction_qualifier" }],
          "expected": ReferenceError
        },
        {
          "method": "from_trapi",
          "args": [{ "qualifier_value": "increased" }],
          "expected": ReferenceError
        }
      ]
    }
  });
}

function _test_QPath() {
  return test.make_class_test({
    "with_constraint": {
      "class_constructor": {
        "args": ["p0", "sn", "on", "biolink:Gene"],
        "expected": {
          "binding": "p0",
          "subject": "sn",
          "object": "on",
          "constraint": "biolink:Gene"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on",
            "constraints": [{ "intermediate_categories": ["biolink:Gene"] }]
          }
        }
      ]
    },
    "without_constraint": {
      "class_constructor": {
        "args": ["p0", "sn", "on", null],
        "expected": {
          "binding": "p0",
          "subject": "sn",
          "object": "on",
          "constraint": null
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "subject": "sn",
            "object": "on"
          }
        }
      ]
    },
    "gen_binding": {
      "steps": [
        {
          "method": "gen_binding",
          "args": [],
          "expected": "p0"
        }
      ]
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": ["p0", { "subject": "sn", "object": "on" }],
          "expected": {
            "binding": "p0",
            "subject": "sn",
            "object": "on",
            "constraint": null
          }
        },
        {
          "method": "from_trapi",
          "args": ["p0", { "subject": "sn" }],
          "expected": ReferenceError
        }
      ]
    },
    "round_trip_without_constraint": {
      "steps": [
        {
          "method": "from_trapi",
          "args": ["p0", new _QPath("p0", "sn", "on", null).to_trapi()],
          "expected": {
            "binding": "p0",
            "subject": "sn",
            "object": "on",
            "constraint": null
          }
        }
      ]
    },
    "round_trip_with_constraint": {
      "steps": [
        {
          "method": "from_trapi",
          "args": ["p0", new _QPath("p0", "sn", "on", "biolink:Gene").to_trapi()],
          "expected": {
            "binding": "p0",
            "subject": "sn",
            "object": "on",
            "constraint": "biolink:Gene"
          }
        }
      ]
    }
  });
}

function _test_QGraph() {
  return test.make_class_test({
    "default_constructor": {
      "class_constructor": {
        "args": [],
        "expected": {
          "qnodes": {},
          "qconnections": {},
          "_query_type": "standard"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "nodes": {},
            "edges": {}
          }
        }
      ]
    },
    "chemical_disease": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": _qnode_sn_chemical(), "on": _qnode_on_disease() },
          { "e0": new _QEdge(_qnode_sn_chemical(), _qnode_on_disease(), "treats") }
        ],
        "expected": {
          "qnodes": {
            "sn": _expected_qnode_sn_chemical(),
            "on": _expected_qnode_on_disease()
          },
          "qconnections": {
            "e0": {
              "subject": _expected_qnode_sn_chemical(),
              "object": _expected_qnode_on_disease(),
              "predicates": ["biolink:treats"],
              "query_mode": "inferred"
            }
          },
          "_query_type": "standard"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "nodes": {
              "sn": { "categories": ["biolink:ChemicalEntity"] },
              "on": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] }
            },
            "edges": {
              "e0": {
                "subject": "sn",
                "object": "on",
                "predicates": ["biolink:treats"],
                "knowledge_type": "inferred"
              }
            }
          }
        },
        {
          "method": "get_type",
          "args": [],
          "expected": CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_DISEASE
        },
        {
          "method": "_is_lookup",
          "args": [],
          "expected": false
        }
      ]
    },
    "gene_chemical": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": _qnode_sn_chemical_with_id(), "on": _qnode_on_gene_without_id() },
          { "e0": new _QEdge(_qnode_sn_chemical_with_id(), _qnode_on_gene_without_id(), "affects", [_qualifier_set_increased()]) }
        ]
      },
      "steps": [
        {
          "method": "get_type",
          "args": [],
          "expected": CONSTANTS.QGRAPH.TEMPLATE.GENE_CHEMICAL
        }
      ]
    },
    "chemical_gene": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": _qnode_sn_chemical(), "on": _qnode_on_gene() },
          { "e0": new _QEdge(_qnode_sn_chemical(), _qnode_on_gene(), "affects", [_qualifier_set_increased()]) }
        ]
      },
      "steps": [
        {
          "method": "get_type",
          "args": [],
          "expected": CONSTANTS.QGRAPH.TEMPLATE.CHEMICAL_GENE
        }
      ]
    },
    "lookup": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": _qnode_sn_disease(), "on": _qnode_on_chemical() },
          { "e0": new _QEdge(_qnode_sn_disease(), _qnode_on_chemical(), "related_to", null, "lookup") }
        ]
      },
      "steps": [
        {
          "method": "_is_lookup",
          "args": [],
          "expected": true
        },
        {
          "method": "get_type",
          "args": [],
          "expected": CONSTANTS.QGRAPH.TEMPLATE.LOOKUP
        },
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "nodes": {
              "sn": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] },
              "on": { "categories": ["biolink:ChemicalEntity"] }
            },
            "edges": {
              "e0": {
                "subject": "sn",
                "object": "on",
                "predicates": ["biolink:related_to"],
                "knowledge_type": "lookup"
              }
            }
          }
        }
      ]
    },
    "pathfinder": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": _qnode_sn_disease(), "on": _qnode_on_chemical_with_id() },
          { "p0": new _QPath("p0", "sn", "on", "biolink:Gene") },
          "pathfinder"
        ],
        "expected": {
          "qnodes": {
            "sn": _expected_qnode_sn_disease(),
            "on": _expected_qnode_on_chemical_with_id()
          },
          "qconnections": {
            "p0": {
              "binding": "p0",
              "subject": "sn",
              "object": "on",
              "constraint": "biolink:Gene"
            }
          },
          "_query_type": "pathfinder"
        }
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": {
            "nodes": {
              "sn": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] },
              "on": { "ids": ["CHEBI:123"], "categories": ["biolink:ChemicalEntity"] }
            },
            "paths": {
              "p0": {
                "subject": "sn",
                "object": "on",
                "constraints": [{ "intermediate_categories": ["biolink:Gene"] }]
              }
            }
          }
        },
        {
          "method": "get_type",
          "args": [],
          "expected": CONSTANTS.QGRAPH.TEMPLATE.PATHFINDER
        }
      ]
    },
    "unsupported_query_type": {
      "class_constructor": {
        "args": [{}, {}, "bogus"]
      },
      "steps": [
        {
          "method": "to_trapi",
          "args": [],
          "expected": Error
        }
      ]
    },
    "unsupported_template": {
      "config_loader": () => load_trapi(_test_config),
      "class_constructor": {
        "args": [
          { "sn": new _QNode("sn", "Disease"), "on": _qnode_on_gene() },
          { "e0": new _QEdge(new _QNode("sn", "Disease"), _qnode_on_gene(), "related_to") }
        ]
      },
      "steps": [
        {
          "method": "get_type",
          "args": [],
          "expected": RangeError
        }
      ]
    },
    "from_trapi": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            {
              "nodes": {
                "sn": { "categories": ["biolink:ChemicalEntity"] },
                "on": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] }
              },
              "edges": {
                "e0": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": ["biolink:treats"],
                  "knowledge_type": "inferred"
                }
              }
            }
          ],
          "expected": {
            "qnodes": {
              "sn": _expected_qnode_sn_chemical(),
              "on": _expected_qnode_on_disease()
            },
            "qconnections": {
              "e0": {
                "subject": _expected_qnode_sn_chemical(),
                "object": _expected_qnode_on_disease(),
                "predicates": ["biolink:treats"],
                "query_mode": "inferred"
              }
            },
            "_query_type": "standard"
          }
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "nodes": {
                "sn": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] },
                "on": { "ids": ["CHEBI:123"], "categories": ["biolink:ChemicalEntity"] }
              },
              "paths": {
                "p0": { "subject": "sn", "object": "on" }
              }
            }
          ],
          "expected": {
            "qnodes": {
              "sn": _expected_qnode_sn_disease(),
              "on": _expected_qnode_on_chemical_with_id()
            },
            "qconnections": {
              "p0": {
                "binding": "p0",
                "subject": "sn",
                "object": "on",
                "constraint": null
              }
            },
            "_query_type": "pathfinder"
          }
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "nodes": {
                "sn": { "categories": ["biolink:ChemicalEntity"] },
                "on": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] }
              },
              "edges": {},
              "paths": {}
            }
          ],
          "expected": Error
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "nodes": {
                "sn": { "categories": ["biolink:ChemicalEntity"] },
                "on": { "ids": ["MONDO:123"], "categories": ["biolink:Disease"] }
              }
            }
          ],
          "expected": Error
        },
        {
          "method": "from_trapi",
          "args": [
            {
              "edges": {}
            }
          ],
          "expected": ReferenceError
        }
      ]
    },
    "round_trip_gene_chemical": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            new _QGraph(
              { "sn": _qnode_sn_chemical_with_id(), "on": _qnode_on_gene_without_id() },
              { "e0": new _QEdge(_qnode_sn_chemical_with_id(), _qnode_on_gene_without_id(), "affects", [_qualifier_set_increased()]) }
            ).to_trapi()
          ],
          "expected": {
            "qnodes": {
              "sn": { "ids": ["CHEBI:123"], "categories": ["biolink:ChemicalEntity"], "binding": "sn" },
              "on": { "categories": ["biolink:Gene"], "binding": "on" }
            },
            "qconnections": {
              "e0": {
                "subject": { "ids": ["CHEBI:123"], "categories": ["biolink:ChemicalEntity"], "binding": "sn" },
                "object": { "categories": ["biolink:Gene"], "binding": "on" },
                "predicates": ["biolink:affects"],
                "query_mode": "inferred",
                "qualifier_constraints": [_expected_qualifier_set_increased()]
              }
            },
            "_query_type": "standard"
          }
        }
      ]
    },
    "round_trip_pathfinder": {
      "steps": [
        {
          "method": "from_trapi",
          "args": [
            new _QGraph(
              { "sn": _qnode_sn_disease(), "on": _qnode_on_chemical_with_id() },
              { "p0": new _QPath("p0", "sn", "on", "biolink:Gene") },
              "pathfinder"
            ).to_trapi()
          ],
          "expected": {
            "qnodes": {
              "sn": _expected_qnode_sn_disease(),
              "on": _expected_qnode_on_chemical_with_id()
            },
            "qconnections": {
              "p0": {
                "binding": "p0",
                "subject": "sn",
                "object": "on",
                "constraint": "biolink:Gene"
              }
            },
            "_query_type": "pathfinder"
          }
        }
      ]
    }
  });
}

function _test_InvalidQualifiersError() {
  return test.make_class_test({
    "message_includes_edge": {
      "class_constructor": {
        "args": [{ "subject": "CHEBI:123", "object": "MONDO:123", "qualifiers": "bad" }]
      },
      "steps": [
        {
          "get": "message",
          "expected": 'Invalid qualifiers in knowledge edge: {"subject":"CHEBI:123","object":"MONDO:123","qualifiers":"bad"}'
        }
      ]
    }
  });
}

function _test_MissingQueryGraphError() {
  return test.make_class_test({
    "message_includes_message": {
      "class_constructor": {
        "args": [{ "message": { "knowledge_graph": {} } }]
      },
      "steps": [
        {
          "get": "message",
          "expected": 'No query graph in {"message":{"knowledge_graph":{}}}'
        }
      ]
    }
  });
}

function _qnode_sn_chemical() {
  return new _QNode("sn", "ChemicalEntity");
}

function _expected_qnode_sn_chemical() {
  return { "categories": ["biolink:ChemicalEntity"], "binding": "sn" };
}

function _qnode_sn_chemical_with_id() {
  return new _QNode("sn", "ChemicalEntity", ["CHEBI:123"]);
}

function _qnode_on_chemical() {
  return new _QNode("on", "ChemicalEntity");
}

function _expected_qnode_on_chemical() {
  return { "categories": ["biolink:ChemicalEntity"], "binding": "on" };
}

function _qnode_on_chemical_with_id() {
  return new _QNode("on", "ChemicalEntity", ["CHEBI:123"]);
}

function _expected_qnode_on_chemical_with_id() {
  return { "ids": ["CHEBI:123"], "categories": ["biolink:ChemicalEntity"], "binding": "on" };
}

function _qnode_on_disease() {
  return new _QNode("on", "Disease", ["MONDO:123"]);
}

function _expected_qnode_on_disease() {
  return { "ids": ["MONDO:123"], "categories": ["biolink:Disease"], "binding": "on" };
}

function _qnode_sn_disease() {
  return new _QNode("sn", "Disease", ["MONDO:123"]);
}

function _expected_qnode_sn_disease() {
  return { "ids": ["MONDO:123"], "categories": ["biolink:Disease"], "binding": "sn" };
}

function _qnode_on_gene() {
  return new _QNode("on", "Gene", ["NCBIGene:123"]);
}

function _expected_qnode_on_gene() {
  return { "ids": ["NCBIGene:123"], "categories": ["biolink:Gene"], "binding": "on" };
}

function _qnode_on_gene_without_id() {
  return new _QNode("on", "Gene");
}

function _qualifier_predicate_causes() {
  return new _QEdgeQualifier("biolink:qualified_predicate", "biolink:causes");
}

function _expected_qualifier_predicate_causes() {
  return { "qualifier_type_id": "biolink:qualified_predicate", "qualifier_value": "biolink:causes" };
}

function _qualifier_aspect_activity() {
  return new _QEdgeQualifier("biolink:object_aspect_qualifier", "activity_or_abundance");
}

function _expected_qualifier_aspect_activity() {
  return { "qualifier_type_id": "biolink:object_aspect_qualifier", "qualifier_value": "activity_or_abundance" };
}

function _qualifier_direction_increased() {
  return new _QEdgeQualifier("biolink:object_direction_qualifier", "increased");
}

function _expected_qualifier_direction_increased() {
  return { "qualifier_type_id": "biolink:object_direction_qualifier", "qualifier_value": "increased" };
}

function _qualifier_set_increased() {
  return new _QEdgeQualifierSet([
    _qualifier_predicate_causes(),
    _qualifier_aspect_activity(),
    _qualifier_direction_increased()
  ]);
}

function _expected_qualifier_set_increased() {
  return {
    "qualifier_set": [
      _expected_qualifier_predicate_causes(),
      _expected_qualifier_aspect_activity(),
      _expected_qualifier_direction_increased()
    ]
  };
}
