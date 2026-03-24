export { suite }

import * as cmn from "#lib/common.mjs";
import * as test from "#test/lib/common.mjs";
import { load_trapi, CONSTANTS } from "#lib/trapi/core.mjs";

const suite = {
  tests: {
    client_request_to_trapi_query: _test_client_request_to_trapi_query(),
    get_pk: _test_get_pk(),
    get_results: _test_get_results(),
    get_auxiliary_graphs: _test_get_auxiliary_graphs(),
    get_auxiliary_graph: _test_get_auxiliary_graph(),
    get_auxiliary_graph_edges: _test_get_auxiliary_graph_edges(),
    get_edge_bindings: _test_get_edge_bindings(),
    get_node_binding: _test_get_node_binding(),
    get_path_bindings: _test_get_path_bindings(),
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
    is_valid_query: _test_is_valid_query(),
    AttributeIterator: _test_AttributeIterator()
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
                "predicates": ["biolink:treats"],
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
                "predicates": ["biolink:treats"],
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
                "predicates": ["biolink:treats"],
                "qualifier_set": [
                  {
                    "qualifier_type_id": "biolink:qualified_predicate",
                    "qualifier_value": "biolink:causes"
                  },
                  {
                    "qualifier_type_id": "biolink:subject_aspect_qualifier",
                    "qualifier_value": "activity_or_abundance"
                  },
                  {
                    "qualifier_type_id": "biolink:subject_direction_qualifier",
                    "qualifier_value": "increased"
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
                "predicates": ["biolink:treats"],
                "qualifier_set": [
                  {
                    "qualifier_type_id": "biolink:qualified_predicate",
                    "qualifier_value": "biolink:causes"
                  },
                  {
                    "qualifier_type_id": "biolink:subject_aspect_qualifier",
                    "qualifier_value": "activity_or_abundance"
                  },
                  {
                    "qualifier_type_id": "biolink:subject_direction_qualifier",
                    "qualifier_value": "decreased"
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
                "object": "on"
              }
            }
          }
        }
      }
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

function _test_get_node_binding() {
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
      "expected": "infores:gtopdb"
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
      "expected": "infores:gtopdb"
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
    "invalid_template": {
      "args": [
        4
      ],
      "expected": false
    }
  });
}

function _test_AttributeIterator() {
  return test.make_class_test({
    "empty_constructor": {
      "constructor": {
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
      "constructor": {
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
      "constructor": {
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
      "constructor": {
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
