export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import * as trapi from "#lib/trapi/core.mjs";
import * as id from "#lib/summarization/identifiers.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";
import { SummaryPath } from "#lib/summarization/SummaryPath.mjs";
import {
  biolink_config,
  simple_kgraph,
  sourced_kgraph,
  dangling_object_kgraph,
  dead_support_kgraph
} from "#test/data/summarization/kgraphs.mjs";
import {
  SummaryFragment,
  SummaryFragmentEdges,
  MissingInforesError,
  NodeBindingNotFoundError
} from "#lib/summarization/summary-fragment.mjs";

await bl.load_biolink(biolink_config());
trapi.load_trapi({ "query_subject_key": "sn", "query_object_key": "on" });

const suite = {
  tests: {
    answers_to_summary_fragments: _test_answers_to_summary_fragments(),
    answer_to_fragments: _test_answer_to_fragments(),
    result_to_fragment: _test_result_to_fragment(),
    analysis_to_fragment: _test_analysis_to_fragment(),
    gen_kedge_transforms: _test_gen_kedge_transforms(),
    make_error_fragment: _test_make_error_fragment(),
    SummaryFragment: _test_SummaryFragment(),
    SummaryFragmentEdges: _test_SummaryFragmentEdges(),
    _make_fragment_config: _test_make_fragment_config(),
    _get_result_endpoints: _test_get_result_endpoints(),
    _flatten_binding: _test_flatten_binding(),
    _get_result_scoring_components: _test_get_result_scoring_components(),
    _make_knode_transform: _test_make_knode_transform(),
    _make_kedge_transform: _test_make_kedge_transform(),
    _get_knowledge_level: _test_get_knowledge_level()
  },
  skip: {
    gen_knode_transforms: true,
    MissingInforesError: true,
    NodeBindingNotFoundError: true
  }
};

function _name_and_agent_rules(knode, context) {
  return [knode.name, context.agent];
}

function _predicate_and_context_rules(kedge, context) {
  return [kedge.predicate, context.agent, context.infores, context.knowledge_level];
}

function _error_recording_knode_rules(knode, context) {
  const agent_errors = context.errors[context.agent] ?? (context.errors[context.agent] = []);
  agent_errors.push(knode.name);
  return [];
}

function _make_summary_edge(eid, edge_binding, is_root, inverted_id = null) {
  const summary_edge = new SummaryEdge(eid);
  summary_edge.is_root = is_root;
  summary_edge.metadata = {
    edge_bindings: [edge_binding],
    inverted_id: inverted_id,
    is_root: is_root
  };
  return summary_edge;
}

function _merged_summary_edge(eid, edge_bindings, is_root, metadata_is_root = is_root) {
  const summary_edge = _make_summary_edge(eid, edge_bindings[0], metadata_is_root);
  summary_edge.is_root = is_root;
  summary_edge.metadata.edge_bindings = edge_bindings;
  return summary_edge;
}

function _test_answers_to_summary_fragments() {
  const eid = id.gen_eid("eb1", sourced_kgraph(), false, true);
  return test.make_function_test({
    "rules-share-the-errors-accumulator": {
      "args": [
        [
          {
            "message": {
              "query_graph": {
                "nodes": {
                  "sn": { "categories": ["biolink:ChemicalEntity"] },
                  "on": { "categories": ["biolink:Disease"], "ids": ["MONDO:0008029"] }
                },
                "edges": {
                  "t_edge": {
                    "subject": "sn",
                    "object": "on",
                    "predicates": ["biolink:treats"],
                    "knowledge_type": "inferred"
                  }
                }
              },
              "knowledge_graph": sourced_kgraph(),
              "auxiliary_graphs": {},
              "results": [
                {
                  "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
                  "analyses": [
                    { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
                  ],
                  "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
                  "normalized_score": 0.7
                }
              ]
            }
          },
          {
            "message": {
              "query_graph": {
                "nodes": {
                  "sn": { "categories": ["biolink:ChemicalEntity"] },
                  "on": { "categories": ["biolink:Disease"], "ids": ["MONDO:0008029"] }
                },
                "edges": {
                  "t_edge": {
                    "subject": "sn",
                    "object": "on",
                    "predicates": ["biolink:treats"],
                    "knowledge_type": "inferred"
                  }
                }
              },
              "knowledge_graph": sourced_kgraph(),
              "auxiliary_graphs": {},
              "results": [
                {
                  "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
                  "analyses": [
                    { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
                  ],
                  "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
                  "normalized_score": 0.7
                }
              ]
            }
          }
        ],
        _error_recording_knode_rules,
        _predicate_and_context_rules,
        { "max_hops": 1, "replace_indirect_edges": false }
      ],
      "expected": [
        [
          {
            "agents": ["infores:ara1"],
            "paths": [new SummaryPath(["nb1", eid, "nb2"])],
            "nodes": [
              { "id": "nb1", "transforms": [] },
              { "id": "nb2", "transforms": [] }
            ],
            "edges": {
              "base": { [eid]: _make_summary_edge(eid, "eb1", true) },
              "updates": [{ "id": eid, "transforms": ["biolink:treats", "infores:ara1", "infores:gtopdb", "trusted"] }]
            },
            "scores": { "nb1": [{ "confidence": 1, "novelty": 0.5, "clinical_evidence": 0, "normalized_score": 0.7 }] },
            "errors": {}
          },
          {
            "agents": ["infores:ara1"],
            "paths": [new SummaryPath(["nb1", eid, "nb2"])],
            "nodes": [
              { "id": "nb1", "transforms": [] },
              { "id": "nb2", "transforms": [] }
            ],
            "edges": {
              "base": { [eid]: _make_summary_edge(eid, "eb1", true) },
              "updates": [{ "id": eid, "transforms": ["biolink:treats", "infores:ara1", "infores:gtopdb", "trusted"] }]
            },
            "scores": { "nb1": [{ "confidence": 1, "novelty": 0.5, "clinical_evidence": 0, "normalized_score": 0.7 }] },
            "errors": {}
          }
        ],
        { "infores:ara1": ["node 1", "node 2", "node 1", "node 2"] }
      ]
    },
    "no-answers": {
      "args": [
        [],
        _name_and_agent_rules,
        _predicate_and_context_rules,
        { "max_hops": 1, "replace_indirect_edges": false }
      ],
      "expected": [[], {}]
    }
  });
}

function _test_answer_to_fragments() {
  return test.make_function_test({
    "no-results-key": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": { "categories": ["biolink:ChemicalEntity"] },
                "on": { "categories": ["biolink:Disease"], "ids": ["MONDO:0008029"] }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": ["biolink:treats"],
                  "knowledge_type": "inferred"
                }
              }
            },
            "knowledge_graph": sourced_kgraph(),
            "auxiliary_graphs": {}
          }
        },
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {}
      ],
      "expected": []
    },
    "error-only-result-is-dropped": {
      "args": [
        {
          "message": {
            "query_graph": {
              "nodes": {
                "sn": { "categories": ["biolink:ChemicalEntity"] },
                "on": { "categories": ["biolink:Disease"], "ids": ["MONDO:0008029"] }
              },
              "edges": {
                "t_edge": {
                  "subject": "sn",
                  "object": "on",
                  "predicates": ["biolink:treats"],
                  "knowledge_type": "inferred"
                }
              }
            },
            "knowledge_graph": sourced_kgraph(),
            "auxiliary_graphs": {},
            "results": [
              {
                "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
                "analyses": [
                  { "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
                ],
                "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
                "normalized_score": 0.7
              }
            ]
          }
        },
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {}
      ],
      "expected": []
    }
  });
}

function _test_result_to_fragment() {
  const eid = id.gen_eid("eb1", sourced_kgraph(), false, true);
  return test.make_function_test({
    "single-analysis-with-score": {
      "args": [
        {
          "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
          "analyses": [
            { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
          ],
          "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
          "normalized_score": 0.7
        },
        sourced_kgraph(),
        {},
        "sn",
        "on",
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [new SummaryPath(["nb1", eid, "nb2"])],
        "nodes": [
          { "id": "nb1", "transforms": ["node 1", "infores:ara1"] },
          { "id": "nb2", "transforms": ["node 2", "infores:ara1"] }
        ],
        "edges": {
          "base": { [eid]: _make_summary_edge(eid, "eb1", true) },
          "updates": [{ "id": eid, "transforms": ["biolink:treats", "infores:ara1", "infores:gtopdb", "trusted"] }]
        },
        "scores": { "nb1": [{ "confidence": 1, "novelty": 0.5, "clinical_evidence": 0, "normalized_score": 0.7 }] },
        "errors": {}
      }
    },
    "multiple-analyses-are-merged": {
      "args": [
        {
          "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
          "analyses": [
            { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
            { "resource_id": "infores:ara2", "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
          ],
          "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
          "normalized_score": 0.7
        },
        sourced_kgraph(),
        {},
        "sn",
        "on",
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1", "infores:ara2"],
        "paths": [new SummaryPath(["nb1", eid, "nb2"]), new SummaryPath(["nb1", eid, "nb2"])],
        "nodes": [
          { "id": "nb1", "transforms": ["node 1", "infores:ara1"] },
          { "id": "nb2", "transforms": ["node 2", "infores:ara1"] },
          { "id": "nb1", "transforms": ["node 1", "infores:ara2"] },
          { "id": "nb2", "transforms": ["node 2", "infores:ara2"] }
        ],
        "edges": {
          "base": { [eid]: _merged_summary_edge(eid, ["eb1", "eb1"], true) },
          "updates": [
            { "id": eid, "transforms": ["biolink:treats", "infores:ara1", "infores:gtopdb", "trusted"] },
            { "id": eid, "transforms": ["biolink:treats", "infores:ara2", "infores:gtopdb", "trusted"] }
          ]
        },
        "scores": { "nb1": [{ "confidence": 1, "novelty": 0.5, "clinical_evidence": 0, "normalized_score": 0.7 }] },
        "errors": {}
      }
    },
    "error-analysis-gets-no-score": {
      "args": [
        {
          "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] },
          "analyses": [
            { "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
          ],
          "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
          "normalized_score": 0.7
        },
        sourced_kgraph(),
        {},
        "sn",
        "on",
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["unknown"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "unknown": ["Expected analysis to have resource_id"] }
      }
    },
    "missing-start-binding": {
      "args": [
        {
          "node_bindings": { "on": [{ "id": "nb2" }] },
          "analyses": [
            { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } }
          ],
          "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
          "normalized_score": 0.7
        },
        sourced_kgraph(),
        {},
        "sn",
        "on",
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["unknown"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "unknown": ['Node binding not found for "sn"'] }
      }
    },
    "missing-analyses": {
      "args": [
        {
          "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] }
        },
        sourced_kgraph(),
        {},
        "sn",
        "on",
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["unknown"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "unknown": ["Unknown error while building result summary fragment"] }
      }
    }
  });
}

function _test_analysis_to_fragment() {
  const eid = id.gen_eid("eb1", sourced_kgraph(), false, true);
  return test.make_function_test({
    "single-edge": {
      "args": [
        { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
        sourced_kgraph(),
        {},
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [new SummaryPath(["nb1", eid, "nb2"])],
        "nodes": [
          { "id": "nb1", "transforms": ["node 1", "infores:ara1"] },
          { "id": "nb2", "transforms": ["node 2", "infores:ara1"] }
        ],
        "edges": {
          "base": { [eid]: _make_summary_edge(eid, "eb1", true) },
          "updates": [{ "id": eid, "transforms": ["biolink:treats", "infores:ara1", "infores:gtopdb", "trusted"] }]
        },
        "scores": {},
        "errors": {}
      }
    },
    "missing-resource-id": {
      "args": [
        { "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
        sourced_kgraph(),
        {},
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["unknown"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "unknown": ["Expected analysis to have resource_id"] }
      }
    },
    "edge-binding-not-found": {
      "args": [
        { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb-missing" }] } },
        sourced_kgraph(),
        {},
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "infores:ara1": ['Edge binding not found for "eb-missing"'] }
      }
    },
    "summary-analysis-construction-failed": {
      "args": [
        { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
        dangling_object_kgraph(),
        {},
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "infores:ara1": ["Summary analysis construction failed"] }
      }
    },
    "missing-support-paths": {
      "args": [
        { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
        dead_support_kgraph(),
        { "ax1": { "edges": ["eb2"] } },
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "infores:ara1": ["Support graph ax1 bound to edge eb1 produced no paths"] }
      }
    },
    "unknown-error": {
      "args": [
        { "resource_id": "infores:ara1", "edge_bindings": { "ab1": [{ "id": "eb1" }] } },
        simple_kgraph(),
        {},
        "nb1",
        ["nb2"],
        {
          "knode_rules": _name_and_agent_rules,
          "kedge_rules": _predicate_and_context_rules,
          "max_path_len": 3,
          "replace_indirect_edges": false
        },
        {},
        new Map()
      ],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "infores:ara1": ["Unknown error while building Analysis Graph"] }
      }
    }
  });
}

function _test_gen_kedge_transforms() {
  return test.make_function_test({
    "multiple-bindings-share-summary-edge-id": {
      "args": [
        {
          "se1": { "metadata": { "edge_bindings": ["eb1", "eb2"] } }
        },
        sourced_kgraph(),
        _predicate_and_context_rules,
        { "agent": "ara1" }
      ],
      "expected": [
        { "id": "se1", "transforms": ["biolink:treats", "ara1", "infores:gtopdb", "trusted"] },
        { "id": "se1", "transforms": ["biolink:treats", "ara1", "infores:arax", "inferred"] }
      ]
    }
  });
}

function _test_make_error_fragment() {
  return test.make_function_test({
    "records-agent-and-error": {
      "args": ["infores:ara1", "something went wrong"],
      "expected": {
        "agents": ["infores:ara1"],
        "paths": [],
        "nodes": [],
        "edges": { "base": {}, "updates": [] },
        "scores": {},
        "errors": { "infores:ara1": ["something went wrong"] }
      }
    }
  });
}

function _test_SummaryFragment() {
  return test.make_class_test({
    "defaults": {
      "class_constructor": {
        "args": [],
        "expected": {
          "agents": [],
          "paths": [],
          "nodes": [],
          "edges": { "base": {}, "updates": [] },
          "scores": {},
          "errors": {}
        }
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": true }
      ]
    },
    "not-empty-with-nodes": {
      "class_constructor": {
        "args": [["infores:ara1"], [], [{ "id": "nb1", "transforms": [] }]]
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": false }
      ]
    },
    "not-empty-with-edge-updates": {
      "class_constructor": {
        "args": [
          ["infores:ara1"],
          [],
          [],
          new SummaryFragmentEdges({}, [{ "id": "se1", "transforms": [] }])
        ]
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": false }
      ]
    },
    "push-score-groups-by-result": {
      "class_constructor": { "args": [] },
      "steps": [
        { "method": "push_score", "args": ["r1", { "score": 1 }], "expected": undefined },
        { "method": "push_score", "args": ["r1", { "score": 2 }], "expected": undefined },
        { "method": "push_score", "args": ["r2", { "score": 3 }], "expected": undefined },
        {
          "get": "scores",
          "expected": {
            "r1": [{ "score": 1 }, { "score": 2 }],
            "r2": [{ "score": 3 }]
          }
        }
      ]
    },
    "push-agent-and-error": {
      "class_constructor": { "args": [] },
      "steps": [
        { "method": "push_agent", "args": ["infores:ara1"], "expected": undefined },
        { "method": "push_error", "args": ["infores:ara1", "first"], "expected": undefined },
        { "method": "push_error", "args": ["infores:ara1", "second"], "expected": undefined },
        { "get": "agents", "expected": ["infores:ara1"] },
        { "get": "errors", "expected": { "infores:ara1": ["first", "second"] } }
      ]
    },
    "merge-combines-all-sections": {
      "class_constructor": {
        "args": [
          ["infores:ara1"],
          [new SummaryPath(["nb1", "e1", "nb2"])],
          [{ "id": "nb1", "transforms": [] }],
          new SummaryFragmentEdges({ "e1": _make_summary_edge("e1", "eb1", true) }, [{ "id": "e1", "transforms": [] }]),
          { "r1": [{ "score": 1 }] },
          { "infores:ara1": ["first"] }
        ]
      },
      "steps": [
        {
          "method": "merge",
          "args": [
            new SummaryFragment(
              ["infores:ara2"],
              [new SummaryPath(["nb1", "e2", "nb3"])],
              [{ "id": "nb3", "transforms": [] }],
              new SummaryFragmentEdges({ "e1": _make_summary_edge("e1", "eb2", false) }, [{ "id": "e2", "transforms": [] }]),
              { "r1": [{ "score": 2 }], "r2": [{ "score": 3 }] },
              { "infores:ara1": ["second"], "infores:ara2": ["third"] }
            )
          ],
          "expected": {
            "agents": ["infores:ara1", "infores:ara2"],
            "paths": [new SummaryPath(["nb1", "e1", "nb2"]), new SummaryPath(["nb1", "e2", "nb3"])],
            "nodes": [{ "id": "nb1", "transforms": [] }, { "id": "nb3", "transforms": [] }],
            "edges": {
              "base": {
                "e1": _merged_summary_edge("e1", ["eb1", "eb2"], true)
              },
              "updates": [{ "id": "e1", "transforms": [] }, { "id": "e2", "transforms": [] }]
            },
            "scores": { "r1": [{ "score": 1 }, { "score": 2 }], "r2": [{ "score": 3 }] },
            "errors": { "infores:ara1": ["first", "second"], "infores:ara2": ["third"] }
          }
        }
      ]
    }
  });
}

function _test_SummaryFragmentEdges() {
  return test.make_class_test({
    "defaults": {
      "class_constructor": {
        "args": [],
        "expected": { "base": {}, "updates": [] }
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": true }
      ]
    },
    "not-empty-with-base": {
      "class_constructor": {
        "args": [{ "e1": _make_summary_edge("e1", "eb1", true) }]
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": false }
      ]
    },
    "not-empty-with-updates": {
      "class_constructor": {
        "args": [{}, [{ "id": "e1", "transforms": [] }]]
      },
      "steps": [
        { "method": "is_empty", "args": [], "expected": false }
      ]
    },
    "merge-existing-edge-and-appends-updates": {
      "class_constructor": {
        "args": [
          { "e1": _make_summary_edge("e1", "eb1", false) },
          [{ "id": "e1", "transforms": [] }]
        ]
      },
      "steps": [
        {
          "method": "merge",
          "args": [
            new SummaryFragmentEdges(
              { "e1": _make_summary_edge("e1", "eb2", true) },
              [{ "id": "e1", "transforms": ["x"] }]
            )
          ],
          "expected": {
            "base": { "e1": _merged_summary_edge("e1", ["eb1", "eb2"], true, false) },
            "updates": [{ "id": "e1", "transforms": [] }, { "id": "e1", "transforms": ["x"] }]
          }
        }
      ]
    }
  });
}

function _test_make_fragment_config() {
  return test.make_function_test({
    "derives-max-path-len-from-hops": {
      "args": [
        _name_and_agent_rules,
        _predicate_and_context_rules,
        { "max_hops": 2, "replace_indirect_edges": true }
      ],
      "expected": {
        "knode_rules": _name_and_agent_rules,
        "kedge_rules": _predicate_and_context_rules,
        "max_path_len": 5,
        "replace_indirect_edges": true
      }
    }
  });
}

function _test_get_result_endpoints() {
  return test.make_function_test({
    "single-end": {
      "args": [
        { "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] } },
        "sn",
        "on",
        simple_kgraph()
      ],
      "expected": ["nb1", ["nb2"]]
    },
    "multiple-ends": {
      "args": [
        { "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }, { "id": "nb2.1" }] } },
        "sn",
        "on",
        dead_support_kgraph()
      ],
      "expected": ["nb1", ["nb2", "nb2.1"]]
    },
    "missing-start-key": {
      "args": [
        { "node_bindings": { "on": [{ "id": "nb2" }] } },
        "sn",
        "on",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    },
    "missing-end-key": {
      "args": [
        { "node_bindings": { "sn": [{ "id": "nb1" }] } },
        "sn",
        "on",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    }
  });
}

function _test_flatten_binding() {
  return test.make_function_test({
    "single-binding": {
      "args": [
        { "node_bindings": { "sn": [{ "id": "nb1" }], "on": [{ "id": "nb2" }] } },
        "sn",
        simple_kgraph()
      ],
      "expected": ["nb1"]
    },
    "multiple-bindings": {
      "args": [
        { "node_bindings": { "on": [{ "id": "nb2" }, { "id": "nb2.1" }] } },
        "on",
        dead_support_kgraph()
      ],
      "expected": ["nb2", "nb2.1"]
    },
    "missing-key": {
      "args": [
        { "node_bindings": { "on": [{ "id": "nb2" }] } },
        "sn",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    },
    "empty-bindings": {
      "args": [
        { "node_bindings": { "sn": [] } },
        "sn",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    },
    "binding-not-an-array": {
      "args": [
        { "node_bindings": { "sn": { "id": "nb1" } } },
        "sn",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    },
    "binding-not-in-kgraph": {
      "args": [
        { "node_bindings": { "sn": [{ "id": "nb-missing" }] } },
        "sn",
        simple_kgraph()
      ],
      "expected": NodeBindingNotFoundError
    }
  });
}

function _test_get_result_scoring_components() {
  return test.make_function_test({
    "with-components-and-score": {
      "args": [
        {
          "ordering_components": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0 },
          "normalized_score": 0.7
        }
      ],
      "expected": { "confidence": 1, "novelty": 0.5, "clinical_evidence": 0, "normalized_score": 0.7 }
    },
    "defaults-when-missing": {
      "args": [{}],
      "expected": { "confidence": 0, "novelty": 0, "clinical_evidence": 0, "normalized_score": 0 }
    }
  });
}

function _test_make_knode_transform() {
  return test.make_function_test({
    "applies-rules-with-context": {
      "args": [
        "nb1",
        { "name": "node 1" },
        _name_and_agent_rules,
        { "agent": "ara1" }
      ],
      "expected": {
        "id": "nb1",
        "transforms": ["node 1", "ara1"]
      }
    },
    "no-rules": {
      "args": [
        "nb1",
        { "name": "node 1" },
        () => [],
        { "agent": "ara1" }
      ],
      "expected": {
        "id": "nb1",
        "transforms": []
      }
    }
  });
}

function _test_make_kedge_transform() {
  return test.make_function_test({
    "extends-context-with-provenance": {
      "args": [
        "se1",
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "sources": [
            { "resource_id": "infores:gtopdb", "resource_role": "primary_knowledge_source" }
          ],
          "attributes": [
            { "attribute_type_id": "biolink:knowledge_level", "value": "knowledge_assertion" }
          ]
        },
        _predicate_and_context_rules,
        { "agent": "ara1" }
      ],
      "expected": {
        "id": "se1",
        "transforms": ["biolink:treats", "ara1", "infores:gtopdb", "trusted"]
      }
    },
    "falls-back-to-catalog-knowledge-level": {
      "args": [
        "se1",
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "sources": [
            { "resource_id": "infores:arax", "resource_role": "primary_knowledge_source" }
          ],
          "attributes": []
        },
        _predicate_and_context_rules,
        { "agent": "ara1" }
      ],
      "expected": {
        "id": "se1",
        "transforms": ["biolink:treats", "ara1", "infores:arax", "inferred"]
      }
    },
    "missing-infores": {
      "args": [
        "se1",
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "sources": [
            { "resource_id": "infores:not-a-real-source", "resource_role": "primary_knowledge_source" }
          ],
          "attributes": []
        },
        _predicate_and_context_rules,
        { "agent": "ara1" }
      ],
      "expected": MissingInforesError
    },
    "no-primary-source": {
      "args": [
        "se1",
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "sources": []
        },
        _predicate_and_context_rules,
        { "agent": "ara1" }
      ],
      "expected": Error
    }
  });
}

function _test_get_knowledge_level() {
  return test.make_function_test({
    "text-mining-agent": {
      "args": [
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "attributes": [
            { "attribute_type_id": "biolink:agent_type", "value": "text_mining_agent" },
            { "attribute_type_id": "biolink:knowledge_level", "value": "knowledge_assertion" }
          ]
        },
        { "knowledge_level": "trusted" }
      ],
      "expected": "ml"
    },
    "knowledge-assertion": {
      "args": [
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "attributes": [
            { "attribute_type_id": "biolink:agent_type", "value": "manual_agent" },
            { "attribute_type_id": "biolink:knowledge_level", "value": "knowledge_assertion" }
          ]
        },
        { "knowledge_level": "unknown" }
      ],
      "expected": "trusted"
    },
    "not-provided": {
      "args": [
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "attributes": [
            { "attribute_type_id": "biolink:knowledge_level", "value": "not_provided" }
          ]
        },
        { "knowledge_level": "trusted" }
      ],
      "expected": "unknown"
    },
    "other-knowledge-level": {
      "args": [
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "attributes": [
            { "attribute_type_id": "biolink:knowledge_level", "value": "prediction" }
          ]
        },
        { "knowledge_level": "trusted" }
      ],
      "expected": "inferred"
    },
    "no-attributes-uses-provenance": {
      "args": [
        {
          "subject": "nb1",
          "predicate": "biolink:treats",
          "object": "nb2",
          "attributes": []
        },
        { "knowledge_level": "trusted" }
      ],
      "expected": "trusted"
    }
  });
}
