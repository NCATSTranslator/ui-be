export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import { CONSTANTS as TRAPI_CONSTANTS } from "#lib/trapi/core.mjs";
import {
  analysis_to_summary_analysis,
  gen_analysis_paths
} from "#lib/summarization/summary-analysis.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";
import * as id from "#lib/summarization/identifiers.mjs";

const DIRECT = TRAPI_CONSTANTS.GRAPH.EDGE.TYPE.DIRECT;
const INDIRECT = TRAPI_CONSTANTS.GRAPH.EDGE.TYPE.INDIRECT;

const suite = {
  tests: {
    analysis_to_summary_analysis: _test_analysis_to_summary_analysis(),
    gen_analysis_paths: _test_gen_analysis_paths(),
    summary_analysis_to_summary_paths_and_edges: await _test_summary_analysis_to_summary_paths_and_edges()
  }
};

function _test_analysis_to_summary_analysis() {
  return test.make_function_test({
    "simple": {
      "args": [
        {
          "edge_bindings": {
            "ab1": [
              { "id": "eb1" }
            ]
          }
        },
        _simple_kgraph(),
        {}
      ],
      "expected": {
        "root": {
          "id": "root",
          "_edge_ids": ["eb1"]
        },
        "node_ids": ["nb1", "nb2"],
        "_graphs": {
          "root": {
            "id": "root",
            "_edge_ids": ["eb1"]
          }
        },
        "_edges": {
          "eb1": {
            "id": "eb1",
            "_graph_ids": []
          }
        }
      },
      "config_loader": () => bl.loadBiolink(_test_biolink_config()),
    },
    "with-support": {
      "args": [
        {
          "edge_bindings": {
            "ab1": [
              { "id": "eb1" }
            ]
          }
        },
        _with_support_kgraph(),
        {
          "ax1": {
            "edges": [
              "eb2",
              "eb3"
            ]
          }
        }
      ],
      "expected": {
        "root": {
          "id": "root",
          "_edge_ids": ["eb1"]
        },
        "node_ids": ["nb1", "nb2", "nb2.1"],
        "_graphs": {
          "root": {
            "id": "root",
            "_edge_ids": ["eb1"]
          },
          "ax1": {
            "id": "ax1",
            "_edge_ids": ["eb2", "eb3"]
          }
        },
        "_edges": {
          "eb1": {
            "id": "eb1",
            "_graph_ids": ["ax1"]
          },
          "eb2": {
            "id": "eb2",
            "_graph_ids": []
          },
          "eb3": {
            "id": "eb3",
            "_graph_ids": []
          }
        }
      },
      "config_loader": () => bl.loadBiolink(_test_biolink_config()),
    },
    "with-nested-support": {
      "args": [
        {
          "edge_bindings": {
            "ab1": [
              { "id": "eb1" },
              { "id": "eb2" }
            ]
          }
        },
        _with_nested_support_kgraph(),
        {
          "ax1": {
            "edges": [
              "ax1-eb1",
              "ax1-eb2"
            ]
          },
          "ax2": {
            "edges": [
              "ax1-eb1",
              "ax2-eb1"
            ]
          },
          "ax3": {
            "edges": [
              "ax3-eb1",
              "ax3-eb2",
              "ax3-eb3"
            ]
          },
          "ax4": {
            "edges": [
              "ax4-eb1",
              "ax4-eb2"
            ]
          }
        }
      ],
      "expected": {
        "root": {
          "id": "root",
          "_edge_ids": ["eb1", "eb2"]
        },
        "node_ids": ["answer-2", "target", "answer-1", "nb3", "nb2", "nb1", "nb2.1"],
        "_graphs": {
          "root": {
            "id": "root",
            "_edge_ids": ["eb1", "eb2"]
          },
          "ax1": {
            "id": "ax1",
            "_edge_ids": ["ax1-eb1", "ax1-eb2"]
          },
          "ax2": {
            "id": "ax2",
            "_edge_ids": ["ax1-eb1", "ax2-eb1"]
          },
          "ax3": {
            "id": "ax3",
            "_edge_ids": ["ax3-eb1", "ax3-eb2", "ax3-eb3"]
          },
          "ax4": {
            "id": "ax4",
            "_edge_ids": ["ax4-eb1", "ax4-eb2"]
          }
        },
        "_edges": {
          "eb1": {
            "id": "eb1",
            "_graph_ids": ["ax1", "ax2"]
          },
          "eb2": {
            "id": "eb2",
            "_graph_ids": ["ax3"]
          },
          "ax1-eb1": {
            "id": "ax1-eb1",
            "_graph_ids": []
          },
          "ax1-eb2": {
            "id": "ax1-eb2",
            "_graph_ids": []
          },
          "ax2-eb1": {
            "id": "ax2-eb1",
            "_graph_ids": []
          },
          "ax3-eb1": {
            "id": "ax3-eb1",
            "_graph_ids": []
          },
          "ax3-eb2": {
            "id": "ax3-eb2",
            "_graph_ids": ["ax4"]
          },
          "ax3-eb3": {
            "id": "ax3-eb3",
            "_graph_ids": []
          },
          "ax4-eb1": {
            "id": "ax4-eb1",
            "_graph_ids": []
          },
          "ax4-eb2": {
            "id": "ax4-eb2",
            "_graph_ids": []
          }
        }
      },
      "config_loader": () => bl.loadBiolink(_test_biolink_config()),
    }
  });
}

function _test_gen_analysis_paths() {
  return test.make_function_test({
    "simple": {
      "args": [
        _simple_summary_analysis(),
        _simple_kgraph(),
        "nb1",
        ["nb2"],
        4
      ],
      "expected": {
        "root": [["nb1", "eb1", "nb2"]]
      }
    },
    "with-support": {
      "args": [
        _with_support_summary_analysis(),
        _with_support_kgraph(),
        "nb1",
        ["nb2"],
        4
      ],
      "expected": {
        "root": [["nb1", "eb1", "nb2"]],
        "ax1": [["nb1", "eb2", "nb2.1", "eb3", "nb2"]]
      }
    },
    "with-nested-support": {
      "args": [
        _with_nested_support_summary_analysis(),
        _with_nested_support_kgraph(),
        "target",
        ["answer-1", "answer-2"],
        4
      ],
      "expected": {
        "root": [
          ["target", "eb1", "answer-1"],
          ["target", "eb2", "answer-2"]
        ],
        "ax1": [["target", "ax1-eb2", "nb1", "ax1-eb1", "answer-1"]],
        "ax2": [["target", "ax2-eb1", "nb1", "ax1-eb1", "answer-1"]],
        "ax3": [["target", "ax3-eb3", "nb3", "ax3-eb2", "nb2", "ax3-eb1", "answer-2"]],
        "ax4": [["nb3", "ax4-eb2", "nb2.1", "ax4-eb1", "nb2"]]
      }
    }
  });
}

async function _test_summary_analysis_to_summary_paths_and_edges() {
  return test.make_function_test({
    simple: __simple_test_case(),
    with_support: __with_support_test_case(),
    with_nested_support: await __with_nested_support_test_case()
  });

  function __simple_test_case() {
    const _eid_1 = id.gen_eid("eb1", _simple_kgraph(), false, true);
    return {
      args: [
        _simple_summary_analysis(),
        _simple_analysis_paths(),
        _simple_kgraph()
      ],
      expected: [
        [
          [
            "nb1",
            _eid_1,
            "nb2"
          ]
        ],
        {
          [_eid_1]: _make_summary_edge("eb1", [], true, DIRECT)
        }
      ]
    }
  }

  function __with_support_test_case() {
    const _eid_1 = id.gen_eid("eb1", _with_support_kgraph(), false, true);
    const _eid_2 = id.gen_eid("eb2", _with_support_kgraph(), false, false);
    const _eid_3 = id.gen_eid("eb3", _with_support_kgraph(), false, false);
    const _support_path_1 = ["nb1", _eid_2, "nb2.1", _eid_3, "nb2"];
    return {
      args: [
        _with_support_summary_analysis(),
        _with_support_analysis_paths(),
        _with_support_kgraph()
      ],
      expected: [
        [
          [
            "nb1",
            _eid_1,
            "nb2"
          ],
          _support_path_1
        ],
        {
          [_eid_1]: _make_summary_edge("eb1", [id.gen_pid(_support_path_1)], true, INDIRECT),
          [_eid_2]: _make_summary_edge("eb2", [], false, DIRECT),
          [_eid_3]: _make_summary_edge("eb3", [], false, DIRECT)
        }
      ]
    }
  }

  async function __with_nested_support_test_case() {
    await bl.loadBiolink(_test_biolink_config());
    const _eid = [
      id.gen_eid("eb1", _with_nested_support_kgraph(), false, true),
      id.gen_eid("eb2", _with_nested_support_kgraph(), false, true),
      id.gen_eid("ax1-eb1", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax1-eb2", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax2-eb1", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax3-eb1", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax3-eb2", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax3-eb3", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax4-eb1", _with_nested_support_kgraph(), false, false),
      id.gen_eid("ax4-eb2", _with_nested_support_kgraph(), false, false)
    ]
    const _inverted_eid = [
      id.gen_eid("eb1", _with_nested_support_kgraph(), true, true),
      id.gen_eid("eb2", _with_nested_support_kgraph(), true, true),
      id.gen_eid("ax1-eb1", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax1-eb2", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax2-eb1", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax3-eb1", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax3-eb2", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax3-eb3", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax4-eb1", _with_nested_support_kgraph(), true, false),
      id.gen_eid("ax4-eb2", _with_nested_support_kgraph(), true, false)
    ];
    const _support_path = [
      ["target", _inverted_eid[3], "nb1", _inverted_eid[2], "answer-1"],
      ["target", _inverted_eid[4], "nb1", _inverted_eid[2], "answer-1"],
      ["target", _inverted_eid[7], "nb3", _inverted_eid[6], "nb2", _inverted_eid[5], "answer-2"]
    ];
    const _nested_support = ["nb3", _inverted_eid[9], "nb2.1", _inverted_eid[8], "nb2"];
    return {
      config_loader: () => bl.loadBiolink(_test_biolink_config()),
      args: [
        _with_nested_support_summary_analysis(),
        _with_nested_support_analysis_paths(),
        _with_nested_support_kgraph()
      ],
      expected: [
        [
          ["target", _inverted_eid[0], "answer-1"],
          ["target", _inverted_eid[1], "answer-2"],
          _support_path[0],
          _support_path[1],
          _support_path[2],
          _nested_support
        ],
        {
          [_inverted_eid[0]]: _make_summary_edge(
            "eb1",
            [id.gen_pid(_support_path[0]), id.gen_pid(_support_path[1])],
            true,
            INDIRECT,
            _eid[0]),
          [_inverted_eid[1]]: _make_summary_edge("eb2", [id.gen_pid(_support_path[2])], true, INDIRECT, _eid[1]),
          [_inverted_eid[2]]: _make_summary_edge("ax1-eb1", [], false, DIRECT, _eid[2]),
          [_inverted_eid[3]]: _make_summary_edge("ax1-eb2", [], false, DIRECT, _eid[3]),
          [_inverted_eid[4]]: _make_summary_edge("ax2-eb1", [], false, DIRECT, _eid[4]),
          [_inverted_eid[5]]: _make_summary_edge("ax3-eb1", [], false, DIRECT, _eid[5]),
          [_inverted_eid[6]]: _make_summary_edge("ax3-eb2", [id.gen_pid(_nested_support)], false, INDIRECT, _eid[6]),
          [_inverted_eid[7]]: _make_summary_edge("ax3-eb3", [], false, DIRECT, _eid[7]),
          [_inverted_eid[8]]: _make_summary_edge("ax4-eb1", [], false, DIRECT, _eid[8]),
          [_inverted_eid[9]]: _make_summary_edge("ax4-eb2", [], false, DIRECT, _eid[9])
        }
      ]
    }
  }
}

function _test_biolink_config() {
  return {
    "version": "4.2.1",
    "support_deprecated_predicates": false,
    "infores_catalog": "infores-catalog-v1.1.4.json",
    "prefix_catalog": {
      "path": "prefix-catalog.json",
      "exclude": ["VANDF"]
    }
  }
}

function _simple_kgraph() {
  return {
    "nodes": {
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" }
    },
    "edges": {
      "eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2"
      }
    }
  };
}

function _simple_summary_analysis() {
  return test.make_lazy({
    call: analysis_to_summary_analysis,
    args: [
      {
        "edge_bindings": {
          "ab1": [
            { "id": "eb1" }
          ]
        }
      },
      _simple_kgraph(),
      {}
    ]
  });
}

function _simple_analysis_paths() {
  return test.make_lazy({
    call: gen_analysis_paths,
    args: [
      _simple_summary_analysis(),
      _simple_kgraph(),
      "nb1",
      ["nb2"],
      4
    ]
  });
}

function _with_support_kgraph() {
  return {
    "nodes": {
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" },
      "nb2.1": { "name": "node 2.1" }
    },
    "edges": {
      "eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax1" ]
          }
        ]
      },
      "eb2": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "nb2.1"
      },
      "eb3": {
        "subject": "nb2.1",
        "predicate": "biolink:subclass_of",
        "object": "nb2"
      }
    }
  };
}

function _with_support_summary_analysis() {
  return test.make_lazy({
    call: analysis_to_summary_analysis,
    args: [
      {
        "edge_bindings": {
          "ab1": [
            { "id": "eb1" }
          ]
        }
      },
      _with_support_kgraph(),
      {
        "ax1": {
          "edges": [
            "eb2",
            "eb3"
          ]
        }
      }
    ],
  });
}

function _with_support_analysis_paths() {
  return test.make_lazy({
    call: gen_analysis_paths,
    args: [
      _with_support_summary_analysis(),
      _with_support_kgraph(),
      "nb1",
      ["nb2"],
      4
    ]
  });
}

function _with_nested_support_kgraph() {
  return {
    "nodes": {
      "answer-1": { "name": "answer 1" },
      "answer-2": { "name": "answer 2" },
      "nb1": { "name": "node 1" },
      "nb2": { "name": "node 2" },
      "nb2.1": { "name": "node 2.1" },
      "nb3": { "name": "node 5" },
      "target": { "name": "target" }
    },
    "edges": {
      "eb1": {
        "subject": "answer-1",
        "predicate": "biolink:treats",
        "object": "target",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax1", "ax2" ]
          }
        ]
      },
      "eb2": {
        "subject": "answer-2",
        "predicate": "biolink:treats",
        "object": "target",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax3" ]
          }
        ]
      },
      "ax1-eb1": {
        "subject": "answer-1",
        "predicate": "biolink:related_to",
        "object": "nb1"
      },
      "ax1-eb2": {
        "subject": "nb1",
        "predicate": "biolink:related_to",
        "object": "target"
      },
      "ax2-eb1": {
        "subject": "nb1",
        "predicate": "biolink:treats",
        "object": "target"
      },
      "ax3-eb1": {
        "subject": "answer-2",
        "predicate": "biolink:related_to",
        "object": "nb2"
      },
      "ax3-eb2": {
        "subject": "nb2",
        "predicate": "biolink:subclass_of",
        "object": "nb3",
        "attributes": [
          {
            "attribute_type_id": "biolink:support_graphs",
            "value": [ "ax4" ]
          }
        ]
      },
      "ax3-eb3": {
        "subject": "nb3",
        "predicate": "biolink:treats",
        "object": "target"
      },
      "ax4-eb1": {
        "subject": "nb2",
        "predicate": "biolink:subclass_of",
        "object": "nb2.1"
      },
      "ax4-eb2": {
        "subject": "nb2.1",
        "predicate": "biolink:subclass_of",
        "object": "nb3"
      }
    }
  };
}

function _with_nested_support_summary_analysis() {
  return test.make_lazy({
    call: analysis_to_summary_analysis,
    args: [
      {
        "edge_bindings": {
          "ab1": [
            { "id": "eb1" },
            { "id": "eb2" }
          ]
        }
      },
      _with_nested_support_kgraph(),
      {
        "ax1": {
          "edges": [
            "ax1-eb1",
            "ax1-eb2"
          ]
        },
        "ax2": {
          "edges": [
            "ax1-eb1",
            "ax2-eb1"
          ]
        },
        "ax3": {
          "edges": [
            "ax3-eb1",
            "ax3-eb2",
            "ax3-eb3"
          ]
        },
        "ax4": {
          "edges": [
            "ax4-eb1",
            "ax4-eb2"
          ]
        }
      }
    ]
  });
}

function _with_nested_support_analysis_paths() {
  return test.make_lazy({
    call: gen_analysis_paths,
    args: [
      _with_nested_support_summary_analysis(),
      _with_nested_support_kgraph(),
      "target",
      ["answer-1", "answer-2"],
      4
    ]
  });
}

function _make_summary_edge(edge_binding, support, is_root, type, inverted_id = null) {
  const summary_edge = new SummaryEdge();
  summary_edge.isRootPath = is_root;
  summary_edge.supPaths = support;
  summary_edge.metadata = {
    edge_bindings: [edge_binding],
    inverted_id: inverted_id,
    is_root: is_root
  }
  summary_edge.type = type;
  return summary_edge;
}
