export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import { SummaryPath } from "#lib/summarization/SummaryPath.mjs";
import { biolink_config } from "#test/data/summarization/kgraphs.mjs";

await bl.load_biolink(biolink_config());

const suite = {
  tests: {
    _merge_fragments: _test_merge_fragments()
  },
  skip: {
    answers_to_summary: true
  }
};

function _test_merge_fragments() {
  return test.make_function_test({
    "merges-two-agents-into-one-summary": {
      "args": [
        [
          {
            "agents": ["infores:ara1"],
            "paths": [new SummaryPath(["nb1", "e1", "nb2"])],
            "nodes": [
              {
                "id": "nb1",
                "transforms": [(node) => { node.names.push("drug"); node.types.push("biolink:ChemicalEntity"); return node; }]
              },
              {
                "id": "nb2",
                "transforms": [(node) => { node.names.push("disease"); node.types.push("biolink:Disease"); return node; }]
              }
            ],
            "edges": {
              "base": {
                "e1": {
                  "id": "e1",
                  "aras": [],
                  "is_root": true,
                  "knowledge_level": null,
                  "description": null,
                  "subject": null,
                  "object": null,
                  "predicate": null,
                  "predicate_url": null,
                  "provenance": [],
                  "publications": {},
                  "metadata": { "edge_bindings": ["eb1"], "inverted_id": null, "is_root": true },
                  "trials": []
                }
              },
              "updates": [
                { "id": "e1", "transforms": [(edge) => { edge.predicate = "biolink:treats"; return edge; }] }
              ]
            },
            "scores": { "nb1": [{ "normalized_score": 0.5 }] },
            "errors": {}
          },
          {
            "agents": ["infores:ara2"],
            "paths": [
              new SummaryPath(["nb1", "e1", "nb2"]),
              new SummaryPath(["nb1", "e2", "nb3", "e1", "nb2"])
            ],
            "nodes": [
              {
                "id": "nb1",
                "transforms": [
                  (node) => { node.types.push("biolink:SmallMolecule"); return node; },
                  (node) => { node.synonyms.push("aspirin"); return node; }
                ]
              }
            ],
            "edges": {
              "base": {
                "e1": {
                  "id": "e1",
                  "aras": [],
                  "is_root": true,
                  "knowledge_level": null,
                  "description": null,
                  "subject": null,
                  "object": null,
                  "predicate": null,
                  "predicate_url": null,
                  "provenance": [],
                  "publications": {},
                  "metadata": { "edge_bindings": ["eb2"], "inverted_id": null, "is_root": true },
                  "trials": []
                }
              },
              "updates": [
                { "id": "e1", "transforms": [(edge) => { edge.provenance.push({ "infores": "infores:kp1", "records": [] }); return edge; }] }
              ]
            },
            "scores": { "nb1": [{ "normalized_score": 0.7 }] },
            "errors": { "infores:ara2": ["analysis failed"] }
          }
        ],
        1,
        { "infores:ara1": ["rule failed"] }
      ],
      "expected": {
        "results": [
          { "paths": ["1cf60ec6", "0ac80c4a"] }
        ],
        "paths": {
          "1cf60ec6": {
            "id": "1cf60ec6",
            "score": 0,
            "subgraph": ["nb1", "e1", "nb2"],
            "aras": ["infores:ara1", "infores:ara2"]
          },
          "0ac80c4a": {
            "id": "0ac80c4a",
            "score": 0,
            "subgraph": ["nb1", "e2", "nb3", "e1", "nb2"],
            "aras": ["infores:ara2"]
          }
        },
        "nodes": {
          "nb1": {
            "id": "nb1",
            "aras": ["infores:ara1", "infores:ara2", "infores:ara2"],
            "descriptions": [],
            "names": ["drug"],
            "types": ["biolink:SmallMolecule", "biolink:ChemicalEntity"],
            "synonyms": ["aspirin"],
            "curies": [],
            "provenance": [],
            "source_time": null
          },
          "nb2": {
            "id": "nb2",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": ["disease"],
            "types": ["biolink:Disease"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "source_time": null
          }
        },
        "edges": {
          "e1": {
            "id": "e1",
            "aras": ["infores:ara1", "infores:ara2"],
            "is_root": true,
            "knowledge_level": null,
            "description": null,
            "subject": null,
            "object": null,
            "predicate": "biolink:treats",
            "predicate_url": null,
            "provenance": [{ "infores": "infores:kp1", "records": [] }],
            "publications": {},
            "metadata": { "edge_bindings": ["eb1", "eb2"], "inverted_id": null, "is_root": true },
            "trials": [],
            "source_time": null
          }
        },
        "scores": { "nb1": [{ "normalized_score": 0.5 }, { "normalized_score": 0.7 }] },
        "errors": { "infores:ara1": ["rule failed"], "infores:ara2": ["analysis failed"] }
      }
    },
    "pathfinder-groups-results-by-sorted-metapath": {
      "args": [
        [
          {
            "agents": ["infores:ara1"],
            "paths": [
              new SummaryPath(["nb1", "e1", "nb2"]),
              new SummaryPath(["nb3", "e2", "nb2"])
            ],
            "nodes": [
              {
                "id": "nb1",
                "transforms": [(node) => { node.types.push("biolink:ChemicalEntity", "biolink:SmallMolecule"); return node; }]
              },
              {
                "id": "nb2",
                "transforms": [(node) => { node.types.push("biolink:Disease"); return node; }]
              },
              {
                "id": "nb3",
                "transforms": [(node) => { node.types.push("biolink:SmallMolecule"); return node; }]
              }
            ],
            "edges": { "base": {}, "updates": [] },
            "scores": {},
            "errors": {}
          }
        ],
        3,
        {}
      ],
      "expected": {
        "results": [
          { "paths": ["1cf60ec6", "382e3f90"] }
        ],
        "paths": {
          "1cf60ec6": {
            "id": "1cf60ec6",
            "score": 0,
            "subgraph": ["nb1", "e1", "nb2"],
            "aras": ["infores:ara1"]
          },
          "382e3f90": {
            "id": "382e3f90",
            "score": 0,
            "subgraph": ["nb3", "e2", "nb2"],
            "aras": ["infores:ara1"]
          }
        },
        "nodes": {
          "nb1": {
            "id": "nb1",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": [],
            "types": ["biolink:SmallMolecule", "biolink:ChemicalEntity"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "source_time": null
          },
          "nb2": {
            "id": "nb2",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": [],
            "types": ["biolink:Disease"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "source_time": null
          },
          "nb3": {
            "id": "nb3",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": [],
            "types": ["biolink:SmallMolecule"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "source_time": null
          }
        },
        "edges": {},
        "scores": {},
        "errors": {}
      }
    }
  });
}