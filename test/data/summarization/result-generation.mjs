export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import * as taglib from "#lib/taglib.mjs";
import { SummaryNode } from "#lib/summarization/SummaryNode.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";
import { SummaryPath } from "#lib/summarization/SummaryPath.mjs";
import { biolink_config } from "#test/data/summarization/kgraphs.mjs";

await bl.load_biolink(biolink_config());

const suite = {
  tests: {
    generate_results: _test_generate_results(),
    gen_metapath: _test_gen_metapath()
  }
};

function _test_generate_results() {
  return test.make_function_test({
    "builds-results-from-root-paths-and-prunes": {
      "args": [
        [
          { "paths": ["d192b476", "1cf60ec6", "deadbeef"] },
          { "paths": ["382e3f90"] }
        ],
        {
          "1cf60ec6": taglib.tags_from_raw_obj(Object.assign(new SummaryPath(["nb1", "e1", "nb2"]), { "aras": ["infores:ara1"] }), {
            "p/ev/other": { "id": "p/ev/other", "description": { "name": "Other", "description": "" } },
            "r/cc/drug": { "id": "r/cc/drug", "description": { "name": "Drug", "description": "" } }
          }),
          "d192b476": taglib.tags_from_raw_obj(Object.assign(new SummaryPath(["nb1", "e2", "nb3", "e3", "nb2"]), { "aras": ["infores:ara1"] }), {
            "p/ev/publications": { "id": "p/ev/publications", "description": { "name": "Publications", "description": "" } }
          }),
          "382e3f90": Object.assign(new SummaryPath(["nb3", "e2", "nb2"]), { "aras": ["infores:ara2"] })
        },
        {
          "nb1": SummaryNode.from_object({
            "id": "nb1", "aras": [], "descriptions": [], "names": ["aspirin"], "types": ["biolink:SmallMolecule"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb2": SummaryNode.from_object({
            "id": "nb2", "aras": [], "descriptions": [], "names": ["diabetes"], "types": ["biolink:Disease"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb3": SummaryNode.from_object({
            "id": "nb3", "aras": [], "descriptions": [], "names": ["PTGS1"], "types": ["biolink:Gene"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb9": SummaryNode.from_object({
            "id": "nb9", "aras": [], "descriptions": [], "names": ["orphan"], "types": ["biolink:Gene"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          })
        },
        {
          "e1": SummaryEdge.from_object({
            "id": "e1", "aras": [], "is_root": true, "knowledge_level": null, "description": null, "subject": null,
            "object": null, "predicate": null, "predicate_url": null, "provenance": [], "publications": {},
            "metadata": null, "trials": [], "tags": {}
          }),
          "e2": SummaryEdge.from_object({
            "id": "e2", "aras": [], "is_root": false, "knowledge_level": null, "description": null, "subject": null,
            "object": null, "predicate": null, "predicate_url": null, "provenance": [], "publications": {},
            "metadata": null, "trials": [], "tags": {}
          }),
          "e3": SummaryEdge.from_object({
            "id": "e3", "aras": [], "is_root": true, "knowledge_level": null, "description": null, "subject": null,
            "object": null, "predicate": null, "predicate_url": null, "provenance": [], "publications": {},
            "metadata": null, "trials": [], "tags": {}
          }),
          "e9": SummaryEdge.from_object({
            "id": "e9", "aras": [], "is_root": true, "knowledge_level": null, "description": null, "subject": null,
            "object": null, "predicate": null, "predicate_url": null, "provenance": [], "publications": {},
            "metadata": null, "trials": [], "tags": {}
          })
        },
        { "nb1": [{ "normalized_score": 0.5 }] },
        {},
        1
      ],
      "expected": {
        "results": [
          {
            "id": "106e2c4b",
            "subject": "nb1",
            "drug_name": "aspirin",
            "paths": ["1cf60ec6"],
            "object": "nb2",
            "scores": [{ "normalized_score": 0.5 }],
            "tags": { "p/ev/publications": null, "p/ev/other": null, "r/cc/drug": null }
          }
        ],
        "tags": {
          "p/ev/publications": { "name": "Publications", "description": "" },
          "p/ev/other": { "name": "Other", "description": "" },
          "r/cc/drug": { "name": "Drug", "description": "" }
        },
        "path_ids": ["1cf60ec6", "d192b476", "382e3f90"],
        "node_ids": ["nb1", "nb2", "nb3"],
        "edge_ids": ["e1", "e2", "e3"],
        "errors": { "infores:ara2": ["No root paths found"] }
      },
      post: _project_output
    },
    "pathfinder-names-and-ids-by-metapath": {
      "args": [
        [
          { "paths": ["1cf60ec6"] }
        ],
        {
          "1cf60ec6": Object.assign(new SummaryPath(["nb1", "e1", "nb2"]), { "aras": ["infores:ara1"] })
        },
        {
          "nb1": SummaryNode.from_object({
            "id": "nb1", "aras": [], "descriptions": [], "names": ["aspirin"], "types": ["biolink:SmallMolecule"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb2": SummaryNode.from_object({
            "id": "nb2", "aras": [], "descriptions": [], "names": ["diabetes"], "types": ["biolink:Disease"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          })
        },
        {
          "e1": SummaryEdge.from_object({
            "id": "e1", "aras": [], "is_root": true, "knowledge_level": null, "description": null, "subject": null,
            "object": null, "predicate": null, "predicate_url": null, "provenance": [], "publications": {},
            "metadata": null, "trials": [], "tags": {}
          })
        },
        { "nb1": [{ "normalized_score": 0.5 }] },
        {},
        3
      ],
      "expected": {
        "results": [
          {
            "id": "d0830e72",
            "subject": "nb1",
            "drug_name": "aspirin/diabetes",
            "paths": ["1cf60ec6"],
            "object": "nb2",
            "scores": [{ "normalized_score": 0.5 }],
            "tags": {}
          }
        ],
        "tags": {},
        "path_ids": ["1cf60ec6"],
        "node_ids": ["nb1", "nb2"],
        "edge_ids": ["e1"],
        "errors": {}
      },
      post: _project_output
    }
  });
}

function _test_gen_metapath() {
  return test.make_function_test({
    "most-specific-type-per-node": {
      "args": [
        ["nb1", "e1", "nb2", "e2", "nb3"],
        {
          "nb1": SummaryNode.from_object({
            "id": "nb1",
            "aras": [],
            "descriptions": [],
            "names": [],
            "types": ["biolink:SmallMolecule", "biolink:ChemicalEntity"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {}
          }),
          "nb2": SummaryNode.from_object({
            "id": "nb2",
            "aras": [],
            "descriptions": [],
            "names": [],
            "types": ["biolink:Gene"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {}
          }),
          "nb3": SummaryNode.from_object({
            "id": "nb3",
            "aras": [],
            "descriptions": [],
            "names": [],
            "types": ["biolink:Disease"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {}
          })
        }
      ],
      "expected": ["SmallMolecule", "Gene", "Disease"]
    }
  });
}

function _project_output({actual}) {
  return {
    "results": actual.results,
    "tags": actual.tags,
    "path_ids": Object.keys(actual.paths),
    "node_ids": Object.keys(actual.nodes),
    "edge_ids": Object.keys(actual.edges),
    "errors": actual.errors
  };
}
