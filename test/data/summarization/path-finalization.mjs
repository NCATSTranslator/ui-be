export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import { SummaryNode } from "#lib/summarization/SummaryNode.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";
import { SummaryPath } from "#lib/summarization/SummaryPath.mjs";
import { MissingInforesError } from "#lib/summarization/summary-fragment.mjs";
import { biolink_config } from "#test/data/summarization/kgraphs.mjs";

await bl.load_biolink(biolink_config());

const suite = {
  tests: {
    finalize_paths: _test_finalize_paths()
  }
};

function _test_finalize_paths() {
  return test.make_function_test({
    "tags-path-and-drops-dangling-path": {
      "args": [
        {
          "4969d8d9": Object.assign(new SummaryPath(["nb1", "e1", "nb2", "e2", "nb3"]), {
            "aras": ["infores:aragorn", "infores:aragorn", "infores:arax"]
          }),
          "086bbe32": Object.assign(new SummaryPath(["nb1", "e9", "nb3"]), {
            "aras": ["infores:aragorn"]
          })
        },
        {
          "nb1": SummaryNode.from_object({
            "id": "nb1", "aras": [], "descriptions": [], "names": [], "types": ["biolink:Gene"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb2": SummaryNode.from_object({
            "id": "nb2", "aras": [], "descriptions": [], "names": [], "types": ["biolink:Protein"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb3": SummaryNode.from_object({
            "id": "nb3", "aras": [], "descriptions": [], "names": [], "types": ["biolink:SmallMolecule"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          })
        },
        {
          "e1": SummaryEdge.from_object({
            "id": "e1", "aras": [], "is_root": true, "knowledge_level": null, "description": null,
            "subject": null, "object": null, "predicate": null, "predicate_url": null, "provenance": [],
            "publications": {}, "metadata": null, "trials": [],
            "tags": { "e/ev/other": { "id": "e/ev/other", "description": { "name": "Other", "description": "" } } }
          }),
          "e2": SummaryEdge.from_object({
            "id": "e2", "aras": [], "is_root": true, "knowledge_level": null, "description": null,
            "subject": null, "object": null, "predicate": null, "predicate_url": null, "provenance": [],
            "publications": {}, "metadata": null, "trials": [],
            "tags": { "e/ev/publications": { "id": "e/ev/publications", "description": { "name": "Publications", "description": "" } } }
          })
        },
        2
      ],
      "expected": {
        "4969d8d9": {
          "id": "4969d8d9",
          "score": 0,
          "subgraph": ["nb1", "e1", "nb2", "e2", "nb3"],
          "aras": ["infores:aragorn", "infores:arax"],
          "tags": {
            "p/ev/other": { "id": "p/ev/other", "description": { "name": "Other", "description": "" } },
            "p/ev/publications": { "id": "p/ev/publications", "description": { "name": "Publications", "description": "" } },
            "p/pc/Protein": { "id": "p/pc/Protein", "description": { "name": "Protein", "description": "" } },
            "p/ara/infores:aragorn": { "id": "p/ara/infores:aragorn", "description": { "name": "ARAGORN", "description": "" } },
            "p/ara/infores:arax": { "id": "p/ara/infores:arax", "description": { "name": "ARAX Translator Reasoner", "description": "" } },
            "p/pt/2": { "id": "p/pt/2", "description": { "name": "2 Connections", "description": "" } }
          }
        }
      }
    },
    "unknown-agent-throws": {
      "args": [
        {
          "1cf60ec6": Object.assign(new SummaryPath(["nb1", "e1", "nb2"]), {
            "aras": ["infores:not-real"]
          })
        },
        {
          "nb1": SummaryNode.from_object({
            "id": "nb1", "aras": [], "descriptions": [], "names": [], "types": ["biolink:Gene"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          }),
          "nb2": SummaryNode.from_object({
            "id": "nb2", "aras": [], "descriptions": [], "names": [], "types": ["biolink:SmallMolecule"],
            "synonyms": [], "curies": [], "provenance": [], "tags": {}
          })
        },
        {
          "e1": SummaryEdge.from_object({
            "id": "e1", "aras": [], "is_root": true, "knowledge_level": null, "description": null,
            "subject": null, "object": null, "predicate": null, "predicate_url": null, "provenance": [],
            "publications": {}, "metadata": null, "trials": [], "tags": {}
          })
        },
        2
      ],
      "expected": MissingInforesError
    }
  });
}
