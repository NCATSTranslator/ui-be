export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import * as taglib from "#lib/taglib.mjs";
import { SummaryEdge } from "#lib/summarization/SummaryEdge.mjs";
import { biolink_config } from "#test/data/summarization/kgraphs.mjs";

await bl.load_biolink(biolink_config());

const suite = {
  tests: {
    finalize_edges: _test_finalize_edges(),
    _filter_redundant_provenance: _test_filter_redundant_provenance()
  },
  skip: {
    SummaryPublication: true
  }
};

function _test_finalize_edges() {
  return test.make_function_test({
    "finalizes-valid-edge": {
      "args": [
        {
          "e1": Object.assign(SummaryEdge.from_object({
            "id": "e1",
            "aras": ["infores:ara1"],
            "is_root": true,
            "knowledge_level": null,
            "description": null,
            "subject": "nb1",
            "object": "nb2",
            "predicate": "treats",
            "predicate_url": null,
            "provenance": [{ "infores": "infores:gtopdb", "records": [] }],
            "publications": {
              "trusted": [
                { "id": "PMID:1", "infores": "infores:gtopdb" },
                { "id": "PMID:1", "infores": "infores:gtopdb" }
              ]
            },
            "metadata": { "edge_bindings": ["eb1"], "inverted_id": null, "is_root": true },
            "trials": [],
            "tags": {}
          }), {
            "supporting_text": { "PMID:1": { "text": "t", "subject": [1, 2], "object": [3, 4] } },
            "supporting_trials": { "NCT01": { "phase": 2 } },
            "qualifiers": false
          })
        },
        {}
      ],
      "expected": {
        "edges": {
          "e1": {
            "id": "e1",
            "aras": ["infores:ara1"],
            "is_root": true,
            "knowledge_level": "trusted",
            "description": "*",
            "subject": "nb1",
            "object": "nb2",
            "predicate": "treats",
            "predicate_url": "https://biolink.github.io/biolink-model/treats/",
            "provenance": [{ "infores": "infores:gtopdb", "records": [] }],
            "publications": {
              "trusted": [
                {
                  "id": "PMID:1",
                  "support": { "text": "t", "subject": [1, 2], "object": [3, 4] },
                  "infores": "infores:gtopdb"
                }
              ]
            },
            "metadata": { "edge_bindings": ["eb1"], "inverted_id": null, "is_root": true },
            "trials": ["NCT01"],
            "source_time": null,
            "tags": {
              "e/ev/publications": { "id": "e/ev/publications", "description": { "name": "Publications", "description": "" } },
              "e/ev/clinical": { "id": "e/ev/clinical", "description": { "name": "Clinical Trials", "description": "" } },
              "e/ev/other": { "id": "e/ev/other", "description": { "name": "Other", "description": "" } },
              "e/pred/treats": { "id": "e/pred/treats", "description": { "name": "treats", "description": "" } }
            }
          }
        },
        "publications": {
          "PMID:1": {
            "type": "PMID",
            "url": "https://www.ncbi.nlm.nih.gov/pubmed/1",
            "infores": ["infores:gtopdb"]
          }
        },
        "trials": {
          "NCT01": {
            "phase": 2,
            "id": "NCT01",
            "url": "https://clinicaltrials.gov/ct2/show/NCT01"
          }
        },
        "provenance": {
          "infores:gtopdb": {
            "name": "Guide to Pharmacology Database",
            "wiki": "https://github.com/NCATSTranslator/Translator-All/wiki/GtoPdb",
            "url": null,
            "knowledge_level": "trusted"
          }
        },
        "errors": {}
      },
      post: _to_raw_output
    },
    "drops-edge-without-provenance": {
      "args": [
        {
          "e1": SummaryEdge.from_object({
            "id": "e1",
            "aras": ["infores:ara1"],
            "is_root": true,
            "knowledge_level": null,
            "description": null,
            "subject": "nb1",
            "object": "nb2",
            "predicate": "treats",
            "predicate_url": null,
            "provenance": [],
            "publications": {},
            "metadata": { "edge_bindings": ["eb1"], "inverted_id": null, "is_root": true },
            "trials": [],
            "tags": {}
          })
        },
        { "infores:ara1": ["rule failed"] }
      ],
      "expected": {
        "edges": {},
        "publications": {},
        "trials": {},
        "provenance": {},
        "errors": { "infores:ara1": ["rule failed", "No provenance for edge: nb1-treats-nb2"] }
      },
      post: _to_raw_output
    },
    "drops-edge-with-unknown-infores-under-unknown-agent": {
      "args": [
        {
          "e1": Object.assign(SummaryEdge.from_object({
            "id": "e1",
            "aras": ["infores:ara1", "infores:ara2"],
            "is_root": true,
            "knowledge_level": null,
            "description": null,
            "subject": "nb1",
            "object": "nb2",
            "predicate": "treats",
            "predicate_url": null,
            "provenance": [{ "infores": "infores:not-real", "records": [] }],
            "publications": {},
            "metadata": { "edge_bindings": ["eb1"], "inverted_id": null, "is_root": true },
            "trials": [],
            "tags": {}
          }), {
            "supporting_text": {},
            "supporting_trials": {},
            "qualifiers": false
          })
        },
        {}
      ],
      "expected": {
        "edges": {},
        "publications": {},
        "trials": {},
        "provenance": {},
        "errors": { "unknown": ["Found invalid infores: infores:not-real on edge nb1-treats-nb2"] }
      },
      post: _to_raw_output
    }
  });
}

function _test_filter_redundant_provenance() {
  return test.make_function_test({
    "drops-sources-without-records-when-another-has-them": {
      "args": [
        {
          "provenance": [
            { "infores": "infores:a", "records": [] },
            { "infores": "infores:a", "records": ["https://example.org/a/1"] },
            { "infores": "infores:b", "records": [] }
          ]
        }
      ],
      "expected": {
        "provenance": [
          { "infores": "infores:a", "records": ["https://example.org/a/1"] },
          { "infores": "infores:b", "records": [] }
        ]
      }
    }
  });
}

function _to_raw_output({actual}) {
  for (const edge of Object.values(actual.edges)) {
    edge.tags = taglib.tags_to_raw_obj(edge);
  }
  for (const pid of Object.keys(actual.publications)) {
    actual.publications[pid] = actual.publications[pid].to_obj();
  }
  return actual;
}
