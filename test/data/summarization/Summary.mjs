export { suite }

import * as test from "#test/lib/common.mjs";
import { SummaryPublication } from "#lib/summarization/edge-finalization.mjs";

const suite = {
  tests: {
    Summary: _test_Summary(),
    SummaryMetadata: _test_SummaryMetadata()
  }
};

function _test_Summary() {
  return test.make_class_test({
    "defaults": {
      "class_constructor": {
        "args": [{ "qid": "q1", "aras": [], "timestamp": null }],
        "expected": {
          "meta": { "qid": "q1", "aras": [], "timestamp": null },
          "results": [],
          "paths": {},
          "nodes": {},
          "edges": {},
          "trials": {},
          "provenance": {},
          "tags": {},
          "errors": {},
          "publications": {}
        }
      }
    },
    "converts-publications-to-objects": {
      "class_constructor": {
        "args": [
          { "qid": "q1", "aras": [], "timestamp": null },
          [],
          {},
          {},
          {},
          {
            "PMID:1": Object.assign(new SummaryPublication("PMID", "https://www.ncbi.nlm.nih.gov/pubmed/1"), {
              "infores": new Set(["infores:gtopdb", "infores:arax"])
            })
          }
        ]
      },
      "steps": [
        {
          "get": "publications",
          "expected": {
            "PMID:1": {
              "type": "PMID",
              "url": "https://www.ncbi.nlm.nih.gov/pubmed/1",
              "infores": ["infores:gtopdb", "infores:arax"]
            }
          }
        }
      ]
    },
    "set-timestamp-stamps-meta-nodes-and-edges": {
      "class_constructor": {
        "args": [
          { "qid": "q1", "aras": [], "timestamp": null },
          [],
          {},
          { "nb1": { "id": "nb1", "source_time": null } },
          { "e1": { "id": "e1", "source_time": null } }
        ]
      },
      "steps": [
        { "method": "set_timestamp", "args": ["2026-09-22 03:54:38"], "expected": undefined },
        { "get": "meta", "expected": { "qid": "q1", "aras": [], "timestamp": "2026-09-22 03:54:38" } },
        { "get": "nodes", "expected": { "nb1": { "id": "nb1", "source_time": "2026-09-22 03:54:38" } } },
        { "get": "edges", "expected": { "e1": { "id": "e1", "source_time": "2026-09-22 03:54:38" } } }
      ]
    }
  });
}

function _test_SummaryMetadata() {
  return test.make_class_test({
    "records-qid-and-agents": {
      "class_constructor": {
        "args": ["q1", ["infores:ara1"]],
        "expected": { "qid": "q1", "aras": ["infores:ara1"], "timestamp": null }
      }
    },
    "non-string-qid-throws": {
      "class_constructor": {
        "args": [1, []],
        "expected": TypeError
      }
    },
    "non-array-agents-throws": {
      "class_constructor": {
        "args": ["q1", "infores:ara1"],
        "expected": TypeError
      }
    }
  });
}
