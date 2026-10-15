export { suite }

import * as test from "#test/lib/common.mjs";
import * as bl from "#lib/biolink-model.mjs";
import * as taglib from "#lib/taglib.mjs";
import { SummaryNode } from "#lib/summarization/SummaryNode.mjs";
import { biolink_config } from "#test/data/summarization/kgraphs.mjs";

await bl.load_biolink(biolink_config());

const suite = {
  tests: {
    finalize_nodes: _test_finalize_nodes()
  }
};

function _test_finalize_nodes() {
  return test.make_function_test({
    "chemical-trials-link-to-disease-in-graph": {
      "args": [
        {
          "CHEBI:15365": SummaryNode.from_object({
            "id": "CHEBI:15365",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": ["aspirin", "aspirin"],
            "types": ["biolink:ChemicalEntity", "biolink:SmallMolecule"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {},
            "annotations": {
              "chemical": {
                "curies": null,
                "approval": null,
                "descriptions": null,
                "indications": null,
                "synonyms": null,
                "roles": null,
                "otc_status": null,
                "clinical_trials": {
                  "value": [{ "id": "NCT01", "disease_ids": ["MONDO:0005148", "MONDO:9999999"] }],
                  "metadata": { "sources": [] }
                }
              }
            }
          }),
          "MONDO:0005148": SummaryNode.from_object({
            "id": "MONDO:0005148",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": [],
            "types": ["biolink:Disease"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {},
            "annotations": {
              "disease": {
                "descriptions": null,
                "synonyms": null,
                "clinical_trials": null,
                "curies": {
                  "value": [{ "curie": "MESH:D003924", "url": null }],
                  "metadata": { "sources": [] }
                }
              }
            }
          })
        }
      ],
      "expected": {
        "CHEBI:15365": {
          "id": "CHEBI:15365",
          "aras": ["infores:ara1"],
          "descriptions": [],
          "names": ["aspirin"],
          "types": ["biolink:SmallMolecule", "biolink:ChemicalEntity"],
          "synonyms": [],
          "curies": ["CHEBI:15365"],
          "provenance": ["http://purl.obolibrary.org/obo/CHEBI_15365"],
          "source_time": null,
          "annotations": {
            "chemical": {
              "curies": {
                "value": [{ "curie": "CHEBI:15365", "url": "http://purl.obolibrary.org/obo/CHEBI_15365" }],
                "metadata": { "sources": null }
              },
              "approval": null,
              "descriptions": null,
              "indications": null,
              "synonyms": null,
              "roles": null,
              "otc_status": null,
              "clinical_trials": { "value": ["NCT01"], "metadata": { "sources": [] } }
            }
          },
          "tags": {}
        },
        "MONDO:0005148": {
          "id": "MONDO:0005148",
          "aras": ["infores:ara1"],
          "descriptions": [],
          "names": ["MONDO:0005148"],
          "types": ["biolink:Disease"],
          "synonyms": [],
          "curies": ["MONDO:0005148", "MESH:D003924"],
          "provenance": ["http://purl.obolibrary.org/obo/MONDO_0005148"],
          "source_time": null,
          "annotations": {
            "disease": {
              "descriptions": null,
              "synonyms": null,
              "clinical_trials": { "value": ["NCT01"], "metadata": { "sources": null } },
              "curies": {
                "value": [
                  { "curie": "MONDO:0005148", "url": "http://purl.obolibrary.org/obo/MONDO_0005148" },
                  { "curie": "MESH:D003924", "url": "http://id.nlm.nih.gov/mesh/D003924" }
                ],
                "metadata": { "sources": [] }
              }
            }
          },
          "tags": {}
        }
      },
      post: _tags_to_raw
    },
    "protein-with-gene-type-is-conflated-and-tagged": {
      "args": [
        {
          "UniProtKB:P23219": SummaryNode.from_object({
            "id": "UniProtKB:P23219",
            "aras": ["infores:ara1"],
            "descriptions": [],
            "names": ["PTGS1"],
            "types": ["biolink:Gene", "biolink:Protein"],
            "synonyms": [],
            "curies": [],
            "provenance": [],
            "tags": {},
            "annotations": {
              "gene": {
                "curies": null,
                "descriptions": null,
                "name": null,
                "species": null,
                "tdl": { "value": ["Tclin"], "metadata": { "sources": [] } }
              }
            }
          })
        }
      ],
      "expected": {
        "UniProtKB:P23219": {
          "id": "UniProtKB:P23219",
          "aras": ["infores:ara1"],
          "descriptions": [],
          "names": ["PTGS1"],
          "types": ["biolink:Gene", "biolink:Protein"],
          "synonyms": [],
          "curies": ["UniProtKB:P23219"],
          "provenance": ["http://purl.uniprot.org/uniprot/P23219"],
          "source_time": null,
          "annotations": {
            "gene": {
              "curies": {
                "value": [{ "curie": "UniProtKB:P23219", "url": "http://purl.uniprot.org/uniprot/P23219" }],
                "metadata": { "sources": null }
              },
              "descriptions": null,
              "name": null,
              "species": null,
              "tdl": { "value": ["Tclin"], "metadata": { "sources": [] } }
            }
          },
          "tags": {
            "n/tdl/tclin": { "id": "n/tdl/tclin", "description": { "name": "Tclin (approved drug)", "description": "" } }
          }
        }
      },
      post: _tags_to_raw
    }
  });
}

function _tags_to_raw({actual}) {
  for (const node of Object.values(actual)) {
    node.tags = taglib.tags_to_raw_obj(node);
  }
  return actual;
}
