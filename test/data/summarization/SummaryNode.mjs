export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    SummaryNode: _test_SummaryNode()
  }
};

function _test_SummaryNode() {
  return test.make_class_test({
    default_class_constructor: {
      class_constructor: {
        args: [],
        expected: {
          aras: [],
          descriptions: [],
          names: [],
          types: [],
          synonyms: [],
          curies: [],
          provenance: []
        }
      }
    },
    null_agents_defaults_to_empty: {
      class_constructor: {
        args: [null],
        expected: {
          aras: [],
          descriptions: [],
          names: [],
          types: [],
          synonyms: [],
          curies: [],
          provenance: []
        }
      }
    },
    with_agents: {
      class_constructor: {
        args: [["ara1", "ara2"]],
        expected: {
          aras: ["ara1", "ara2"],
          descriptions: [],
          names: [],
          types: [],
          synonyms: [],
          curies: [],
          provenance: []
        }
      }
    },
    name_returns_null_when_empty: {
      class_constructor: { args: [] },
      steps: [
        { method: "name", args: [], expected: null }
      ]
    },
    name_returns_first_name_when_populated: {
      injected: {
        names: ["Gene A", "Gene B"]
      },
      steps: [
        { method: "name", args: [], expected: "Gene A" }
      ]
    },
    name_returns_first_curie_when_names_empty: {
      injected: {
        curies: ["NCBIGene:1234", "NCBIGene:5678"]
      },
      steps: [
        { method: "name", args: [], expected: "NCBIGene:1234" }
      ]
    },
    name_prefers_name_over_curie: {
      injected: {
        names: ["Gene A"],
        curies: ["NCBIGene:1234"]
      },
      steps: [
        { method: "name", args: [], expected: "Gene A" }
      ]
    },
    get_specific_type_default_is_named_thing: {
      class_constructor: { args: [] },
      steps: [
        { method: "get_specific_type", args: [], expected: "Named Thing" }
      ]
    },
    get_specific_type_strips_biolink_prefix: {
      injected: {
        types: ["biolink:Gene"]
      },
      steps: [
        { method: "get_specific_type", args: [], expected: "Gene" }
      ]
    },
    get_specific_type_replaces_underscores: {
      injected: {
        types: ["biolink:small_molecule", "biolink:Disease"]
      },
      steps: [
        { method: "get_specific_type", args: [], expected: "small molecule" }
      ]
    }
  });
}
