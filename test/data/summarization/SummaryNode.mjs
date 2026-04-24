export { suite }

import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    SummaryNode: _test_SummaryNode()
  }
};

function _test_SummaryNode() {
  return test.make_class_test({
    default_constructor: {
      constructor: {
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
      constructor: {
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
      constructor: {
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
      constructor: { args: [] },
      steps: [
        { method: "name", args: [], expected: null }
      ]
    },
    get_specific_type_default_is_named_thing: {
      constructor: { args: [] },
      steps: [
        { method: "get_specific_type", args: [], expected: "Named Thing" }
      ]
    }
  });
}
