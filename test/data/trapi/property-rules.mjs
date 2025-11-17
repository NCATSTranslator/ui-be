export { suite }

import * as cmn from "#lib/common.mjs";
import * as test from "#test/lib/common.mjs";

const suite = {
  tests: {
    make_rule_transform_property: _test_make_rule_transform_property(),
    make_rule_get_property: _test_make_rule_get_property(),
    make_rule_transform_and_aggregate_property: _test_make_rule_transform_and_aggregate_property(),
    make_rule_aggregate_property: _test_make_rule_aggregate_property(),
    make_rule_transform_attribute_value: _test_make_rule_transform_attribute_value(),
    make_rule_map_attributes: _test_make_rule_map_attributes()
  }
};

function _test_make_rule_transform_property() {
  return test.make_function_test({
    get: {
      args: [{
        source_key: "test-prop",
        transform: cmn.jsonGet
      }],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {
          "test-prop": "ok"
        },
        target: {}
      },
      post: test.apply_rule
    },
    get_double: {
      args: [{
        source_key: "test-prop",
        transform: (source, key) => 2 * cmn.jsonGet(source, key)
      }],
      expected: {
        "test-prop": 4
      },
      context: {
        source: {
          "test-prop": 2
        },
        target: {}
      },
      post: test.apply_rule
    },
    no_rewrite: {
      args: [{
        source_key: "test-prop",
        transform: cmn.jsonGet
      }],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {
          "test-prop": "not ok"
        },
        target: {
          "test-prop": "ok"
        }
      },
      post: test.apply_rule
    },
    property_missing_empty_target: {
      args: [{
        source_key: "test-prop",
        transform: cmn.jsonGet
      }],
      expected: {
        "test-prop": null
      },
      context: {
        source: {},
        target: {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_get_property() {
  return test.make_function_test({
    prop_exists: {
      args: ["test-prop"],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {
          "test-prop": "ok"
        },
        target: {}
      },
      post: test.apply_rule
    },
    no_rewrite: {
      args: ["test-prop"],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {
          "test-prop": "not ok"
        },
        target: {
          "test-prop": "ok"
        }
      },
      post: test.apply_rule
    },
    default_value: {
      args: ["test-prop"],
      expected: {
        "test-prop": null
      },
      context: {
        source: {},
        target: {}
      },
      post: test.apply_rule
    },
    no_rewrite_default_value: {
      args: ["test-prop"],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {},
        target: {
          "test-prop": "ok"
        }
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_transform_and_aggregate_property() {
  return test.make_function_test({
    get: {
      args: [{
        source_key: "test-prop",
        target_key: "test-prop",
        transform: cmn.jsonGet
      }],
      expected: {
        "test-prop": ["ok-1", "ok-2", "ok-3"]
      },
      context: {
        source: [
          {"test-prop": "ok-1"},
          {"test-prop": "ok-2"},
          {"test-prop": "ok-3"}
        ],
        target: {}
      },
      post: test.apply_rule
    },
    get_double: {
      args: [{
        source_key: "test-prop",
        target_key: "doubled-prop",
        transform: (source, key) => 2 * cmn.jsonGet(source, key)
      }],
      expected: {
        "doubled-prop": [2, 4, 6],
        "other-prop": "ok"
      },
      context: {
        source: [
          {"test-prop": 1},
          {"test-prop": 2},
          {"test-prop": 3}
        ],
        target: {"other-prop": "ok"}
      },
      post: test.apply_rule
    },
    no_match: {
      args: [{
        source_key: "test-prop",
        target_key: "doubled-prop",
        transform: (source, key) => 2 * cmn.jsonGet(source, key)
      }],
      expected: {
        "doubled-prop": [],
        "other-prop": "ok"
      },
      context: {
        source: [
          {"some-prop": 1},
          {"some-prop": 2},
          {"some-prop": 3}
        ],
        target: {"other-prop": "ok"}
      },
      post: test.apply_rule
    },
    extension: {
      args: [{
        source_key: "test-prop",
        target_key: "doubled-prop",
        transform: (source, key) => 2 * cmn.jsonGet(source, key)
      }],
      expected: {
        "doubled-prop": [2,4,6,8]
      },
      context: {
        source: [
          {"test-prop": 2},
          {"test-prop": 3},
          {"test-prop": 4}
        ],
        target: {"doubled-prop": [2]}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_aggregate_property() {
  return test.make_function_test({
    get: {
      args: [{
        source_key: "test-prop",
        target_key: "test-prop"
      }],
      expected: {
        "test-prop": ["ok-1", "ok-2", "ok-3"]
      },
      context: {
        source: [
          {"test-prop": "ok-1"},
          {"test-prop": "ok-2"},
          {"test-prop": "ok-3"}
        ],
        target: {}
      },
      post: test.apply_rule
    },
    rename: {
      args: [{
        source_key: "test-prop",
        target_key: "diff-prop"
      }],
      expected: {
        "diff-prop": ["ok-1", "ok-2", "ok-3"]
      },
      context: {
        source: [
          {"test-prop": "ok-1"},
          {"test-prop": "ok-2"},
          {"test-prop": "ok-3"}
        ],
        target: {}
      },
      post: test.apply_rule
    },
    no_match: {
      args: [{
        source_key: "test-prop",
        target_key: "test-prop"
      }],
      expected: {
        "test-prop": [],
        "other-prop": "ok"
      },
      context: {
        source: [
          {"some-prop": 1},
          {"some-prop": 2},
          {"some-prop": 3}
        ],
        target: {"other-prop": "ok"}
      },
      post: test.apply_rule
    },
    extension: {
      args: [{
        source_key: "test-prop",
        target_key: "diff-prop"
      }],
      expected: {
        "diff-prop": [1,2,3,3]
      },
      context: {
        source: [
          {"test-prop": 2},
          {"test-prop": 3},
          {"test-prop": 3}
        ],
        target: {"diff-prop": [1]}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_transform_attribute_value() {
  return test.make_function_test({
    identity_and_rename: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": "ok"
      },
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-attr-id",
              value: "ok"
            }
          ]
        },
        target: {}
      },
      post: test.apply_rule
    },
    double_and_rename: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "double-prop",
        transform: (x) => 2 * x
      }],
      expected: {
        "double-prop": 2
      },
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-attr-id",
              value: 1
            }
          ]
        },
        target: {}
      },
      post: test.apply_rule
    },
    no_match: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": null
      },
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "some-attr-id",
              value: 1
            }
          ]
        },
        target: {}
      },
      post: test.apply_rule
    },
    empty_attrs: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": null
      },
      context: {
        source: {
          attributes: []
        },
        target: {}
      },
      post: test.apply_rule
    },
    null_attrs: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": null
      },
      context: {
        source: {
          attributes: null
        },
        target: {}
      },
      post: test.apply_rule
    },
    missing_attrs: {
      args: [{
        attr_id: "test-attr-id",
        target_key: "test-prop",
        transform: (x) => X
      }],
      expected: {
        "test-prop": null
      },
      context: {
        source: {},
        target: {}
      },
      post: test.apply_rule
    }
  });
}

function _test_make_rule_map_attributes() {
  return test.make_function_test({
    identity_single_attr: {
      args: [{
        attr_ids: ["test-attr-id"],
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": [
          {
            attribute_type_id: "test-attr-id",
            value: "ok"
          }
        ]
      },
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-attr-id",
              value: "ok"
            }
          ]
        },
        target: {}
      },
      post: test.apply_rule
    },
    multi_attr: {
      args: [{
        attr_ids: ["test-attr-id-1", "test-attr-id-2"],
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": [
          {
            attribute_type_id: "test-attr-id-1",
            value: 1
          },
          {
            attribute_type_id: "test-attr-id-2",
            value: 2
          },
          {
            attribute_type_id: "test-attr-id-1",
            value: 1
          }
        ]
      },
      context: {
        source: {
          attributes: [
            {
              attribute_type_id: "test-attr-id-1",
              value: 1
            },
            {
              attribute_type_id: "test-attr-id-2",
              value: 2
            },
            {
              attribute_type_id: "test-attr-id-1",
              value: 1
            }
          ]
        },
        target: {}
      },
      post: test.apply_rule
    },
    multi_attr_aggregate: {
      args: [{
        attr_ids: ["test-attr-id-1", "test-attr-id-2"],
        target_key: "test-prop",
        transform: (x) => x
      }],
      expected: {
        "test-prop": [
          {
            attribute_type_id: "test-attr-id-1",
            value: 1
          },
          {
            attribute_type_id: "test-attr-id-2",
            value: 2
          },
          {
            attribute_type_id: "test-attr-id-1",
            value: 3
          },
          {
            attribute_type_id: "test-attr-id-2",
            value: 1
          }
        ]
      },
      context: {
        source: [
          {
            attributes: [
              {
                attribute_type_id: "test-attr-id-1",
                value: 1
              },
              {
                attribute_type_id: "test-attr-id-2",
                value: 2
              }
            ]
          },
          {
            attributes: [
              {
                attribute_type_id: "test-attr-id-1",
                value: 3
              },
              {
                attribute_type_id: "test-attr-id-2",
                value: 1
              }
            ]
          }
        ],
        target: {}
      },
      post: test.apply_rule
    }
  });
}

