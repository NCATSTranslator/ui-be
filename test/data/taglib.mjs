export {suite}

import * as cmn from "#lib/common.mjs";
import * as test from "#test/lib/common.mjs";
import * as taglib from "#lib/taglib.mjs";

const suite = {
  tests: {
    Tag: _test_Tag(),
    make_taggable: _test_make_taggable(),
    set_tag: _test_set_tag(),
    has_tag: _test_has_tag(),
    get_tags: _test_get_tags(),
    merge_tags: _test_merge_tags()
  },
  skip: {
    TAG_ID: true
  }
}

function _test_Tag() {
  return test.make_class_test({
    id_only: {
      constructor: {
        args: [{id: "test-id"}],
        expected: {
          id: "test-id",
          description: {
            name: "",
            description: ""
          }
        }
      }
    },
    id_name: {
      constructor: {
        args: [{id: "test-id", name: "Test ID"}],
        expected: {
          id: "test-id",
          description: {
            name: "Test ID",
            description: ""
          }
        }
      }
    },
    id_name_description: {
      constructor: {
        args: [{id: "test-id", name: "Test ID", description: "Test description"}],
        expected: {
          id: "test-id",
          description: {
            name: "Test ID",
            description: "Test description"
          }
        }
      }
    },
    no_id: {
      constructor: {
        args: [{name: "Test ID", description: "Test description"}],
        expected: cmn.DeveloperError
      }
    }
  });
}

function _test_make_taggable() {
  return test.make_function_test({
    empty_object: {
      args: [{}],
      expected: {
        [taglib.TAG_ID]: {
          _data: {}
        }
      }
    },
    non_object: {
      args: [[]],
      expected: cmn.DeveloperError
    },
    already_tagged: {
      args: [taglib.make_taggable({})],
      expected: cmn.DeveloperError
    }
  });
}

function _test_set_tag() {
  const test_tag = new taglib.Tag({id: "test-id", name: "OK"});
  return test.make_function_test({
    taggable: {
      args: [taglib.make_taggable({}), test_tag],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag
          }
        }
      }
    },
    not_taggable: {
      args: [{}, test_tag],
      expected: cmn.DeveloperError
    }
  });
}

function _test_has_tag() {
  const test_tag = new taglib.Tag({id: "test-id", name: "OK"});
  const other_tag = new taglib.Tag({id: "other-id"});
  const tagged_obj = taglib.set_tag(taglib.make_taggable({}), test_tag);
  const other_obj = taglib.set_tag(taglib.make_taggable({}), other_tag);
  return test.make_function_test({
    has_the_tag: {
      args: [tagged_obj, test_tag],
      expected: true
    },
    no_tag: {
      args: [other_obj, test_tag],
      expected: false
    },
    not_taggable: {
      args: [{}, test_tag],
      expected: cmn.DeveloperError
    }
  });
}

function _test_get_tags() {
  const test_tag = new taglib.Tag({id: "test-id", name: "OK"});
  const other_tag = new taglib.Tag({id: "other-id"});
  const tagged_obj = taglib.set_tag(taglib.make_taggable({}), test_tag);
  taglib.set_tag(tagged_obj, other_tag);
  return test.make_function_test({
    valid: {
      args: [tagged_obj],
      expected: [test_tag, other_tag]
    },
    no_tags: {
      args: [taglib.make_taggable({})],
      expected: []
    },
    not_taggable: {
      args: [{}],
      expected: cmn.DeveloperError
    }
  });
}

function _test_merge_tags() {
  const test_id = "test-id";
  const test_tag_1 = new taglib.Tag({id: test_id, name: "OK"});
  const test_tag_2 = new taglib.Tag({id: "other-id"});
  const test_tag_3 = new taglib.Tag({id: test_id, name: "Test"});
  const to_obj = taglib.set_tag(taglib.make_taggable({}), test_tag_1);
  const from_obj = taglib.set_tag(taglib.make_taggable({}), test_tag_2);
  const another_obj = taglib.set_tag(taglib.make_taggable({}), test_tag_3);
  return test.make_function_test({
    valid_merge_simple: {
      args: [{
        from_obj: from_obj,
        to_obj: to_obj
      }],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag_1,
            "other-id": test_tag_2
          }
        }
      }
    },
    merge_same_id_no_overwrite: {
      args: [{
        from_obj: another_obj,
        to_obj: to_obj,
        overwrite: false
      }],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag_1,
          }
        }
      }
    },
    merge_same_id_overwrite: {
      args: [{
        from_obj: another_obj,
        to_obj: to_obj,
        overwrite: true
      }],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag_3,
          }
        }
      }
    },
    merge_selector_match: {
      args: [{
        from_obj: from_obj,
        to_obj: to_obj,
        selector: (tag) => true
      }],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag_1,
            "other-id": test_tag_2
          }
        }
      }
    },
    merge_selector_fail: {
      args: [{
        from_obj: from_obj,
        to_obj: to_obj,
        selector: (tag) => false
      }],
      expected: {
        [taglib.TAG_ID]: {
          _data: {
            "test-id": test_tag_1
          }
        }
      }
    },
    from_obj_not_taggable: {
      args: [{
        from_obj: {},
        to_obj: to_obj,
      }],
      expected: cmn.DeveloperError
    },
    to_obj_not_taggable: {
      args: [{
        from_obj: from_obj,
        to_obj: {},
      }],
      expected: cmn.DeveloperError
    },
  });
}
