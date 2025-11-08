import * as cmn from '../lib/common.mjs';
import * as test from './lib/common.mjs';
import * as taglib from '../lib/taglib.mjs';

const root_path = './test/data/taglib';
const env = new test.Environment([
  test.Environment.Entry({
    key: '%empty-tagged-object',
    construct: taglib.make_taggable,
    args: [{}]
  }),
  test.Environment.Entry({
    key: '%test-tag-1',
    construct: taglib.Tag,
    args: [{id: 'test/1', name: 'test 1', description: 'Test 1'}]
  }),
  test.Environment.Entry({
    key: '%test-tag-2',
    construct: taglib.Tag,
    args: [{id: 'test/2', name: 'test 2', description: 'Test 2'}]
  }),
  test.Environment.Entry({
    key: '%test-tagged-object',
    construct: taglib.set_tag,
    args: ['%empty-tagged-object', '%test-tag-1']
  })
]);
const suite = {
  make_taggable: {
    non_object: {args: [[]], expected: cmn.DeveloperError},
    already_tagged: {args: ['%empty-tagged-object'], expected: cmn.DeveloperError}
  },
  set_tag: {
    empty_tagged_object: {
      args: ['%empty-tagged-object', '%test-tag-1'],
      expected: {
        __tags__: {
          _data: {
            'test/1': '%test-tag-1'
          }
        }
      }
    },
    replace_tag: {
      args: ['%test-tagged-object', '%test-tag-1'],
      expected: {
        __tags__: {
          _data: {
            'test/1': '%test-tag-1'
          }
        }
      }
    },
    add_tag: {
      args: ['%test-tagged-object', '%test-tag-2'],
      expected: {
        __tags__: {
          _data: {
            'test/1': '%test-tag-1',
            'test/2': '%test-tag-2'
          }
        }
      }
    },
    non_tagged_object: {
      args: [{}, '%test-tag-1'],
      expected: cmn.DeveloperError
    },
    non_object: {
      args: [[], '%test-tag-1'],
      expected: cmn.DeveloperError
    }
  },
  has_tag: {
    empty_tagged_object: {
      args: ['%empty-tagged-object', '%test-tag-1'],
      expected: false
    },
    has_tag: {
      args: ['%test-tagged-object', '%test-tag-1'],
      expected: true
    },
    does_not_have_tag: {
      args: ['%test-tagged-object', '%test-tag-2'],
      expected: false
    },
    non_tagged_object: {
      args: [{}, '%test-tag-1'],
      expected: cmn.DeveloperError
    },
    non_object: {
      args: [[], '%test-tag-1'],
      expected: cmn.DeveloperError
    }
  }
}
await test.module_test({
  suite: suite,
  against: taglib,
  env: env
});
