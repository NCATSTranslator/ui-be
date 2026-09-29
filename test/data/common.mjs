export {suite}

import * as test from "#test/lib/common.mjs";
import * as cmn from "#lib/common.mjs";

const tests = {
  is_base64url_string: _test_is_base64url_string()
};

const suite = {
  tests: tests,
  skip: Object.fromEntries(
    Object.keys(cmn).filter((k) => tests[k] === undefined).map((k) => [k, true]))
};

function _test_is_base64url_string() {
  return test.make_function_test({
    upper_case: {
      args: ['ABCDEFGHIJKLMNOPQRSTUVWXYZ'],
      expected: true
    },
    lower_case: {
      args: ['abcdefghijklmnopqrstuvwxyz'],
      expected: true
    },
    digits: {
      args: ['0123456789'],
      expected: true
    },
    hyphen_and_underscore: {
      args: ['-_'],
      expected: true
    },
    mixed: {
      args: ['aZ09-_'],
      expected: true
    },
    empty_string: {
      args: [''],
      expected: true
    },
    plus_sign: {
      args: ['abc+'],
      expected: false
    },
    slash: {
      args: ['abc/'],
      expected: false
    },
    equals_padding: {
      args: ['abc='],
      expected: false
    },
    space: {
      args: ['ab c'],
      expected: false
    },
    trailing_newline: {
      args: ['abc\n'],
      expected: false
    },
    illegal_char_at_start: {
      args: ['!abc'],
      expected: false
    },
    non_ascii: {
      args: ['abcé'],
      expected: false
    },
    number: {
      args: [123],
      expected: false
    },
    array_of_valid_chars: {
      args: [['a', 'b']],
      expected: false
    },
    null_value: {
      args: [null],
      expected: false
    },
    undefined_value: {
      args: [undefined],
      expected: false
    }
  });
}
