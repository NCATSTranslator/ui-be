export {suite}

import * as test from "#test/lib/common.mjs";
import { API_KEY_PREFIX, generate_api_key } from "#model/ApiKey.mjs";

const _VALID_KEY = generate_api_key();
const _VALID_BODY = _VALID_KEY.slice(API_KEY_PREFIX.length);

const suite = {
  tests: {
    is_api_key_syntactically_valid: _test_is_api_key_syntactically_valid()
  },
  skip: {
    ApiKey: true,
    API_KEY_PREFIX: true,
    generate_api_key: true,
    hash_api_key: true,
    display_api_key: true
  }
}

function _test_is_api_key_syntactically_valid() {
  return test.make_function_test({
    generated_key: {
      args: [_VALID_KEY],
      expected: true
    },
    one_character_too_long: {
      args: [`${_VALID_KEY}x`],
      expected: false
    },
    grossly_too_long: {
      args: [`${_VALID_KEY}${'x'.repeat(4096)}`],
      expected: false
    },
    one_character_too_short: {
      args: [`${API_KEY_PREFIX}${_VALID_BODY.slice(0, -1)}`],
      expected: false
    },
    trailing_newline: {
      args: [`${_VALID_KEY}\n`],
      expected: false
    },
    missing_prefix: {
      args: [_VALID_BODY],
      expected: false
    },
    wrong_prefix: {
      args: [`key_${_VALID_BODY}`],
      expected: false
    },
    illegal_body_character: {
      args: [`${API_KEY_PREFIX}${_VALID_BODY.slice(0, -1)}+`],
      expected: false
    },
    prefix_only: {
      args: [API_KEY_PREFIX],
      expected: false
    },
    empty_string: {
      args: [''],
      expected: false
    },
    non_string: {
      args: [12345],
      expected: false
    },
    null_key: {
      args: [null],
      expected: false
    },
    undefined_key: {
      args: [undefined],
      expected: false
    }
  });
}
