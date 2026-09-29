export {suite}

import * as test from "#test/lib/common.mjs";
import { API_KEY_PREFIX, generate_api_key } from "#model/ApiKey.mjs";

const _VALID_KEY = generate_api_key();
const _VALID_BODY = _VALID_KEY.slice(API_KEY_PREFIX.length);

const suite = {
  tests: {
    is_api_key_syntactically_valid: _test_is_api_key_syntactically_valid(),
    api_key_expiry: _test_api_key_expiry(),
    parse_api_key_expiry: _test_parse_api_key_expiry()
  },
  skip: {
    ApiKey: true,
    ApiKeyLimitError: true,
    API_KEY_NAME_MAX_LEN: true,
    API_KEY_DEFAULT_TTL_DAYS: true,
    API_KEY_MAX_TTL_DAYS: true,
    API_KEYS_MAX_ACTIVE_PER_USER: true,
    API_KEY_PREFIX: true,
    generate_api_key: true,
    hash_api_key: true,
    display_api_key: true
  }
}

function _test_api_key_expiry() {
  const now = new Date('2026-01-01T00:00:00.000Z');
  const iso = ({actual}) => actual.toISOString();
  return test.make_function_test({
    default_is_thirty_days: {
      args: [undefined, now],
      post: iso,
      expected: '2026-01-31T00:00:00.000Z'
    },
    one_day: {
      args: [1, now],
      post: iso,
      expected: '2026-01-02T00:00:00.000Z'
    },
    seven_days: {
      args: [7, now],
      post: iso,
      expected: '2026-01-08T00:00:00.000Z'
    },
    crosses_year_boundary: {
      args: [365, now],
      post: iso,
      expected: '2027-01-01T00:00:00.000Z'
    }
  });
}

function _test_parse_api_key_expiry() {
  const now = new Date('2026-01-01T00:00:00.000Z');
  const iso = ({actual}) => actual === null ? null : actual.toISOString();
  return test.make_function_test({
    future_date_time: {
      args: ['2026-02-15T12:30:00.000Z', now],
      post: iso,
      expected: '2026-02-15T12:30:00.000Z'
    },
    date_only: {
      args: ['2026-03-01', now],
      post: iso,
      expected: '2026-03-01T00:00:00.000Z'
    },
    with_offset: {
      args: ['2026-02-01T00:00:00+02:00', now],
      post: iso,
      expected: '2026-01-31T22:00:00.000Z'
    },
    at_the_maximum: {
      args: ['2027-01-01T00:00:00.000Z', now],
      post: iso,
      expected: '2027-01-01T00:00:00.000Z'
    },
    beyond_the_maximum: {
      args: ['2027-01-01T00:00:00.001Z', now],
      post: iso,
      expected: null
    },
    exactly_now: {
      args: ['2026-01-01T00:00:00.000Z', now],
      post: iso,
      expected: null
    },
    in_the_past: {
      args: ['2025-12-31T23:59:59.999Z', now],
      post: iso,
      expected: null
    },
    unparseable: {
      args: ['soon', now],
      post: iso,
      expected: null
    },
    number: {
      args: [7, now],
      post: iso,
      expected: null
    },
    null_value: {
      args: [null, now],
      post: iso,
      expected: null
    },
    empty_string: {
      args: ['', now],
      post: iso,
      expected: null
    }
  });
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
