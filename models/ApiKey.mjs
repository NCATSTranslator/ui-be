'use strict';

import { createHash, randomBytes } from 'node:crypto';
import { v4 as uuidv4 } from 'uuid';
import * as cmn from '../lib/common.mjs';

export {
  ApiKey,
  ApiKeyLimitError,
  API_KEY_PREFIX,
  API_KEY_NAME_MAX_LEN,
  API_KEY_DEFAULT_TTL_DAYS,
  API_KEY_MAX_TTL_DAYS,
  API_KEYS_MAX_ACTIVE_PER_USER,
  api_key_expiry,
  parse_api_key_expiry,
  generate_api_key,
  hash_api_key,
  display_api_key,
  is_api_key_syntactically_valid
};

const API_KEY_PREFIX = 'tkey_';
const API_KEY_NAME_MAX_LEN = 128;
const API_KEY_DEFAULT_TTL_DAYS = 30;
const API_KEY_MAX_TTL_DAYS = 365;
const API_KEYS_MAX_ACTIVE_PER_USER = 10;
const API_KEY_BYTES = 32;
const API_KEY_BODY_LEN = 43;
const API_KEY_LEN = API_KEY_PREFIX.length + API_KEY_BODY_LEN;

function generate_api_key() {
  return `${API_KEY_PREFIX}${randomBytes(API_KEY_BYTES).toString('base64url')}`;
}

function hash_api_key(key) {
  return createHash('sha256').update(key).digest('hex');
}

function display_api_key(key) {
  const body = key.slice(API_KEY_PREFIX.length);
  return `${API_KEY_PREFIX}${body.slice(0, 4)}...${body.slice(-4)}`;
}

function api_key_expiry(days = API_KEY_DEFAULT_TTL_DAYS, now = new Date()) {
  return new Date(now.getTime() + days * cmn.MS_PER_DAY);
}

function parse_api_key_expiry(value, now = new Date()) {
  if (typeof value !== 'string') return null;
  const time_expires = new Date(value);
  if (Number.isNaN(time_expires.getTime())) return null;
  if (time_expires <= now) return null;
  if (time_expires > api_key_expiry(API_KEY_MAX_TTL_DAYS, now)) return null;
  return time_expires;
}

function is_api_key_syntactically_valid(key) {
  return cmn.is_string(key)
    && key.length === API_KEY_LEN
    && key.startsWith(API_KEY_PREFIX)
    && cmn.is_base64url_string(key.slice(API_KEY_PREFIX.length));
}

class ApiKeyLimitError extends Error {
  constructor(msg) {
    super(msg);
    this.name = "ApiKeyLimitError";
  }
}

class ApiKey {
  constructor({
    id = uuidv4(),
    user_id,
    name,
    key_hash,
    key_display,
    time_created = new Date(),
    time_last_used = null,
    time_revoked = null,
    time_expires
  } = {}) {

    if (!user_id) {
      throw new Error("user_id is required");
    } else if (!name) {
      throw new Error("name is required");
    } else if (!key_hash) {
      throw new Error("key_hash is required");
    } else if (!key_display) {
      throw new Error("key_display is required");
    } else if (!time_expires) {
      throw new Error("time_expires is required");
    }

    this.id = id;
    this.user_id = user_id;
    this.name = name;
    this.key_hash = key_hash;
    this.key_display = key_display;
    this.time_created = time_created;
    this.time_last_used = time_last_used;
    this.time_revoked = time_revoked;
    this.time_expires = time_expires;
  }

  static from_raw_key(user_id, name, raw_key, time_expires = api_key_expiry()) {
    return new ApiKey({
      user_id: user_id,
      name: name,
      key_hash: hash_api_key(raw_key),
      key_display: display_api_key(raw_key),
      time_expires: time_expires
    });
  }

  is_revoked() {
    return this.time_revoked !== null;
  }

  is_expired(time = new Date()) {
    return this.time_expires <= time;
  }

  revoke(time = new Date()) {
    this.time_revoked = time;
    return this;
  }

  /* JS serialization hook */
  toJSON() {
    const { key_hash, ...rest } = this;
    return rest;
  }
}
