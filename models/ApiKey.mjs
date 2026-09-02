'use strict';

import { createHash, randomBytes } from 'node:crypto';
import { v4 as uuidv4 } from 'uuid';

export {
  ApiKey,
  API_KEY_PREFIX,
  generate_api_key,
  hash_api_key,
  display_api_key,
  is_api_key_syntactically_valid
};

const API_KEY_PREFIX = 'tkey_';
const API_KEY_BYTES = 32;
const API_KEY_BODY_LEN = 43;
const API_KEY_RX = new RegExp(`^${API_KEY_PREFIX}[A-Za-z0-9_-]{${API_KEY_BODY_LEN}}$`);

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

function is_api_key_syntactically_valid(key) {
  return typeof key === 'string' && API_KEY_RX.test(key);
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
    time_expires = null
  } = {}) {

    if (!user_id) {
      throw new Error("user_id is required");
    } else if (!name) {
      throw new Error("name is required");
    } else if (!key_hash) {
      throw new Error("key_hash is required");
    } else if (!key_display) {
      throw new Error("key_display is required");
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

  static from_raw_key(user_id, name, raw_key, time_expires = null) {
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
    return this.time_expires !== null && this.time_expires <= time;
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
