'use strict';

import { createHash, randomBytes } from 'node:crypto';
import { v4 as uuidv4 } from 'uuid';

export {
  ApiKey,
  API_KEY_PREFIX,
  generateApiKey,
  hashApiKey,
  displayApiKey,
  isApiKeySyntacticallyValid
};

const API_KEY_PREFIX = 'tkey_';
const API_KEY_BYTES = 32;
const API_KEY_BODY_LEN = 43;
const API_KEY_RX = new RegExp(`^${API_KEY_PREFIX}[A-Za-z0-9_-]{${API_KEY_BODY_LEN}}$`);

function generateApiKey() {
  return `${API_KEY_PREFIX}${randomBytes(API_KEY_BYTES).toString('base64url')}`;
}

function hashApiKey(key) {
  return createHash('sha256').update(key).digest('hex');
}

function displayApiKey(key) {
  const body = key.slice(API_KEY_PREFIX.length);
  return `${API_KEY_PREFIX}${body.slice(0, 4)}...${body.slice(-4)}`;
}

function isApiKeySyntacticallyValid(key) {
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
    time_revoked = null
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
  }

  static fromRawKey(user_id, name, rawKey) {
    return new ApiKey({
      user_id: user_id,
      name: name,
      key_hash: hashApiKey(rawKey),
      key_display: displayApiKey(rawKey)
    });
  }

  isRevoked() {
    return this.time_revoked !== null;
  }

  revoke(time = new Date()) {
    this.time_revoked = time;
    return this;
  }

  /* key_hash is a credential-equivalent secret and is never returned to a client. */
  toJSON() {
    const { key_hash, ...rest } = this;
    return rest;
  }
}
