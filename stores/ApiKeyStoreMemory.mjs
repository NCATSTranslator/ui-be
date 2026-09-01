'use strict';

import { ApiKey } from '../models/ApiKey.mjs';

export { ApiKeyStoreMemory };

/* API keys are a demo feature, so they are held in a Map rather than a table and need no
 * migration. Consequences of that trade:
 *   - every key is lost when the process restarts,
 *   - nothing is shared between server processes,
 *   - a single instance must be shared by every service that touches keys, since each
 *     instance is its own separate "table".
 * Making keys durable means a real store against the api_keys table, plus its migration. */
class ApiKeyStoreMemory {
  constructor() {
    this._keys = new Map();
  }

  /* Callers get their own copy, so mutating a returned key cannot reach back into the store
   * the way it cannot reach back into a database. */
  _clone(apiKey) {
    return new ApiKey({ ...apiKey });
  }

  async createApiKey(apiKey) {
    for (const stored of this._keys.values()) {
      if (stored.key_hash === apiKey.key_hash) {
        throw new Error('An API key with that hash already exists');
      }
    }
    this._keys.set(apiKey.id, this._clone(apiKey));
    return this._clone(apiKey);
  }

  async retrieveApiKeyByHash(keyHash) {
    for (const stored of this._keys.values()) {
      if (stored.key_hash === keyHash) {
        return this._clone(stored);
      }
    }
    return null;
  }

  async retrieveApiKeysByUserId(userId, includeRevoked=false) {
    const matches = [];
    for (const stored of this._keys.values()) {
      if (stored.user_id !== userId) continue;
      if (!includeRevoked && stored.isRevoked()) continue;
      matches.push(this._clone(stored));
    }
    matches.sort((a, b) => b.time_created - a.time_created);
    return matches;
  }

  async revokeApiKeyById(id, userId, time=new Date()) {
    const stored = this._keys.get(id);
    if (!stored || stored.user_id !== userId || stored.isRevoked()) {
      return null;
    }
    stored.revoke(time);
    return this._clone(stored);
  }

  async updateLastUsedById(id, time=new Date()) {
    const stored = this._keys.get(id);
    if (!stored) {
      return null;
    }
    stored.time_last_used = time;
    return this._clone(stored);
  }
}
