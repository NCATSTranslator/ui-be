'use strict';

import { v4 as uuidv4, validate as uuidValidate, version as uuidVersion } from 'uuid';

export { Session };

class Session {
  constructor({
    id = null,
    token = uuidv4(),
    time_token_created = new Date(),
    time_session_created = new Date(),
    time_session_updated = new Date(),
    linked_from = null,
    force_kill = false,
    user_id = null,
    data = null,
    auth_provider = null
  } = {}) {
    this.id = id;
    this.token = token;
    this.time_token_created = time_token_created;
    this.time_session_created = time_session_created;
    this.time_session_updated = time_session_updated;
    this.linked_from = linked_from;
    this.force_kill = force_kill;
    this.user_id = user_id;
    this.data = data;
    this.auth_provider = auth_provider;
  }

  updateSessionTime(time = new Date()) {
    this.time_session_updated = time;
    return this;
  }

  refreshSessionToken(token = uuidv4()) {
    this.token = token;
    this.time_token_created = new Date();
    return this;
  }

  static isTokenSyntacticallyValid(token) {
    return uuidValidate(token) && uuidVersion(token) === 4;
  }

  updateSession(updatedFields = {}) {
    for (let key in updatedFields) {
      if (this.hasOwnProperty(key)) {
        this[key] = updatedFields[key];
      }
    }
    return this;
  }
}
