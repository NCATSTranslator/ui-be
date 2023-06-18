import { v4 as uuidv4 } from 'uuid';

export { SessionData };

class SessionData {
  constructor({
    id = null,
    token = uuidv4(),
    time_token_created = new Date().toISOString(),
    time_session_created = new Date().toISOString(),
    time_session_updated = new Date().toISOString(),
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

  static createNewUnauthSession({
    id = null,
    token = uuidv4(),
    time_token_created = new Date().toISOString(),
    time_session_created = new Date().toISOString(),
    time_session_updated = new Date().toISOString(),
    data = null
  } = {}) {
    return new SessionData({
      id,
      token,
      time_token_created,
      time_session_created,
      time_session_updated,
      data
    });
  }

  updateSessionTime(time = new Date().toISOString()) {
    this.time_session_updated = time;
    return this;
  }

  updateSessionToken(token = uuidv4()) {
    this.token = token;
    this.time_token_created = new Date().toISOString();
    return this;
  }
  
  updateSessionData(updatedFields = {}) {
    for (let key in updatedFields) {
      if (this.hasOwnProperty(key)) {
        this[key] = updatedFields[key];
      }
    }
    return this;
  }
}
