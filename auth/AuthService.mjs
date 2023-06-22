'use strict';

import { SessionStorePostgres } from './SessionStorePostgres.mjs';
import { Session } from '../models/Session.mjs';

export { AuthService };

class AuthService {
  constructor(token_ttl_sec, session_absolute_ttl_sec, session_max_idle_time_sec, config=null) {
    // TODO: don't instantiate here: pass in a working SessionStore.
    this.token_ttl_sec = token_ttl_sec;
    this.session_absolute_ttl_sec = session_absolute_ttl_sec;
    this.session_max_idle_time_sec = session_max_idle_time_sec;

    this.SessionStore = new SessionStorePostgres({
      host: 'localhost',
      user: 'postgres',
      password: 'yourpassword',
      port: '5432',
      database: 'app_data'
    });
  }

  async createNewUnauthSession() {
    let res = null;
    try {
       res = this.SessionStore.createNewSession(new Session());
       console.log(`wow that worked! ${res}`);
       return res;
    } catch (err) {
      console.log(err);
      return res;
    }
  }

  async retrieveSessionByToken(token) {
    let res = null;
    try {
      res = this.SessionStore.retrieveSessionByToken(token);
      return res;
    } catch (err) {
      console.log(err);
      return res;
    }
  }

  async updateSessionTime(session_data) {
    session_data.updateSessionTime();
    try {
      return this.SessionStore.updateSession(session_data);
    } catch (err) {
      console.log(err);
      return false;
    }
  }

  async refreshSessionToken(session_data) {
    try {
      session_data.refreshSessionToken();
      session_data.updateSessionTime()
      return this.SessionStore.updateSession(session_data);
    } catch (err) {
      console.log(err);
      return false;
    }
  }

}
