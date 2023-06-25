'use strict';

import { SessionStorePostgres } from './SessionStorePostgres.mjs';
import { Session } from '../models/Session.mjs';

export { AuthService };

class AuthService {
  constructor(token_ttl_sec, session_max_idle_time_sec, session_absolute_ttl_sec, config=null) {
    this.token_ttl_sec = token_ttl_sec;
    this.session_absolute_ttl_sec = session_absolute_ttl_sec;
    this.session_max_idle_time_sec = session_max_idle_time_sec;

    // TODO: don't instantiate here: pass in a working SessionStore.
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
    try {
      session_data.updateSessionTime();
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

  isTokenExpired(session_data) {
    return (new Date() - session_data.time_token_created) > (this.token_ttl_sec * 1000);
  }

  isSessionExpired(session_data) {
    const now = new Date();
    return (now - session_data.time_session_updated) > (this.session_max_idle_time_sec * 1000)
      || (now - session_data.time_session_created) > (this.session_absolute_ttl_sec * 1000);
  }

  isTokenSyntacticallyValid(token) {
    return token && Session.isTokenSyntacticallyValid(token);
  }

  async validateUnauthSession(token) {
    try {
      if (!token || !Session.isTokenSyntacticallyValid(token)) {
        console.log("we didn't get a session");
        return this.createNewUnauthSession();
      }
      let session_data = await this.retrieveSessionByToken(token);
      if (!session_data || this.isSessionExpired(session_data)) {
        return this.createNewUnauthSession();
      } else if (this.isTokenExpired(session_data)) {
        return this.refreshSessionToken(session_data);
      } else {
        return this.updateSessionTime(session_data);
      }
    } catch (err) {
      console.log(err);
      return null;
    }
  }

}
