'use strict';

import { SessionStorePostgres } from './SessionStorePostgres.mjs';
import { Session } from '../models/Session.mjs';

export { AuthService };

class AuthService {
  constructor(config) {
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
    const session_data = Session.createNewUnauthSession();
    let res = null;
    try {
       res = this.SessionStore.createNewSession(session_data);
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
