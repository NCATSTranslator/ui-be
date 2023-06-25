'use strict';

import { Session } from '../models/Session.mjs';

export { AuthService };

class AuthService {
  constructor(sessionParams, sessionStore) {
    this.tokenTTLSec = sessionParams.tokenTTLSec;
    this.sessionAbsoluteTTLSec = sessionParams.sessionAbsoluteTTLSec;
    this.sessionMaxIdleTimeSec = sessionParams.sessionMaxIdleTimeSec;

    this.sessionStore = sessionStore;
  }

  async createNewUnauthSession() {
    let res = null;
    try {
       res = this.sessionStore.createNewSession(new Session());
       console.log(`authservice: new session: ${JSON.stringify(res)}`);
       return res;
    } catch (err) {
      console.log(err);
      return res;
    }
  }

  async retrieveSessionByToken(token) {
    let res = null;
    try {
      res = this.sessionStore.retrieveSessionByToken(token);
      return res;
    } catch (err) {
      console.log(err);
      return res;
    }
  }

  async updateSessionTime(sessionData) {
    try {
      sessionData.updateSessionTime();
      return this.sessionStore.updateSession(sessionData);
    } catch (err) {
      console.log(err);
      return false;
    }
  }

  async refreshSessionToken(sessionData) {
    try {
      sessionData.refreshSessionToken();
      sessionData.updateSessionTime()
      return this.sessionStore.updateSession(sessionData);
    } catch (err) {
      console.log(err);
      return false;
    }
  }

  isTokenExpired(sessionData) {
    return (new Date() - sessionData.time_token_created) > (this.tokenTTLSec * 1000);
  }

  isSessionExpired(sessionData) {
    const now = new Date();
    return (now - sessionData.time_session_updated) > (this.sessionMaxIdleTimeSec * 1000)
      || (now - sessionData.time_session_created) > (this.sessionAbsoluteTTLSec * 1000);
  }

  isTokenSyntacticallyValid(token) {
    return token && Session.isTokenSyntacticallyValid(token);
  }

}
