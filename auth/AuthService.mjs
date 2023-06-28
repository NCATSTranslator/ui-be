'use strict';

import { Session } from '../models/Session.mjs';
import { User } from '../models/User.mjs';
import * as sso from '../SocialSignOn.mjs';

export { AuthService };

class AuthService {
  constructor(sessionParams, sessionStore, userStore) {
    this.tokenTTLSec = sessionParams.tokenTTLSec;
    this.sessionAbsoluteTTLSec = sessionParams.sessionAbsoluteTTLSec;
    this.sessionMaxIdleTimeSec = sessionParams.sessionMaxIdleTimeSec;

    this.sessionStore = sessionStore;
    this.userStore = userStore;
  }

  async getUserById(id) {
    return this.userStore.retrieveUserById(id);
  }

  async createNewUnauthSession() {
    let res = null;
    try {
       res = this.sessionStore.createNewSession(new Session());
       console.log(`authservice: new unauth session: ${JSON.stringify(res)}`);
       return res;
    } catch (err) {
      console.log(err);
      return res;
    }
  }

  async createNewAuthSession(user) {
    let res = null;
    try {
      res = this.sessionStore.createNewSession(new Session({user_id: user.id}));
      console.log(`authservice: new auth session: ${JSON.stringify(res)}`);
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

  async handleSSORedirect(provider, authcode, config) {
    const SSOData = await sso.handleSSORedirect(provider, authcode, config);
    let email = SSOData.email;
    let user = await this.userStore.retrieveUserByEmail(email);
    if (user) {
      if (user.deleted) {
        return null;
      } else {
        return this.createNewAuthSession(user);
      }
    } else {
      user = await this.userStore.createNewUser(new User({
        name: SSOData.name,
        email: SSOData.email,
        profile_pic_url: SSOData.profile_pic_url,
        data: SSOData.raw_data,
        deleted: false
      }));
      return this.createNewAuthSession(user);
    }
  }
}
