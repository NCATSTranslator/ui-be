'use strict';

import { logger } from '../lib/logger.mjs';
import { Session } from '../models/Session.mjs';
import { User } from '../models/User.mjs';
import * as sso from '../lib/SocialSignOn.mjs';

export {
  AuthService,

  SESSION_NO_TOKEN,
  SESSION_INVALID_TOKEN,
  SESSION_TOKEN_NOT_FOUND,
  SESSION_NO_USER,
  SESSION_SESSION_EXPIRED,
  SESSION_TOKEN_EXPIRED,
  SESSION_VALID,
  SESSION_INVALID_USER,
  SESSION_FORCE_KILLED,

  LOGIN_NO_TOKEN,
  LOGIN_INVALID_TOKEN,
  LOGIN_TOKEN_NOT_FOUND,
  LOGIN_WRONG_TOKEN_TYPE,
  LOGIN_FORCE_KILLED,
  LOGIN_BAD_INTERNAL_DATA,
  LOGIN_TTL_EXCEEDED,
  LOGIN_STATE_VALID
};

const SESSION_NO_TOKEN = 0;
const SESSION_INVALID_TOKEN = 1;
const SESSION_TOKEN_NOT_FOUND = 2;
const SESSION_NO_USER = 3;
const SESSION_FORCE_KILLED = 4;
const SESSION_INVALID_USER = 5;
const SESSION_SESSION_EXPIRED = 6;
const SESSION_TOKEN_EXPIRED = 7;
const SESSION_VALID = 8;

const LOGIN_NO_TOKEN = 0;
const LOGIN_INVALID_TOKEN = 1;
const LOGIN_TOKEN_NOT_FOUND = 2;
const LOGIN_WRONG_TOKEN_TYPE = 3;
const LOGIN_FORCE_KILLED = 4;
const LOGIN_BAD_INTERNAL_DATA = 5;
const LOGIN_TTL_EXCEEDED = 6;
const LOGIN_STATE_VALID = 7;


class AuthService {
  constructor(sessionParams, sessionStore, userStore) {
    this.tokenTTLSec = sessionParams.tokenTTLSec;
    this.sessionAbsoluteTTLSec = sessionParams.sessionAbsoluteTTLSec;
    this.sessionMaxIdleTimeSec = sessionParams.sessionMaxIdleTimeSec;
    this.loginRequestTTLSec = sessionParams.loginRequestTTLSec;

    this.sessionStore = sessionStore;
    this.userStore = userStore;
  }

  async getUserById(id) {
    return this.userStore.retrieveUserById(id);
  }

  async getSessionData(token) {
    let retval = {
      status: null,
      user: null,
      session: null
    };

    if (!token) {
      retval.status = SESSION_NO_TOKEN;
      return retval;
    }

    if (!this.isTokenSyntacticallyValid(token)) {
      retval.status = SESSION_INVALID_TOKEN;
      return retval;
    }

    retval.session = await this.retrieveSessionByToken(token);
    if (!retval.session) {
      retval.status = SESSION_TOKEN_NOT_FOUND;
      return retval;
    }

    if (!retval.session.user_id) {
      retval.status = SESSION_NO_USER;
      return retval;
    }

    if (retval.session.force_kill) {
      retval.status = SESSION_FORCE_KILLED;
      return retval;
    }

    // Note: reuse of SESSION_NO_USER is intentional
    retval.user =  await this.getUserById(retval.session.user_id);
    if (!retval.user) {
      retval.status = SESSION_NO_USER;
      return retval;
    }

    if (retval.user.deleted) {
      retval.status = SESSION_INVALID_USER;
      return retval;
    }

    /* Note: it's critical to check session expiry before token expiry, as
     * an expired session requires a re-login whereas a live session with
     * an expired token is a valid session that only requires a token refresh. */
    if (this.isSessionExpired(retval.session)) {
      retval.status = SESSION_SESSION_EXPIRED;
      return retval;
    }

    if (this.isTokenExpired(retval.session)) {
      retval.status = SESSION_TOKEN_EXPIRED;
      return retval;
    }

    retval.status = SESSION_VALID;
    return retval;
  }

  isSessionStatusValid(status) {
    return (status === SESSION_TOKEN_EXPIRED || status === SESSION_VALID);
  }

  async getLoginRequestData(token) {
    let retval = {
      status: null,
      loginRequestSession: null
    };

    if (!token) {
      retval.status = LOGIN_NO_TOKEN;
      return retval;
    }

    if (!this.isTokenSyntacticallyValid(token)) {
      retval.status = LOGIN_INVALID_TOKEN;
      return retval;
    }

    let res = await this.retrieveSessionByToken(token);
    if (!res) {
      retval.status = LOGIN_TOKEN_NOT_FOUND;
      return retval;
    }

    if (res.data.type !== 'login') {
      retval.status = LOGIN_WRONG_TOKEN_TYPE;
      return retval;
    }

    if (res.force_kill) {
      retval.status = LOGIN_FORCE_KILLED;
      return retval;
    }

    if (!(res.data.hasOwnProperty('codeVerifier') && res.data.hasOwnProperty('urlPath'))) {
      retval.status = LOGIN_BAD_INTERNAL_DATA;
      return retval;
    }

    if (this.isLoginRequestTTLExceeded(res.time_session_created)) {
      retval.status = LOGIN_TTL_EXCEEDED;
      return retval;
    }

    retval.loginRequestSession = res;
    retval.status = LOGIN_STATE_VALID;
    return retval;
  }

  async createLoginStateSession(data) {
    let res = null;
    try {
      res = await this.sessionStore.createNewSession(new Session({
        auth_provider: 'una',
        data: data
      }));
      return res;
    } catch (err) {
      logger.error(err);
      return null;
    }
  }

  async createNewUnauthSession(SSOData) {
    let res = null;
    try {
       res = this.sessionStore.createNewSession(new Session());
       return res;
    } catch (err) {
      logger.error(err);
      return res;
    }
  }

  async createNewAuthSession(user, SSOData) {
    let res = null;
    try {
      res = this.sessionStore.createNewSession(new Session({
        user_id: user.id,
        auth_provider: SSOData.provider,
        data: SSOData.raw_data
      }));
      return res;
   } catch (err) {
     logger.error(err);
     return res;
   }

  }

  async retrieveSessionByToken(token) {
    let res = null;
    try {
      res = this.sessionStore.retrieveSessionByToken(token);
      return res;
    } catch (err) {
      logger.error(err);
      return res;
    }
  }

  async updateSessionTime(sessionData) {
    try {
      sessionData.updateSessionTime();
      return this.sessionStore.updateSession(sessionData);
    } catch (err) {
      logger.error(err);
      return false;
    }
  }

  async refreshSessionToken(sessionData) {
    try {
      sessionData.refreshSessionToken();
      sessionData.updateSessionTime()
      return this.sessionStore.updateSession(sessionData);
    } catch (err) {
      logger.error(err);
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

  isLoginRequestTTLExceeded(timeSessionCreated) {
    const now = new Date();
    return (now - timeSessionCreated) > (this.loginRequestTTLSec * 1000);
  }

  isTokenSyntacticallyValid(token) {
    return token && Session.isTokenSyntacticallyValid(token);
  }

  async handleSSORedirect(provider, authcode, config, loginState=null) {
    const SSOData = await sso.handleSSORedirect(provider, authcode, config, loginState.data.codeVerifier);
    if (!SSOData) {
      return null;
    }
    let email = SSOData.email;
    let user = await this.userStore.retrieveUserByEmail(email);
    if (user) {
      if (user.deleted) {
        return null;
      } else {
        return this.createNewAuthSession(user, SSOData);
      }
    } else {
      user = await this.userStore.createNewUser(new User({
        name: SSOData.name,
        email: SSOData.email,
        profile_pic_url: SSOData.profile_pic_url,
      }));
      return this.createNewAuthSession(user, SSOData);
    }
  }

  async expireSessionByToken(token) {
    return this.sessionStore.expireSessionByToken(token);
  }

  async validateAuthSessionToken(token) {
    let tokenRefreshed = false;
    if (!token || !this.isTokenSyntacticallyValid(token)) {
      logger.error(`%% %% %% no cookie found`);
      throw new CookieNotFoundError();
    }
    logger.error(`%% %% %% we get cookie: ${token}`);

    let session = await this.retrieveSessionByToken(token);

    if (!session) {
      logger.error(`%% %% %% no session found for ${token}`);
      throw new SessionNotFoundError(cookie);
    }
    logger.error(`%% %% %% we get session: ${JSON.stringify(session)}`);

    if (!session.user_id || session.force_kill) {
      logger.error(`%% %% %% no user found for ${JSON.stringify(session)} or else force killed`);
      throw new SessionNotUsableError(token, session.id);
    }
    const user = await this.getUserById(session.user_id);
    if (!user) {
      logger.error(`%% %% %% no user found`);
      throw new NoUserForSessionError(token, session.id);
    } else if (user.deleted) {
      logger.error(`%% %% %% User deleted`);
      throw new UserDeletedError(user.id);
    } else if (this.isSessionExpired(session)) {
      logger.error(`%% %% %% Session expired: ${JSON.stringify(session)}`);
      throw new SessionExpiredError(token, session.id);
    } else if (this.isTokenExpired(session)) {
      logger.error(`%% %% %% Token expired, refreshing: ${JSON.stringify(session)}`);
      session = await this.refreshSessionToken(session);
      tokenRefreshed = true;
    } else {
      // Valid session - update time
      logger.error(`%% %% %% session good, udpating time: ${JSON.stringify(session)}`);
      session = await this.updateSessionTime(session);
    }

    return {
      user: user,
      session: session,
      tokenRefreshed: tokenRefreshed
    };
  }


}

/*
import { pg } from '../lib/postgres_preamble.mjs';
import { SessionStorePostgres } from '../stores/SessionStorePostgres.mjs';
import { UserStorePostgres } from '../stores/UserStorePostgres.mjs';

export const aa = (function (config) {
  const dbPool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return new AuthService({
    tokenTTLSec: config.sessions.token_ttl_sec,
    sessionAbsoluteTTLSec: config.sessions.session_absolute_ttl_sec,
    sessionMaxIdleTimeSec: config.sessions.session_max_idle_time_sec
  },
  new SessionStorePostgres(dbPool),
  new UserStorePostgres(dbPool));
})({
  storage: {
    "pg": {
      "host": "pgdb",
      "user": "app_data",
      "port": "5432",
      "database": "app_data"
    }
  },
  "db_conn": {
    "ssl": false
  },
  secrets: { pg: {password: "yourpassword"}},
  sessions: {
    "token_ttl_sec": 1800,
    "session_max_idle_time_sec": 15780000,
    "session_absolute_ttl_sec": 15780000
  }
})
*/
