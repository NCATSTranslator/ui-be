'use strict';

import * as AuthService from '../services/AuthService.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as cmn from '../lib/common.mjs';
import { API_KEY_PREFIX } from '../models/ApiKey.mjs';

export { SessionController };

class SessionController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  // All subsequent Session Controller middleware functions assume that this has been done
  async attachSessionData(req, res, next) {
    /* An API key is an explicit, deliberately-presented credential, so it decides the outcome
     * of the request: if one is presented we do not fall back to the cookie, and a bad key
     * fails rather than silently downgrading to whatever session the browser happens to hold.
     * Key-authenticated requests get a sessionData shaped like a session (so every downstream
     * handler keeps reading req.sessionData.user) but with no session row behind it.
     * req.apiKeyData marks them, so the authenticate* middleware below can check the key
     * instead of running the session-refresh machinery. */
    const apiKeyData = await this._fetchApiKeyStatus(req);
    if (apiKeyData) {
      req.apiKeyData = apiKeyData;
      if (this.authService.isApiKeyStatusValid(apiKeyData.status)) {
        req.sessionData = {
          status: AuthService.SESSION_VALID,
          user: apiKeyData.user,
          session: null
        };
        await this.authService.touchApiKey(apiKeyData.apiKey);
      } else {
        req.sessionData = {
          status: AuthService.SESSION_INVALID_TOKEN,
          user: null,
          session: null
        };
      }
      return next();
    }

    let sessionData = await this._fetchStatus(req);
    if (!sessionData) {
      return res.status(500).send(`Server error retrieving session status.`);
    }
    req.sessionData = sessionData;
    next();
  }

  async _fetchStatus(req) {
    let token = req.cookies[this.config.session_cookie.name];
    let retval = await this.authService.getSessionData(token);
    return retval;
  }

  async _fetchApiKeyStatus(req) {
    const rawKey = this._extractApiKey(req);
    if (!rawKey) {
      return null;
    }
    return this.authService.getApiKeyData(rawKey);
  }

  /* Only a credential carrying our key prefix counts as an API key attempt. Anything else in
   * the Authorization header (a proxy's Basic credentials, say) is ignored so that it cannot
   * knock a normal cookie-authenticated browser request off the session path. */
  _extractApiKey(req) {
    const authorization = req.get('authorization');
    if (authorization) {
      const match = authorization.match(/^Bearer\s+(\S+)$/i);
      if (match && match[1].startsWith(API_KEY_PREFIX)) {
        return match[1];
      }
    }
    const header = req.get('x-api-key');
    if (header && header.startsWith(API_KEY_PREFIX)) {
      return header;
    }
    return null;
  }

  async getStatus(req, res, next) {
    if (!req.sessionData) {
      return res.status(500).send(`Server error retrieving session status`);
    }
    // Delete raw session data before returning to FE
    return res.status(200).json(this._sanitizeSessionData(req.sessionData));
  }

  /*
   * The difference between authenticatePrivilegedRequest vs ...UnprivilegedRequest:
   * The former will return an auth error if the existing session is invalid.
   * The latter will do nothing unless there an existing and valid session.
   *
   * Both will return an error if the attempt to refresh a valid session fails
   * outright, A session that goes invalid mid-refresh is an auth error for the
   * former, but the latter carries on with the refreshed session data so
   * that page routes still serve the app shell and let the FE handle being logged out.
   */
  async authenticatePrivilegedRequest(req, res, next) {
    let oldSession = req.sessionData;
    if (!oldSession) {
      return res.status(500).send('Server error retrieving session status');
    }

    if (req.apiKeyData) {
      if (!this.authService.isApiKeyStatusValid(req.apiKeyData.status)) {
        return res.status(401).send('Invalid API key. Cannot service request.');
      }
      return next();
    }

    if (!this.authService.isSessionStatusValid(oldSession.status)) {
      return res.status(401).send('Invalid session status. Cannot service request.');
    }

    let [success, errstr, errcode] = await this._refreshSession(req, res, oldSession);
    if (!success) {
      return res.status(errcode).send(errstr);
    }
    next();
  }

  async authenticateUnprivilegedRequest(req, res, next) {
    if (req.apiKeyData) {
      return next();
    }
    let oldSession = req.sessionData;
    if (oldSession && this.authService.isSessionStatusValid(oldSession.status)) {
      let [success, errstr, errcode] = await this._refreshSession(req, res, oldSession);
      if (!success && errcode !== 401) {
        return res.status(errcode).send(errstr);
      }
    }
    next();
  }


  /* Gate for routes that must be driven by a human who is actually logged in. API key
   * management is the case that matters: a leaked key must not be able to mint further keys
   * or revoke the ones the owner is using. Must run after authenticatePrivilegedRequest. */
  requireSessionAuth(req, res, next) {
    if (req.apiKeyData) {
      return res.status(403).send('API keys cannot be used for this request. Log in to continue.');
    }
    next();
  }

  /* This function smells awful: it side-effects req, the DB, and cookies.
   * The possible saving grace is that this exact sequence is needed in two cases
   * and at least this centralizes it. */
  async _refreshSession(req, res, sessionData) {
    let presentedToken = sessionData.session ? sessionData.session.token : null;
    let newSession = await this._refreshSessionInDB(sessionData);
    if (!newSession) {
      return [false, 'Server error refreshing session', 500];
    }
    newSession = await this.authService.getSessionData(newSession.token);
    if (!newSession.session) {
      return [false, 'Server error fetching refreshed session', 500];
    }
    req.sessionData = newSession;
    if (!this.authService.isSessionStatusValid(newSession.status)) {
      return [false, 'Invalid session status. Cannot service request.', 401];
    }

    if (newSession.session.token !== presentedToken) {
      let cookiePath = '/'; // TODO get from config
      /* This age should more correctly be maxagesec - <time already elapsed since start of session>,
       * but it doesn't really matter as we always check the session length in the BE. */
      let cookieMaxAgeSec = this.authService.sessionAbsoluteTTLSec;
      wutil.set_session_cookie(res, this.config.session_cookie, newSession.session.token,
        cookiePath, cookieMaxAgeSec);
    }
    return [true, '', 200];
  }

  async _refreshSessionInDB(existingSession) {
    if (!existingSession) {
      return false;
    }
    if (!this.authService.isSessionStatusValid(existingSession.status)) {
      return false;
    }
    let newSession;
    switch (existingSession.status) {
      case AuthService.SESSION_TOKEN_EXPIRED:
        newSession = await this.authService.refreshSessionToken(existingSession.session);
        if (!newSession) {
          return false;
        }
        break;
      case AuthService.SESSION_VALID:
        newSession = await this.authService.updateSessionTime(existingSession.session);
        if (!newSession) {
          return false;
        }
        break;
      default:
        throw new Error('Unexpected case encountered when refreshing status');
        break;
    }
    return newSession;
  }



  _sanitizeSessionData(sessionData) {
    let retval = {...sessionData};
    if (retval && retval.session && retval.session.data) {
      delete retval.session.data;
    }
    return retval;
  }

  async updateStatus(req, res, next) {
    let curSession = req.sessionData;
    /* Key-authenticated requests carry no session row to refresh or expire. */
    if (req.apiKeyData) {
      return res.status(403).send('API keys cannot be used for this request. Log in to continue.');
    }
    if (!this.authService.isSessionStatusValid(curSession.status)) {
      return res.status(401).send('Invalid session status. Cannot service request');
    }
    let [valid, str] = this._validateStatusUpdatePayload(req.body);

    if (!valid) {
      return res.status(400).send(str);
    }

    // now do the actual stuff
    let action = Object.keys(req.body)[0];
    let newSession = null;
    let cookiePath = '/';
    let cookieMaxAgeSec = this.authService.sessionAbsoluteTTLSec;
    let presentedToken = curSession.session.token;
    switch (action) {
      case 'update':
        if (curSession.status === AuthService.SESSION_TOKEN_EXPIRED) {
          newSession = await this.authService.refreshSessionToken(curSession.session);
        } else if (curSession.status === AuthService.SESSION_VALID) {
          newSession = await this.authService.updateSessionTime(curSession.session);
        }
        break;
      case 'expire':
        newSession = await this.authService.expireSessionByToken(curSession.session.token);
        break;
    }
    if (!newSession) {
      return res.status(500).send('Server error while updating status');
    }
    let updatedSessionData = await this.authService.getSessionData(newSession.token)
    if (!updatedSessionData.session) {
      return res.status(500).send('Server error while retrieving updated session');
    }
    if (action === 'update' && updatedSessionData.session.token !== presentedToken) {
      wutil.set_session_cookie(res, this.config.session_cookie, updatedSessionData.session.token,
        cookiePath, cookieMaxAgeSec);
    }
    return res.status(200).json(this._sanitizeSessionData(updatedSessionData));
  }

  _validateStatusUpdatePayload(body) {
    if (!body) {
      return [false, 'No payload found']; // res.status(400).send('No payload found');
    } else if (!cmn.is_object(body)) {
      return [false, 'Payload is not an object']; // res.status(400).send('Payload is not an object');
    }
    let keys = Object.keys(body);
    if (keys.length !== 1) {
      return [false, 'Invalid number of fields in payload (expected exactly 1)']; // res.status(400).send('Invalid number of fields in payload (expected exactly 1)');
    } else if (!['update', 'expire'].includes(keys[0])) {
      return [false, 'Unsupported action requested']; // res.status(400).send('Unsupported action requested');
    } else if (body[keys[0]] !== true) {
      return [false, 'Unsupported value for requested action']; // res.status(400).send('Unsupported value for requested action');
    }
    return [true, ''];
  }

}
