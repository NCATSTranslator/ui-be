'use strict';

import * as AuthService from '../services/AuthService.mjs';
import * as wutil from '../lib/webutils.mjs';
import * as cmn from '../lib/common.mjs';

export { SessionController };

class SessionController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  // All subsequent Session Controller middleware functions assume that this has been done
  async attachSessionData(req, res, next) {
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
   * Both will return an error if there is a valid session but the attempt to
   * refresh it fails.
   */
  async authenticatePrivilegedRequest(req, res, next) {
    let oldSession = req.sessionData;
    if (!oldSession) {
      return res.status(500).send('Server error retrieving session status');
    }

    if (!this.authService.isSessionStatusValid(oldSession.status)) {
      return res.status(401).send('Invalid session status. Cannot service request.');
    }

    let [success, errstr] = await this._refreshSession(req, res, oldSession);
    if (!success) {
      return res.status(500).send(errstr);
    }
    next();
  }

  async authenticateUnprivilegedRequest(req, res, next) {
    let oldSession = req.sessionData;
    if (oldSession && this.authService.isSessionStatusValid(oldSession.status)) {
      let [success, errstr] = await this._refreshSession(req, res, oldSession);
      if (!success) {
        return res.status(500).send(errstr);
      }
    }
    next();
  }


  /* This function smells awful: it side-effects req, the DB, and cookies.
   * The possible saving grace is that this exact sequence is needed in two cases
   * and at least this centralizes it. */
  async _refreshSession(req, res, sessionData) {
    let newSession = await this._refreshSessionInDB(sessionData);
    if (!newSession) {
      return [false, 'Server error refreshing session'];
    }
    newSession = await this.authService.getSessionData(newSession.token);
    if (!newSession) {
      return [false, 'Server error fetching refreshed session'];
    }

    // If the original status was 'token expired', we need to set the new cookie
    if (sessionData.status === AuthService.SESSION_TOKEN_EXPIRED) {
      let cookiePath = '/'; // TODO get from config
      /* This age should more correctly be maxagesec - <time already elapsed since start of session>,
       * but it doesn't really matter as we always check the session length in the BE. */
      let cookieMaxAgeSec = this.authService.sessionAbsoluteTTLSec;
      wutil.setSessionCookie(res, this.config.session_cookie, newSession.session.token,
        cookiePath, cookieMaxAgeSec);
    }
    // Finally, attach the new sessionData to req
    req.sessionData = newSession;
    return [true, ''];
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
    switch (action) {
      case 'update':
        if (curSession.status === AuthService.SESSION_TOKEN_EXPIRED) {
          newSession = await this.authService.refreshSessionToken(curSession.session);
          wutil.setSessionCookie(res, this.config.session_cookie, newSession.token,
            cookiePath, cookieMaxAgeSec);
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
    newSession = await this.authService.getSessionData(newSession.token)
    if (!newSession) {
      return res.status(500).send('Server error while retrieving updated session');
    }
    return res.status(200).json(this._sanitizeSessionData(newSession));
  }

  _validateStatusUpdatePayload(body) {
    if (!body) {
      return [false, 'No payload found']; // res.status(400).send('No payload found');
    } else if (!cmn.isObject(body)) {
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
