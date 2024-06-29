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
    console.log(retval);
    return retval;
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
        newSession = await this.authService.refreshSessionToken(existingSession.status);
        if (!newSession) {
          return false;
        }
        break;
      case AuthService.SESSION_VALID:
        newSession = await this.authService.updateSessionTime(existingSession.status);
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

  async getStatus(req, res, next) {
    if (!req.sessionData) {
      return res.status(500).send(`Server error retrieving session status`);
    }
    // Delete raw session data before returning to FE
    return res.status(200).json(this._sanitizeSessionData(req.sessionData));
  }

  async refreshSession(req, res, next) {
    if (!req.sessionData) {
      return res.status(500).send('Could not find current attached session status');
    }

    if (!this.authService.isSessionStatusValid(req.sessionData.status)) {
      return res.status(401).send('Invalid session status. Cannot service request.');
    }

    let newSession = this._refreshSessionInDB(req.sessionData);
    if (!newSession) {
      res.status(500).send('Error refreshing status');
    }
    newSession = this._fetchStatus(req); // Expensive but feels safer
    if (!newSession) {
      res.status(500).send('Error refreshing status upon refetch');
    }
    // If the original status was 'token expired', it's time to set a new cookie
    if (req.sessionData.status === AuthService.SESSION_TOKEN_EXPIRED) {
      let cookiePath = '/'; // TODO get from config
      /* This age should more correctly be maxagesec - <time already elapsed since start of session>,
       * but it doesn't really matter as we always check the session length in the BE. */
      let cookieMaxAgeSec = this.authService.sessionAbsoluteTTLSec;
      wutil.setSessionCookie(res, this.config.session_cookie, newSession.session.token,
        cookiePath, cookieMaxAgeSec);
    }

    req.sessionData = newSession;
    next();
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
      return res(401).send('Invalid session status. Cannot service request');
    }
    let [valid, str] = this._validateStatusUpdatePayload(req.body);

    if (!valid) {
      return res.status(400).send(str);
    }

    // now do the actual stuff
    let action = Object.keys(req.body)[0];
    let newSession = null;
    switch (action) {
      case 'update':
        if (curSession.status === AuthService.SESSION_TOKEN_EXPIRED) {
          newSession = this.authService.refreshSessionToken(curSession.session);
          wutil.setSessionCookie(res, this.config.session_cookie, newSession.token,
            cookiePath, cookieMaxAgeSec);
        } else if (curSession.status === AuthService.SESSION_VALID) {
          newSession = this.authService.updateSessionTime(curSession.session);
        }
        break;
      case 'expire':
        newSession = this.authService.expireSessionByToken(curSession.session.token);
        break;
    }
    if (!newSession) {
      return res.status(500).send('Server error while updating status');
    }
    // now actualy FETCH the new status, vs just updating the req object?

    return res.status(200).json(this._sanitizeSessionData(newSession));
  }

  _validateStatusUpdatePayload(body) {
    if (!body) {
      return [false, 'No payload found']; // res.status(400).send('No payload found');
    } else if (!cmn.isObj(body)) {
      return [false, 'Payload is not an object']; // res.status(400).send('Payload is not an object');
    }
    let keys = Object.keys(body);
    if (keys.length !== 1) {
      return [false, 'Invalid number of fields in payload (expected exactly 1)']; // res.status(400).send('Invalid number of fields in payload (expected exactly 1)');
    } else if (!['update', 'expire'].includes(keys[0])) {
      return [false, 'Unsupported action requested']; // res.status(400).send('Unsupported action requested');
    } else if (req.body[keys[0]] !== true) {
      return [false, 'Unsupported value for requested action']; // res.status(400).send('Unsupported value for requested action');
    }
    return [true, ''];
  }

}
