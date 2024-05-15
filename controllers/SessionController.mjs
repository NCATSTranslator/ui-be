'use strict';

import * as AuthService from '../services/AuthService.mjs';
import * as wutil from '../lib/webutils.mjs';

export { SessionController };

class SessionController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  // All subsequent Session Controller middleware functions assume that this has been done
  async attachSessionStatus(req, res, next) {
    let sessionStatus = await this._fetchStatus(req);
    if (!sessionStatus) {
      return res.status(500).send(`Server error retrieving session status.`);
    }
    req.sessionStatus = sessionStatus;
    next();
  }

  async _fetchStatus(req) {
    let cookieToken = req.cookies[this.config.session_cookie.name];
    let retval = await this.authService.getSessionStatus(cookieToken);
    console.log(retval);
    return retval;
  }

  async getStatus(req, res, next) {
    if (!req.sessionStatus) {
      return res.status(500).send(`Server error retrieving session status`);
    }
    let sessionStatus = {...req.sessionStatus};
    // Delete raw session data before returning to FE
    return res.status(200).json(this._sanitizeSessionStatus(sessionStatus));
  }

  async refreshSession(req, res, next) {
    let sessionStatus = req.sessionStatus;
    let cookiePath = '/';
    let cookieMaxAgeSec = this.authService.sessionAbsoluteTTLSec;
    let success = false;
    let newSession;
    switch (sessionStatus.status) {
      case AuthService.SESSION_TOKEN_EXPIRED:
        newSession = await this.authService.refreshSessionToken(sessionStatus.session);
        if (!newSession) {
          return res.status(500).send("Server error refreshing session token");
        }
        success = true;
        wutil.setSessionCookie(res, this.config.session_cookie, newSession.token, cookiePath, cookieMaxAgeSec);
        req.sessionStatus = newSession;
        break;
      case AuthService.SESSION_VALID:
        newSession = await this.authService.updateSessionTime(sessionStatus.session);
        if (!newSession) {
          return res.status(500).send("Server error updating session time.");
        }
        req.sessionStatus = newSession;
        break;
      default:
        success = false;
        break;
    }
    next();
  }

  _sanitizeSessionStatus(sessionStatus) {
    if (sessionStatus && sessionStatus.session && sessionStatus.session.data) {
      delete sessionStatus.session.data;
    }
    return sessionStatus;
  }

  async updateStatus(req, res, next) {
    return res.status(200).json({assume: "something happened"});
  }

}
