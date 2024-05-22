'use strict';

import * as wutil from '../lib/webutils.mjs';

export { LoginController };

class LoginController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  async login(req, res, next) {
    const provider = req.params.provider;
    const authcode = req.query.code;
    let newSession = await this.authService.handleSSORedirect(provider, authcode, this.config);
    if (!newSession) {
      return res.status(403).send("There was an error with your login. Please try again with a different account or contact the UI team");
    } else {
      //let cookiePath = this.config.mainsite_path;
      let cookiePath = this.config.session_cookie.path; // *=*
      let cookieMaxAge = this.authService.sessionAbsoluteTTLSec;
      wutil.setSessionCookie(res, this.config.session_cookie, newSession.token, cookiePath, cookieMaxAge);
      // %%SV2: update this to be first '/', then the path in the state param once ready
      return res.redirect(302, '/');  // *=*
    }
  }

  async logout(req, res, next) {
    //let cookiePath = this.config.mainsite_path;
    let cookiePath = this.config.session_cookie.path; // *=*
    let cookieToken = req.cookies[this.config.session_cookie.name];

    // first, expire the cookie
    wutil.setSessionCookie(res, this.config.session_cookie, '', cookiePath, 0);
    // Second, kill the session internally
    let session = await this.authService.retrieveSessionByToken(cookieToken);
    if (!session) {
      console.error(`%% %% %% no session found for ${cookieToken} when logging out`);
    }
    session = await this.authService.expireSessionByToken(cookieToken);
    if (!session) {
      console.error(`%% %% %% error expiring session for ${cookieToken} when logging out`);
    }
    console.log(`Logout successful, redirecting to /logout.html`);
    return res.redirect(302, `/logout.html`);
  }
}
