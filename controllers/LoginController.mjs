'use strict';

import * as wutil from '../lib/webutils.mjs';
import { generatePKCECodeChallenge, generatePKCECodeVerifier } from '../lib/common.mjs';

export { LoginController };

class LoginController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  async authRedir(req, res, next) {
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

  // Note: this is not a generic function; it is purpose-built for login with una only.
  // If support for other SSO providers is needed, different login-with-X functions will be needed.
  async loginWithUna(req, res, next) {
    const redirPath = req.query.path || '/';
    const codeVerifier = generatePKCECodeVerifier(64);
    const codeChallenge = generatePKCECodeChallenge(codeVerifier);

    let stateData = await this.authService.createLoginStateSession({
      urlPath: encodeURIComponent(redirPath),
      codeVerifier: codeVerifier
    });
    if (!stateData) {
      return wutil.sendInternalServerError(res, `Error creating login state session`);
    }
    const unaConfig = this.config.auth.social_providers.una;
    const redirUrl = `${unaConfig.auth_uri}?`
      + `response_type=code`
      + `&client_id=${unaConfig.client_id}`
      + `&scope=${encodeURIComponent(unaConfig.scope)}`
      + `&redirect_uri=${encodeURIComponent(unaConfig.redirect_uri)}`
      + `&state=${stateData.token}`
      + `&code_challenge=${codeChallenge}`
      + `&code_challenge_method=S256`; // case sensitive
    return res.status(200).json({here: redirUrl, state: stateData});
  //    return res.redirect(302, )
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
