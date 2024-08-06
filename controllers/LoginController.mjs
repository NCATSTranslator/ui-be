'use strict';

import * as wutil from '../lib/webutils.mjs';
import * as AuthService from '../services/AuthService.mjs';
import { generatePKCECodeChallenge, generatePKCECodeVerifier } from '../lib/common.mjs';

export { LoginController };

class LoginController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  // Note: this is not a generic function; it is purpose-built for login with una only.
  // If support for other SSO providers is needed, different login-with-X functions will be needed.
  async loginWithUna(req, res, next) {
    const redirPath = req.query.path || '/';

    if (req.sessionData && this.authService.isSessionStatusValid(req.sessionData.status)) {
      // If the user has a valid login session, bypass the login flow
      return res.redirect(302, redirPath);
    }
    const codeVerifier = generatePKCECodeVerifier(64);
    const codeChallenge = generatePKCECodeChallenge(codeVerifier);

    let stateData = await this.authService.createLoginStateSession({
      type: 'login',
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
    console.log({here: redirUrl, state: stateData});
    return res.redirect(302, redirUrl);
  }

  async authRedir(req, res, next) {
    const provider = req.params.provider;
    const authcode = req.query.code;
    const state = req.query.state || null;

    let loginData = await this.authService.getLoginRequestData(state);
    switch (loginData.status) {
      case AuthService.LOGIN_NO_TOKEN:
      case AuthService.LOGIN_INVALID_TOKEN:
      case AuthService.LOGIN_TOKEN_NOT_FOUND:
      case AuthService.LOGIN_WRONG_TOKEN_TYPE:
      case AuthService.LOGIN_FORCE_KILLED:
      case AuthService.LOGIN_BAD_INTERNAL_DATA:
        return res.status(400).send(`Could not process login request (error code ${loginData.status})`);
        break;
      case AuthService.LOGIN_TTL_EXCEEDED:
        return res.status(403).send(`Your login request has exceeded the allowed time. Please try again.`);
        break;
      case AuthService.LOGIN_STATE_VALID:
        break; // Good: continue with the login flow.
      default:
        return wutil.sendInternalServerError(res, `Unexpected login state status: `)
    }
    console.log(`in auth redir handler`);
    console.log(loginData);

    let newSession = await this.authService.handleSSORedirect(provider, authcode, this.config, loginData.loginRequestSession);
    if (!newSession) {
      return res.status(403).send("There was an error with your login. Please try again with a different account or contact the UI team");
    } else {
      //let cookiePath = this.config.mainsite_path;
      let cookiePath = this.config.session_cookie.path; // *=*
      let cookieMaxAge = this.authService.sessionAbsoluteTTLSec;
      wutil.setSessionCookie(res, this.config.session_cookie, newSession.token, cookiePath, cookieMaxAge);
      // %%SV2: update this to be first '/', then the path in the state param once ready
      return res.redirect(302, decodeURIComponent(loginData.loginRequestSession.data.urlPath));  // *=*
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
    console.log(`Logout successful, redirecting to /`);
    return res.redirect(302, `/`);
  }
}
