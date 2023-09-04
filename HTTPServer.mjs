'use strict'

import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pinoHttp } from 'pino-http';
import { default as cookieParser } from 'cookie-parser';

import { createUserController } from './routers/UserAPIController.mjs';
import { createAPIRouter } from './routers/APIRouter.mjs';
import { validateDemoQueryRequest, handleDemoQueryRequest } from './DemoQueryHandler.mjs';

import * as wutil from './webutils.mjs';
import * as cmn from './common.mjs';

export function startServer(config, services) {
  console.log("Der Anfang ist das Ende und das Ende ist der Anfang");
  const translatorService = services.translatorService;
  const authService = services.authService;
  const userService = services.userService;
  const demoQueries = config.frontend.filter(e => e.allow_inbound);
  const demopath = config.demosite_path;
  const mainpath = config.mainsite_path;
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  app.use(pinoHttp());
  app.use(express.json({ limit: config.json_payload_limit }));
  app.use(cookieParser());

  app.use(express.static('./build'));
  const filters = {whitelistRx: /^ara-/}; // TODO: move to config
  config.filters = filters;

  app.get('/health', (req, res, next) => {
    res.send('OK');
  });

  app.all(['/demo', '/demo/*'], validateUnauthSession(config, authService));
  app.all(['/main', '/main/*'], validateAuthSession(config, authService));

  app.get('/main',  (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });
  app.get('/main/logout', handleLogout(config, authService));
  // logout.html is temp. to test una logout.
  app.get('/main/logout.html',  (req, res, next) => {
    res.sendFile(path.join(__root, 'build/logout.html'));
  });

  app.get('/demo/disease/:disease_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.disease_id }),
    handleDemoQueryRequest(config.demosite_path));
  app.get('/demo/gene/:gene_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.gene_id }),
    handleDemoQueryRequest(config.demosite_path));
  app.get('/demo/chemical/:chemical_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.chemical_id }),
    handleDemoQueryRequest(config.demosite_path));


  app.use('/main/api/v1/pub', createAPIRouter(config, services, false));
  app.use('/demo/api/v1/pub', createAPIRouter(config, services, true));
  app.use('/main/api/v1/pvt/users', createUserController(config, services));

  app.get('/oauth2/redir/:provider', handleLogin(config, authService));

  app.get(['/demo', '/main', '/demo/*', '/main/*', '/login'], (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });

  app.get('*', (req, res, next) => {
    res.redirect(302, '/main');
  });

  app.listen(8386);
}

function handleLogin(config, authService) {
  return async function(req, res, next) {
    const provider = req.params.provider;
    const authcode = req.query.code;
    let newSession = await authService.handleSSORedirect(provider, authcode, config);
    if (!newSession) {
      return res.status(403).send("There was an error with your login. Please try again with a different account or contact the UI team");
    } else {
      let cookiePath = config.mainsite_path;
      let cookieMaxAge = authService.sessionAbsoluteTTLSec;
      wutil.setSessionCookie(res, config.session_cookie, newSession.token, cookiePath, cookieMaxAge);
      return res.redirect(302, '/main');
    }
  }
}

function handleLogout(config, authService) {
  return async function(req, res, next) {
    let cookiePath = config.mainsite_path;
    let cookieToken = req.cookies[config.session_cookie.name];

    // first, expire the cookie
    wutil.setSessionCookie(res, config.session_cookie, '', cookiePath, 0);
    // Second, kill the session internally
    let session = await authService.retrieveSessionByToken(cookieToken);
    if (!session) {
      console.error(`%% %% %% no session found for ${cookieToken} when logging out`);
    }
    session = await authService.expireSessionByToken(cookieToken);
    if (!session) {
      console.error(`%% %% %% error expiring session for ${cookieToken} when logging out`);
    }
    console.log(`Logout successful, redirecting to /login`);
    return res.redirect(302, `/demo`);
  };
}


function validateAuthSession(config, authService) {
  return async function(req, res, next) {
    let cookiePath = config.mainsite_path;
    let cookieToken = req.cookies[config.session_cookie.name];
    let cookieMaxAge = authService.sessionAbsoluteTTLSec;

    if (!cookieToken || !authService.isTokenSyntacticallyValid(cookieToken)) {
      console.error(`%% %% %% no cookie found`);
      return res.redirect(302, `/login`);
    }
    console.error(`%% %% %% we get cookie: ${cookieToken}`);

    let session = await authService.retrieveSessionByToken(cookieToken);
    console.error(`%% %% %% we get session: ${JSON.stringify(session)}`);
    if (!session) {
      console.error(`%% %% %% no session found for ${cookieToken}`);
      return res.redirect(302, `/login`);
    }
    if (!session.user_id || session.force_kill) {
      console.error(`%% %% %% no user found for ${JSON.stringify(session)} or else force killed`);
      return res.redirect(302, `/login`);
    }
    const user = await authService.getUserById(session.user_id);
    if (!user) {
      console.error(`%% %% %% no user found`);
      return res.redirect(302, `/login`);
    } else if (user.deleted) {
      console.error(`%% %% %% User deleted`);
      return res.status(403).send('This account has been deactivated. Please re-register to use the site');
    } else if (authService.isSessionExpired(session)) {
      console.error(`%% %% %% Session expired: ${JSON.stringify(session)}`);
      return res.redirect(302, `/login`);
    } else if (authService.isTokenExpired(session)) {
      console.error(`%% %% %% Token expired, refreshing: ${JSON.stringify(session)}`);
      session = await authService.refreshSessionToken(session);
      wutil.setSessionCookie(res, config.session_cookie, session.token, cookiePath, cookieMaxAge);
    } else {
      // Valid session - update time
      console.error(`%% %% %% session good, udpating time: ${JSON.stringify(session)}`);
      session = await authService.updateSessionTime(session);
    }
    req.user = user;
    next();
  }
}

function validateUnauthSession(config, authService) {
  return async function (req, res, next) {
    let session = null;
    let cookiePath = config.demosite_path;
    let cookieToken = req.cookies[config.session_cookie.name];
    let cookieMaxAge = authService.sessionAbsoluteTTLSec;

    console.log(`-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-==- ${cookieToken}`);
    try {
      if (!authService.isTokenSyntacticallyValid(cookieToken)) {
        console.log(">>> >>> >>> did not recv a valid token; creating a new session");
        session = await authService.createNewUnauthSession();
        wutil.setSessionCookie(res, config.session_cookie, session.token, cookiePath, cookieMaxAge);
      } else {
        session = await authService.retrieveSessionByToken(cookieToken);
        if (!session || authService.isSessionExpired(session)) {
          console.log(">>> >>> >>> Sess expired or could not retrieve; creating a new session");
          session = await authService.createNewUnauthSession();
          wutil.setSessionCookie(res, config.session_cookie, session.token, cookiePath, cookieMaxAge);
        } else if (authService.isTokenExpired(session)) {
          // Order matters; check session expiry before checking token expiry
          console.log(">>> >>> >>> Token expired; creating a new TOKEN");
          session = await authService.refreshSessionToken(session);
          wutil.setSessionCookie(res, config.session_cookie, session.token, cookiePath, cookieMaxAge);
        } else {
          // we have a valid existing session
          console.log(">>> >>> >>> Session was valid; updating time");
          session = await authService.updateSessionTime(session);
        }
        console.log(`>>> >>> >>> sessionData: ${JSON.stringify(session)}`);
      }
    } catch (err) {
      wutil.logInternalServerError(`Auth validation error: ${err}`);
      return wutil.sendInternalServerError(`Auth validation error: ${err}`);
    }
    next();
  }
}
