'use strict'

import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pinoHttp } from 'pino-http';
import { default as cookieParser } from 'cookie-parser';

import { createUserController } from './routers/UserAPIController.mjs';
import { createAPIRouter } from './routers/APIRouter.mjs';
import { validateDemoQueryRequest, handleDemoQueryRequest } from './DemoQueryHandler.mjs';

// Controllers
import { ConfigAPIController } from './controllers/ConfigAPIController.mjs';
import { QueryAPIController } from './controllers/QueryAPIController.mjs';
import { LoginController } from './controllers/LoginController.mjs';
import { SessionController } from './controllers/SessionController.mjs';
import { UserAPIController } from './controllers/UserAPIController.mjs';

import * as wutil from './lib/webutils.mjs';
import { User } from './models/User.mjs';

export function startServer(config, services) {

  const filters = {whitelistRx: /^ara-/}; // TODO: move to config file
  config.filters = filters;

  const authService = services.authService;
  const userService = services.userService;
  const translatorService = services.translatorService;
  const demoQueries = config.frontend.cached_queries.filter(e => e.allow_inbound);
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  const loginController = new LoginController(config, authService);
  const queryAPIController = new QueryAPIController(config, translatorService, filters);
  const configAPIController = new ConfigAPIController(config);
  const userAPIController = new UserAPIController(config, userService, translatorService);
  const sessionController = new SessionController(config, authService);

  app.use(pinoHttp());
  app.use(express.json({ limit: config.json_payload_limit }));
  app.use(cookieParser());

  app.use(express.static('./build'));

  app.get('/health', (req, res, next) => {
    res.send('OK');
  });

  /* On all routes below, check the session status. If a valid session exists for a submitted token,
   * refresh the token if necessary, update the session time, and attach the user to req.
   * If no valid session exists (or no token submitted/invalid token, etc.), take no backend action
   * but attach a boolean to the req object indicating no valid session present.
   *
   * ** !!! Privileged routes below rely on these actions taking place prior to their handlers being called !!! ***
   */
  app.use(sessionController.attachSessionData.bind(sessionController));

  // Session status API
  app.get('/api/v1/session/status', sessionController.getStatus.bind(sessionController));
  app.post('/api/v1/session/status', sessionController.updateStatus.bind(sessionController));

  // Login/logout
  app.get('/oauth2/redir/:provider', loginController.login.bind(loginController));
  // *=* NEED TO KNOW HOW LOGOUT WORKS TODAY -- DO WE EVER SEE A BE REQ FOR LOGOUT??
  app.get('/main/logout2', loginController.logout.bind(loginController));

  /* Config. ** Idiosyncratic decision alert!! **
   * Similar to the GET session status API, we choose to not have requests to the config API update the session status.
   * The reason is that this is completely open and app-wide info, requiring no auth and not tied to a user session.
   * If the FE app makes a request to this API, it is doing so for its own purposes, that may or may not be tied
   * to explicit user activity (e.g., the FE app needs this at startup regardless of whether there is a user
   * session active). To maintain consistency with the decision that only intentional user activity should count
   * towards updating session last-touch times, we therefore exclude this API from that set. */
  app.use('/api/v1/config', configAPIController.getConfig.bind(configAPIController));

  /** All routes below this point MUST use one of authenticate[Un]PrivilegedRequest() **/

  // Query routes: unprivileged
  app.use('/api/v1/query', sessionController.authenticateUnprivilegedRequest.bind(sessionController));
  app.post('/api/v1/query', queryAPIController.submitQuery.bind(queryAPIController));
  app.get('/api/v1/query/:qid/status', queryAPIController.getQueryStatus.bind(queryAPIController));
  app.get('/api/v1/query/:qid/result', queryAPIController.getQueryResult.bind(queryAPIController));

  // User routes: privileged
  app.use('/api/v1/users', sessionController.authenticatePrivilegedRequest.bind(sessionController));
  app.get('/api/v1/users/me', userAPIController.getUser.bind(userAPIController));
  app.get('/api/v1/users/me/preferences', userAPIController.getUserPrefs.bind(userAPIController));
  app.post('/api/v1/users/me/preferences', userAPIController.updateUserPrefs.bind(userAPIController));
  app.get('/api/v1/users/me/saves', userAPIController.getUserSaves.bind(userAPIController));
  app.post('/api/v1/users/me/saves', userAPIController.updateUserSaves.bind(userAPIController));
  app.get('/api/v1/users/me/saves/:save_id', userAPIController.getUserSaveById.bind(userAPIController));
  app.put('/api/v1/users/me/saves/:save_id', userAPIController.updateUserSaveById.bind(userAPIController));
  app.delete('/api/v1/users/me/saves/:save_id', userAPIController.deleteUserSaveById.bind(userAPIController));





  // OLDER STUFF

  app.get('/main/logout.html',  (req, res, next) => {
    res.sendFile(path.join(__root, 'build/logout.html'));
  });

  app.all(['/demo', '/demo/*'], validateUnauthSession(config, authService));
  app.all(['/main', '/main/*'], validateAuthSession(config, authService));

  app.get('/main',  (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });
  ///app.get('/main/logout', handleLogout(config, authService));
  // logout.html is temp. to test una logout.


  app.get('/logout.html',  (req, res, next) => {
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

  ///app.get('/oauth2/redir/:provider', handleLogin(config, authService));

  app.get(['/demo', '/main', '/demo/*', '/main/*', '/login'], (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });

  // == -- -- NEW -- -- ==


  //app.use('/api/v1/query', queryAPIController(config, services.translatorService));
  //app.use('/api/v1/users', userRouter(config))
  app.all(['/api', '/api/*'], (req, res) => {
    return res.status(403).send('API action Forbidden');
  });

  app.get('*', (req, res, next) => {
    res.redirect(302, '/main');
  });

  app.listen(8386);
  console.log("Der Anfang ist das Ende und das Ende ist der Anfang");
}

/*
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
*/

function validateAuthSession(config, authService) {
  function handleUnauthSession(req, res) {
    let redirectPath = '/login';
    // This is a little gross because we have to know the route on the FE.
    if (req.path === '/main/results') {
      redirectPath = `/demo/results?${new URLSearchParams(req.query).toString()}`;
    }

    res.redirect(302, redirectPath);
  }

  return async function(req, res, next) {
    let cookiePath = config.mainsite_path;
    let cookieToken = req.cookies[config.session_cookie.name];
    let cookieMaxAge = authService.sessionAbsoluteTTLSec;

    if (!cookieToken || !authService.isTokenSyntacticallyValid(cookieToken)) {
      console.error(`%% %% %% no cookie found`);
      return handleUnauthSession(req, res);
    }
    console.error(`%% %% %% we get cookie: ${cookieToken}`);

    let session = await authService.retrieveSessionByToken(cookieToken);
    console.error(`%% %% %% we get session: ${JSON.stringify(session)}`);
    if (!session) {
      console.error(`%% %% %% no session found for ${cookieToken}`);
      return handleUnauthSession(req, res);
    }
    if (!session.user_id || session.force_kill) {
      console.error(`%% %% %% no user found for ${JSON.stringify(session)} or else force killed`);
      return handleUnauthSession(req, res);
    }
    const user = await authService.getUserById(session.user_id);
    if (!user) {
      console.error(`%% %% %% no user found`);
      return handleUnauthSession(req, res);
    } else if (user.deleted) {
      console.error(`%% %% %% User deleted`);
      return res.status(403).send('This account has been deactivated. Please re-register to use the site');
    } else if (authService.isSessionExpired(session)) {
      console.error(`%% %% %% Session expired: ${JSON.stringify(session)}`);
      return handleUnauthSession(req, res);
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
      console.error(`Yaboo: ${err}`);
      wutil.logInternalServerError(`Auth validation error: ${err}`);
      return wutil.sendInternalServerError(`Auth validation error: ${err}`);
    }
    next();
  }
}
