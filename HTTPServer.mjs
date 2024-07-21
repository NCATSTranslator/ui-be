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
  const API_PATH_PREFIX = '/api/v1';

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
  app.get(`${API_PATH_PREFIX}/session/status`, sessionController.getStatus.bind(sessionController));
  app.post(`${API_PATH_PREFIX}/session/status`, sessionController.updateStatus.bind(sessionController));

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
  app.use(`${API_PATH_PREFIX}/config`, configAPIController.getConfig.bind(configAPIController));

  /** All routes below this point MUST use one of authenticate[Un]PrivilegedRequest() **/

  // Query routes: unprivileged
  app.use(`${API_PATH_PREFIX}/query`, sessionController.authenticateUnprivilegedRequest.bind(sessionController));
  app.post(`${API_PATH_PREFIX}/query`, queryAPIController.submitQuery.bind(queryAPIController));
  app.get(`${API_PATH_PREFIX}/query/:qid/status`, queryAPIController.getQueryStatus.bind(queryAPIController));
  app.get(`${API_PATH_PREFIX}/query/:qid/result`, queryAPIController.getQueryResult.bind(queryAPIController));

  // User routes: privileged
  app.use(`${API_PATH_PREFIX}/users`, sessionController.authenticatePrivilegedRequest.bind(sessionController));
  app.get(`${API_PATH_PREFIX}/users/me`, userAPIController.getUser.bind(userAPIController));
  app.get(`${API_PATH_PREFIX}/users/me/preferences`, userAPIController.getUserPrefs.bind(userAPIController));
  app.post(`${API_PATH_PREFIX}/users/me/preferences`, userAPIController.updateUserPrefs.bind(userAPIController));
  app.get(`${API_PATH_PREFIX}/users/me/saves`, userAPIController.getUserSaves.bind(userAPIController));
  app.post(`${API_PATH_PREFIX}/users/me/saves`, userAPIController.updateUserSaves.bind(userAPIController));
  app.get(`${API_PATH_PREFIX}/users/me/saves/:save_id`, userAPIController.getUserSaveById.bind(userAPIController));
  app.put(`${API_PATH_PREFIX}/users/me/saves/:save_id`, userAPIController.updateUserSaveById.bind(userAPIController));
  app.delete(`${API_PATH_PREFIX}/users/me/saves/:save_id`, userAPIController.deleteUserSaveById.bind(userAPIController));
  // workspaces
  app.get(`${API_PATH_PREFIX}/users/me/workspaces`, userAPIController.getUserWorkspaces.bind(userAPIController));
  app.get(`${API_PATH_PREFIX}/users/me/workspaces/:ws_id`, userAPIController.getUserWorkspaceById.bind(userAPIController));
  app.post(`${API_PATH_PREFIX}/users/me/workspaces`, userAPIController.createUserWorkspace.bind(userAPIController));


  app.all(['/api', '/api/*'], (req, res) => {
    return res.status(403).send('API action Forbidden');
  });

  // All routes below this point MUST be unprivileged
  app.use(sessionController.authenticateUnprivilegedRequest.bind(sessionController));

  // -- THIS IS THE CUTOFF POINT -- OLDER STUFF THAT SHOULD DIE SOON --

  app.get('/main/logout.html',  (req, res, next) => {
    res.sendFile(path.join(__root, 'build/logout.html'));
  });

  // *=* Still TODO: the /demo/disease route handling.
  // Aside from ^, all other routes should now simply return the page skeleton and allow the FE to handle the route
  app.all('*', (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });

/** ** **
  // FIGURE THESE GUYS OUT
  app.get('/demo/disease/:disease_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.disease_id }),
    handleDemoQueryRequest(config.demosite_path));
  app.get('/demo/gene/:gene_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.gene_id }),
    handleDemoQueryRequest(config.demosite_path));
  app.get('/demo/chemical/:chemical_id',
    validateDemoQueryRequest(true, demoQueries, 'id', (req) => { return req.params.chemical_id }),
    handleDemoQueryRequest(config.demosite_path));
*/

  app.listen(8386);
  console.log("Der Anfang ist das Ende und das Ende ist der Anfang");
}
