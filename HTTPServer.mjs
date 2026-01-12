'use strict'

import { logger } from './lib/logger.mjs';
import { default as path } from 'node:path';
import { default as url } from 'node:url';
import { default as express } from 'express';
import { default as pino } from 'pino';
import { default as pinoHttp } from 'pino-http';
import { default as cookieParser } from 'cookie-parser';

import { validateDemoQueryRequest, handleDemoQueryRequest } from './DemoQueryHandler.mjs';

// Controllers
import { ConfigAPIController } from './controllers/ConfigAPIController.mjs';
import { QueryAPIController } from './controllers/QueryAPIController.mjs';
import { LoginController } from './controllers/LoginController.mjs';
import { SessionController } from './controllers/SessionController.mjs';
import { UserAPIController } from './controllers/UserAPIController.mjs';

// Adapters
import { TranslatorServicexFEAdapter } from './adapters/TranslatorServicexFEAdapter.mjs';
import { QueryServicexFEAdapter } from './adapters/QueryServicexFEAdapter.mjs';

export function startServer(config, services) {

  const filters = {whitelistRx: /^ara-/}; // TODO: move to config file
  config.filters = filters;

  const authService = services.authService;
  const userService = services.userService;
  const translatorService = services.translatorService;
  const queryService = services.queryService;
  const demoQueries = config.frontend.cached_queries.filter(e => e.allow_inbound);
  const translatorServicexFEAdapter = new TranslatorServicexFEAdapter();
  const queryServicexFEAdapter = new QueryServicexFEAdapter();
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  const loginController = new LoginController(config, authService);
  const queryAPIController = new QueryAPIController(config,
    translatorService,
    translatorServicexFEAdapter,
    queryService,
    queryServicexFEAdapter,
    userService,
    filters);
  const configAPIController = new ConfigAPIController(config);
  const userAPIController = new UserAPIController(config, userService, translatorService);
  const sessionController = new SessionController(config, authService);
  const API_PATH_V1 = '/api/v1';
  const SITE_PATH_PREFIX = '';
  app.use(pinoHttp({logger: logger}));
  app.use(express.json({
    limit: config.json_payload_limit,
    verify: (req, res, buf, encoding) => {req.rawBody = buf;}
  }));
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
   * ** !!! Privileged routes below rely on this ^ taking place prior to their handlers being called !!! ***
   *
   * Note that the ordering of route handlers below is really important; do not add route handlers
   * without careful understanding of the fall-through.
   *
   */
  app.use(sessionController.attachSessionData.bind(sessionController));

  // Session status API
  app.get(`${API_PATH_V1}/session/status`, sessionController.getStatus.bind(sessionController));
  app.post(`${API_PATH_V1}/session/status`, sessionController.updateStatus.bind(sessionController));

  // Login/logout
  app.get('/oauth2/redir/:provider', loginController.authRedir.bind(loginController));
  app.get('/login', loginController.loginWithUna.bind(loginController));
  // *=* NEED TO KNOW HOW LOGOUT WORKS TODAY -- DO WE EVER SEE A BE REQ FOR LOGOUT??
  app.get('/logout', loginController.logout.bind(loginController));

  /* Config. ** Idiosyncratic decision alert!! **
   * Similar to the GET session status API, we choose to not have requests to the config API update the session status.
   * The reason is that this is completely open and app-wide info, requiring no auth and not tied to a user session.
   * If the FE app makes a request to this API, it is doing so for its own purposes, that may or may not be tied
   * to explicit user activity (e.g., the FE app needs this at startup regardless of whether there is a user
   * session active). To maintain consistency with the decision that only intentional user activity should count
   * towards updating session last-touch times, we therefore exclude this API from that set. */
  app.use(`${API_PATH_V1}/config`, configAPIController.getConfig.bind(configAPIController));

  /** All routes below this point MUST use one of authenticate[Un]PrivilegedRequest() **/

  // Submit query route: privileged session
  app.post(`${API_PATH_V1}/query`,
    sessionController.authenticatePrivilegedRequest.bind(sessionController),
    queryAPIController.submitQuery.bind(queryAPIController));
  // Query request routes: unprivileged session
  app.get(`${API_PATH_V1}/query/:qid/status`,
    sessionController.authenticateUnprivilegedRequest.bind(sessionController),
    queryAPIController.getQueryStatus.bind(queryAPIController));
  app.get(`${API_PATH_V1}/query/:qid/result`,
    sessionController.authenticateUnprivilegedRequest.bind(sessionController),
    queryAPIController.getQueryResult.bind(queryAPIController));
  // Query callback route: behind hmac
  app.post(`${API_PATH_V1}/query/update`, queryAPIController.updateQuery.bind(queryAPIController));

  // User routes: privileged
  //app.use(`${API_PATH_V1}/users`, sessionController.authenticatePrivilegedRequest.bind(sessionController));
  app.use(`${API_PATH_V1}/users`, sessionController.authenticatePrivilegedRequest.bind(sessionController));
  app.get(`${API_PATH_V1}/users/me`, userAPIController.getUser.bind(userAPIController));
  app.get(`${API_PATH_V1}/users/me/preferences`, userAPIController.getUserPrefs.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/preferences`, userAPIController.updateUserPrefs.bind(userAPIController));

  // User queries
  // Creation of user queries is done on submission. See the /query endpoint
  app.get(`${API_PATH_V1}/users/me/queries`, queryAPIController.getUserQueries.bind(queryAPIController));
  app.put(`${API_PATH_V1}/users/me/queries`, queryAPIController.update_user_query.bind(queryAPIController));
  app.put(`${API_PATH_V1}/users/me/queries/touch`, queryAPIController.touch_user_query.bind(queryAPIController));
  app.put(`${API_PATH_V1}/users/me/queries/trash`, queryAPIController.deleteUserQueries.bind(queryAPIController));
  app.put(`${API_PATH_V1}/users/me/queries/restore`, queryAPIController.restoreUserQueries.bind(queryAPIController));

  // User projects
  app.get(`${API_PATH_V1}/users/me/projects`, userAPIController.getUserProjects.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/projects`, userAPIController.createUserProject.bind(userAPIController));
  app.put(`${API_PATH_V1}/users/me/projects`, userAPIController.updateUserProjects.bind(userAPIController));
  app.put(`${API_PATH_V1}/users/me/projects/trash`, userAPIController.deleteUserProjects.bind(userAPIController));
  app.put(`${API_PATH_V1}/users/me/projects/restore`, userAPIController.restoreUserProjects.bind(userAPIController));

  // User bookmarks
  app.get(`${API_PATH_V1}/users/me/bookmarks`, userAPIController.getUserBookmarks.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/bookmarks`, userAPIController.updateUserSaves.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/bookmarks/:save_id`, userAPIController.updateUserSaveById.bind(userAPIController));

  // User tags
  app.get(`${API_PATH_V1}/users/me/tags`, userAPIController.getUserTags.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/tags`, userAPIController.updateUserSaves.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/tags/:save_id`, userAPIController.updateUserSaveById.bind(userAPIController));
  app.delete(`${API_PATH_V1}/users/me/tags/:save_id`, userAPIController.deleteUserSaveById.bind(userAPIController));

  // User saves
  app.get(`${API_PATH_V1}/users/me/saves`, userAPIController.getUserSaves.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/saves`, userAPIController.updateUserSaves.bind(userAPIController));
  app.get(`${API_PATH_V1}/users/me/saves/:save_id`, userAPIController.getUserSaveById.bind(userAPIController));
  app.put(`${API_PATH_V1}/users/me/saves/:save_id`, userAPIController.updateUserSaveById.bind(userAPIController));
  app.delete(`${API_PATH_V1}/users/me/saves/:save_id`, userAPIController.deleteUserSaveById.bind(userAPIController));
  // workspaces
  app.get(`${API_PATH_V1}/users/me/workspaces`, userAPIController.getUserWorkspaces.bind(userAPIController));
  app.get(`${API_PATH_V1}/users/me/workspaces/:ws_id`, userAPIController.getUserWorkspaceById.bind(userAPIController));
  app.post(`${API_PATH_V1}/users/me/workspaces`, userAPIController.createUserWorkspace.bind(userAPIController));
  app.put(`${API_PATH_V1}/users/me/workspaces/:ws_id`, userAPIController.updateUserWorkspace.bind(userAPIController));
  app.delete(`${API_PATH_V1}/users/me/workspaces/:ws_id`, userAPIController.deleteUserWorkspace.bind(userAPIController));

  app.all(['/api', '/api/*'], (req, res) => {
    return res.status(403).send('API action Forbidden');
  });

  // ** ** All routes below this point MUST be unprivileged ** **
  app.use(sessionController.authenticateUnprivilegedRequest.bind(sessionController));

  app.get('/demo/disease/:disease_id',
    validateDemoQueryRequest(demoQueries, 'id', (req) => { return req.params.disease_id }),
    handleDemoQueryRequest(SITE_PATH_PREFIX));
  app.get('/demo/gene/:gene_id',
    validateDemoQueryRequest(demoQueries, 'id', (req) => { return req.params.gene_id }),
    handleDemoQueryRequest(SITE_PATH_PREFIX));
  app.get('/demo/chemical/:chemical_id',
    validateDemoQueryRequest(demoQueries, 'id', (req) => { return req.params.chemical_id }),
    handleDemoQueryRequest(SITE_PATH_PREFIX));

  // Redirect old /main and /demo URLs
  app.all(['/main', '/main/*', '/demo', '/demo/*'], (req, res, next) => {
    if (['/main', '/demo'].includes(req.originalUrl)) {
      res.redirect(308, '/');
    }
    logger.info(`redirection: ${req.originalUrl} -> ${req.originalUrl.replace(/^\/(main|demo)/, '')}`);
    res.redirect(308, req.originalUrl.replace(/^\/(main|demo)/, ''));
  });

  // Any route not explicitly handled above should simply return the page skeleton and allow the FE to handle it
  app.all('*', (req, res, next) => {
    res.sendFile(path.join(__root, 'build/index.html'));
  });

  app.listen(8386);
  logger.info("Der Anfang ist das Ende und das Ende ist der Anfang");
}
