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
import { BiolinkAPIController } from './controllers/BiolinkAPIController.mjs';

// Adapters
import { TranslatorServicexFEAdapter } from './adapters/TranslatorServicexFEAdapter.mjs';
import { QueryServicexFEAdapter } from './adapters/QueryServicexFEAdapter.mjs';

export function start_server(config, services) {

  const filters = {whitelistRx: /^ara-/}; // TODO: move to config file
  config.filters = filters;

  const auth_service = services.authService;
  const user_service = services.userService;
  const translator_service = services.translatorService;
  const query_service = services.queryService;
  const demo_queries = config.frontend.cached_queries.filter(e => e.allow_inbound);
  const translator_servicex_fe_adapter = new TranslatorServicexFEAdapter(
    config.feature_config, config.secrets.hmac.key);
  const query_servicex_fe_adapter = new QueryServicexFEAdapter();
  const __root = path.dirname(url.fileURLToPath(import.meta.url));
  const app = express();
  const login_controller = new LoginController(config, auth_service);
  const query_api_controller = new QueryAPIController(config,
    translator_service,
    translator_servicex_fe_adapter,
    query_service,
    query_servicex_fe_adapter,
    user_service,
    filters);
  const config_api_controller = new ConfigAPIController(config);
  const user_api_controller = new UserAPIController(config, user_service, translator_service);
  const session_controller = new SessionController(config, auth_service);
  const biolink_api_controller = new BiolinkAPIController();
  const API_PATH_V1 = '/api/v1';
  const SITE_PATH_PREFIX = '';
  app.use(pinoHttp({logger: logger}));
  app.use(express.json({
    limit: config.json_payload_limit,
    verify: (req, _res, buf, _encoding) => {req.rawBody = buf;}
  }));
  app.use(cookieParser());

  const build_dir = path.join(__root, 'build');
  app.use(express.static(build_dir, {
    setHeaders(res, filePath) {
      if (path.basename(filePath) === 'index.html') {
        res.setHeader('Cache-Control', 'no-cache');
      }
    },
  }));

  app.get('/health', (_req, res) => {res.send('OK');});

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
  app.use(session_controller.attachSessionData.bind(session_controller));

  // Session status API
  app.get(`${API_PATH_V1}/session/status`, session_controller.getStatus.bind(session_controller));
  app.post(`${API_PATH_V1}/session/status`, session_controller.updateStatus.bind(session_controller));

  // Login/logout
  app.get('/oauth2/redir/:provider', login_controller.authRedir.bind(login_controller));
  app.get('/login', login_controller.loginWithUna.bind(login_controller));
  // *=* NEED TO KNOW HOW LOGOUT WORKS TODAY -- DO WE EVER SEE A BE REQ FOR LOGOUT??
  app.get('/logout', login_controller.logout.bind(login_controller));

  /* Config. ** Idiosyncratic decision alert!! **
   * Similar to the GET session status API, we choose to not have requests to the config API update the session status.
   * The reason is that this is completely open and app-wide info, requiring no auth and not tied to a user session.
   * If the FE app makes a request to this API, it is doing so for its own purposes, that may or may not be tied
   * to explicit user activity (e.g., the FE app needs this at startup regardless of whether there is a user
   * session active). To maintain consistency with the decision that only intentional user activity should count
   * towards updating session last-touch times, we therefore exclude this API from that set. */
  app.use(`${API_PATH_V1}/config`, config_api_controller.getConfig.bind(config_api_controller));

  /** All routes below this point MUST use one of authenticate[Un]PrivilegedRequest() **/

  // Biolink routes: open
  app.post(`${API_PATH_V1}/biolink/node/description`,
    session_controller.authenticateUnprivilegedRequest.bind(session_controller),
    biolink_api_controller.get_node_descriptions.bind(biolink_api_controller));

  // Submit query route: privileged session
  app.post(`${API_PATH_V1}/query`,
    session_controller.authenticatePrivilegedRequest.bind(session_controller),
    query_api_controller.submit_query.bind(query_api_controller));
  // Query request routes: unprivileged session
  app.get(`${API_PATH_V1}/query/:qid/status`,
    session_controller.authenticateUnprivilegedRequest.bind(session_controller),
    query_api_controller.get_query_status.bind(query_api_controller));
  app.get(`${API_PATH_V1}/query/:qid/result`,
    session_controller.authenticateUnprivilegedRequest.bind(session_controller),
    query_api_controller.get_query_result.bind(query_api_controller));
  // Query callback route: behind hmac
  app.post(`${API_PATH_V1}/query/update`, query_api_controller.update_query.bind(query_api_controller));

  // User routes: privileged
  app.use(`${API_PATH_V1}/users`, session_controller.authenticatePrivilegedRequest.bind(session_controller));
  app.get(`${API_PATH_V1}/users/me`, user_api_controller.get_user.bind(user_api_controller));
  app.get(`${API_PATH_V1}/users/me/preferences`, user_api_controller.get_user_prefs.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/preferences`, user_api_controller.update_user_prefs.bind(user_api_controller));

  // User queries
  // Creation of user queries is done on submission. See the /query endpoint
  app.get(`${API_PATH_V1}/users/me/queries`, query_api_controller.get_user_queries.bind(query_api_controller));
  app.put(`${API_PATH_V1}/users/me/queries`, query_api_controller.update_user_query.bind(query_api_controller));
  app.put(`${API_PATH_V1}/users/me/queries/touch`, query_api_controller.touch_user_query.bind(query_api_controller));
  app.post(`${API_PATH_V1}/users/me/queries/copy`, query_api_controller.copy_user_query.bind(query_api_controller));
  app.put(`${API_PATH_V1}/users/me/queries/trash`, query_api_controller.delete_user_queries.bind(query_api_controller));
  app.put(`${API_PATH_V1}/users/me/queries/restore`, query_api_controller.restore_user_queries.bind(query_api_controller));

  // User projects
  app.get(`${API_PATH_V1}/users/me/projects`, user_api_controller.get_user_projects.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/projects`, user_api_controller.create_user_project.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/projects`, user_api_controller.update_user_projects.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/projects/trash`, user_api_controller.delete_user_projects.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/projects/restore`, user_api_controller.restore_user_projects.bind(user_api_controller));

  // User bookmarks
  app.get(`${API_PATH_V1}/users/me/bookmarks`, user_api_controller.get_user_bookmarks.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/bookmarks`, user_api_controller.update_user_saves.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/bookmarks/:save_id`, user_api_controller.update_user_save_by_id.bind(user_api_controller));

  // User tags
  app.get(`${API_PATH_V1}/users/me/tags`, user_api_controller.get_user_tags.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/tags`, user_api_controller.update_user_saves.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/tags/:save_id`, user_api_controller.update_user_save_by_id.bind(user_api_controller));
  app.delete(`${API_PATH_V1}/users/me/tags/:save_id`, user_api_controller.delete_user_save_by_id.bind(user_api_controller));

  // User saves
  app.get(`${API_PATH_V1}/users/me/saves`, user_api_controller.get_user_saves.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/saves`, user_api_controller.update_user_saves.bind(user_api_controller));
  app.get(`${API_PATH_V1}/users/me/saves/:save_id`, user_api_controller.get_user_save_by_id.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/saves/:save_id`, user_api_controller.update_user_save_by_id.bind(user_api_controller));
  app.delete(`${API_PATH_V1}/users/me/saves/:save_id`, user_api_controller.delete_user_save_by_id.bind(user_api_controller));

  // User canvas
  app.get(`${API_PATH_V1}/users/me/canvas`, user_api_controller.get_user_canvases.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/canvas`, user_api_controller.create_user_canvas.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/canvas/trash`, user_api_controller.trash_user_canvases.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/canvas/restore`, user_api_controller.restore_user_canvases.bind(user_api_controller));
  app.put(`${API_PATH_V1}/users/me/canvas/:save_id`, user_api_controller.update_user_canvas.bind(user_api_controller));
  app.get(`${API_PATH_V1}/users/me/canvas/:save_id/graph`, user_api_controller.get_user_canvas_graph.bind(user_api_controller));
  app.post(`${API_PATH_V1}/users/me/canvas/:save_id/graph`, user_api_controller.merge_user_canvas_graph.bind(user_api_controller));
  app.get(`${API_PATH_V1}/users/me/canvas/:save_id/node/:data_id`, user_api_controller.get_user_canvas_node_data.bind(user_api_controller));
  app.get(`${API_PATH_V1}/users/me/canvas/:save_id/edge/:data_id`, user_api_controller.get_user_canvas_edge_data.bind(user_api_controller));

  app.all(['/api', '/api/*'], (req, res) => {
    return res.status(403).send('API action Forbidden');
  });

  // ** ** All routes below this point MUST be unprivileged ** **
  app.use(session_controller.authenticateUnprivilegedRequest.bind(session_controller));

  app.get('/demo/disease/:disease_id',
    validateDemoQueryRequest(demo_queries, 'id', (req) => { return req.params.disease_id }),
    handleDemoQueryRequest(SITE_PATH_PREFIX));
  app.get('/demo/gene/:gene_id',
    validateDemoQueryRequest(demo_queries, 'id', (req) => { return req.params.gene_id }),
    handleDemoQueryRequest(SITE_PATH_PREFIX));
  app.get('/demo/chemical/:chemical_id',
    validateDemoQueryRequest(demo_queries, 'id', (req) => { return req.params.chemical_id }),
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
    res.setHeader('Cache-Control', 'no-cache');
    res.sendFile(path.join(build_dir, 'index.html'));
  });

  app.listen(8386);
  logger.info("Der Anfang ist das Ende und das Ende ist der Anfang");
}
