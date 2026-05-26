'use strict';
import { logger } from "./logger.mjs";
export { send_error, send_internal_server_error, log_internal_server_error, set_session_cookie, inject_query_params };

function send_error(res, error_code, trace) {
  logger.error(trace);
  const response = {
    'status': 'error',
    'data': trace
  }
  res.status(error_code).json(response);
}

function send_internal_server_error(res, trace=null) {
  send_error(res, 500, trace);
}

function log_internal_server_error(req, err) {
  req.log.error(`Internal Server Error: ${err}`);
}

function set_session_cookie(res, cookie_config, cookie_val, cookie_path, max_age_sec) {
  logger.debug(`set session cookie: [${cookie_config.name}/${max_age_sec}]: ${cookie_val}`);
  res.cookie(cookie_config.name, cookie_val, {
    maxAge: max_age_sec * 1000,
    path: cookie_path,
    httpOnly: cookie_config.http_only,
    secure: cookie_config.secure,
    sameSite: cookie_config.same_site
  });
}

function inject_query_params(req, params) {
  for (const key in params) {
    if (req.query[key] === undefined) {
      req.query[key] = params[key];
    }
  }
  return req;
}
