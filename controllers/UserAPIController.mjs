'use strict';

import { default as express } from 'express';
import * as wutil from '../lib/webutils.mjs';
import { UserSavedData } from '../models/UserSavedData.mjs';
import { CookieNotFoundError, NoUserForSessionError, SessionExpiredError,
   SessionNotFoundError, SessionNotUsableError, UserDeletedError
  } from '../services/AuthService.mjs';

export { UserAPIController };

class UserAPIController {
  constructor(config, userService) {
    this.config = config;
    this.userService = userService;
  }

  getUser(req, res, next) {
    let sessionData = req.sessionData;
    console.log(sessionData);
    if (sessionData && sessionData.user) {
      return res.status(200).json(sessionData.user);
    } else {
      wutil.sendInternalServerError(res, 'Server error: couldn\'t find attached user');
    }
  }

  async getUserPrefs(req, res, next) {
    let user_id = req.sessionData.user.id;
    try {
      let result = await this.userService.getUserPreferences(user_id);
      if (!result) {
        return wutil.sendError(res, 404, `No preference data for user ${user_id}`);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async updateUserPrefs(req, res, next) {
    let user_id = req.sessionData.user.id;
    let preferences = req.body;
    try {
      let result = await this.userService.updateUserPreferences(user_id, preferences);
      if (!result || result.length === 0) {
        return wutil.sendError(res, 400, `Nothing was updated`);
      } else {
        result = await this.userService.getUserPreferences(user_id);
        if (!result) {
          return wutil.sendError(res, 500, `Error retrieving preferences after apparently successful update`);
        } else {
          return res.status(200).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }
}


function createUserController(config, services) {
  var router = express.Router();
  var translatorService = services.translatorService;
  var userService = services.userService;
  var authService = services.authService;

  router.all('/', function(req, res, next) {
    return res.status(403).json({message: 'Forbidden'});
  });

  // *=* router.use('/me', authenticateRequest(config, authService));

  router.get('/me', async function(req, res, next) {
      return res.status(200).json(req.user);
  });

  /* ** Preferences ** */
  router.get('/me/preferences', async function(req, res, next) {
    try {
      let result = await userService.getUserPreferences(req.user.id);
      if (!result) {
        return wutil.sendError(res, 404, `No preference data for user ${req.user.id}`);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.post('/me/preferences', async function(req, res, next) {
    let preferences = req.body;
    try {
      let result = await userService.updateUserPreferences(req.user.id, preferences);
      if (!result || result.length === 0) {
        return wutil.sendError(res, 400, `Nothing was updated`);
      } else {
        result = await userService.getUserPreferences(req.user.id);
        if (!result) {
          return wutil.sendError(res, 500, `Error retrieving preferences after apparently successful update`);
        } else {
          return res.status(200).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  /* ** Saves ** */
  router.get('/me/saves', async function(req, res, next) {
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let result = await userService.getUserSavesByUid(req.user.id, includeDeleted);
      if (!result || result.length === 0) {
        return res.status(200).json([]);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.post('/me/saves', async function(req, res, next) {
    try {
      let data = {...req.body};
      // TODO: generalize saving object behavior when we start saving more types
      if (data.save_type === 'bookmark') {
        const pk = data.ars_pkey
        if (!pk) {
          const error = 'No PK in save for result';
          wutil.logInternalServerError(req, error);
          return wutil.sendError(res, 400, error);
        } else {
          console.log(`Retaining ${pk}`);
          await translatorService.retainQuery(pk);
        }
      }

      data.user_id = req.user.id;
      let saveData = new UserSavedData(data);
      let result = await userService.saveUserData(saveData);
      if (!result) {
        return wutil.sendError(res, 400, `Error saving user data`);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.get('/me/saves/:save_id', async function(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let result = await userService.getUserSavesBy(req.user.id, {id: save_id}, includeDeleted);
      if (!result) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        return res.status(200).json(result[0]);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  /* Slight violation of REST in that PUT is supposed to take as input the entire object,
   * and we out here accepting partial fields. What. evah.
   */
  router.put('/me/saves/:save_id', async function(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let exists = await userService.getUserSavesBy(req.user.id, {id: save_id}, includeDeleted);
      if (!exists) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        let result = await userService.updateUserSave(req.body, includeDeleted);
        if (!result) {
          return wutil.sendError(res, 500, `Error saving data`);
        } else {
          return res.status(200).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.delete('/me/saves/:save_id', async function(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    try {
      let exists = await userService.getUserSavesBy(req.user.id, {id: save_id});
      if (!exists) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        let result = await userService.deleteUserSave(save_id);
        if (!result) {
          return wutil.sendError(res, 400, `No saved data found for id ${save_id}`);
        } else {
          return res.status(204).end();
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  return router;
}

function authenticateRequest(config, authService) {
  return async function (req, res, next) {
    let cookieName = config.session_cookie.name;
    let token = req.cookies[cookieName];
    let cookiePath = config.mainsite_path;
    let cookieMaxAge = authService.sessionAbsoluteTTLSec;

    let validationResult = null;
    try {
      validationResult = await authService.validateAuthSessionToken(token);
      req.user = validationResult.user;
      if (validationResult.tokenRefreshed) {
        wutil.setSessionCookie(res, cookieName, validationResult.session.token, cookiePath, cookieMaxAge);
      }
      next();
    } catch (err) {
      if (err instanceof CookieNotFoundError
          || err instanceof SessionNotFoundError
          || err instanceof SessionNotUsableError
          || err instanceof NoUserForSessionError
          || err instanceof SessionExpiredError) {
            return res.redirect(302, `/login`);
      } else if (err instanceof UserDeletedError) {
        return res.status(403).send(`This account has been deactivated. Please re-register to use the site`);
      } else {
        wutil.logInternalServerError(req, err);
        return wutil.sendInternalServerError(res);
      }
    }
  }
}
