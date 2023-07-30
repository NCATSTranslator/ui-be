'use strict';

import { default as express } from 'express';
import { User } from '../models/User.mjs';
import * as wutil from '../webutils.mjs';
import { UserSavedData } from '../models/UserSavedData.mjs';

export { createUserRouter };

function createUserRouter(config, services) {
  var router = express.Router();
  var userService = services.userService;

  router.all('/', function(req, res, next) {
    return res.status(403).json({message: 'Forbidden'});
  });

  router.param('user_id', async function(req, res, next, id) {
    try {
      let user = await userService.getUserById(id);
      if (!user) {
        wutil.sendError(res, 404, `No such user: ${id}`);
      } else {
        req.user = user;
        next();
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.get('/:user_id', async function(req, res, next) {
      return res.status(200).json(req.user);
  });

  /* ** Preferences ** */
  router.get('/:user_id/preferences', async function(req, res, next) {
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

  router.post('/:user_id/preferences', async function(req, res, next) {
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
  router.get('/:user_id/saves', async function(req, res, next) {
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

  router.post('/:user_id/saves', async function(req, res, next) {
    try {
      let saveData = new UserSavedData(req.body);
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

  router.get('/:user_id/saves/:save_id', async function(req, res, next) {
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
  router.put('/:user_id/saves/:save_id', async function(req, res, next) {
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

  router.delete('/:user_id/saves/:save_id', async function(req, res, next) {
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
