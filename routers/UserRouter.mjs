'use strict';

import { default as express } from 'express';
import { User } from '../models/User.mjs';
import * as wutil from '../webutils.mjs';

export { createUserRouter };

function createUserRouter(config, services) {
  var router = express.Router();
  var userService = services.userService;

  router.all('/', function(req, res, next) {
    return res.status(403).json({message: 'Forbidden'});
  });

  router.get('/:user_id', async function(req, res, next) {
    let uid = req.params.user_id;
    try {
      let result = await userService.getUserById(uid);
      if (result) {
        return res.status(200).json(result);
      } else {
        return res.status(404).json({
          no_such_user: uid
        });
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.get('/:user_id/preferences', async function(req, res, next) {
    let uid = req.params.user_id;
    try {
      let user = await userService.getUserById(uid);
      if (!user) {
        return wutil.sendError(res, 404, `No such user: ${uid}`);
      } else {
        let result = await userService.getUserPreferences(uid);
        if (!result) {
          return wutil.sendError(res, 404, `No preference data for user ${uid}`);
        } else {
          return res.status(200).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });

  router.post('/:user_id/preferences', async function(req, res, next) {
    let uid = req.params.user_id;
    let preferences = req.body;
    try {
      let user = await userService.getUserById(uid);
      if (!user) {
        return wutil.sendError(res, 404, `No such user ${uid}`);
      } else if (uid !== preferences.user_id) {
        return wutil.sendError(res, 422, `User ids in route (${uid}) and payload (${preferences.user_id}) do not match`);
      } else {
        let result = await userService.updateUserPreferences(uid, preferences);
        if (!result || result.length === 0) {
          return wutil.sendError(res, 400, `Nothing was updated`);
        } else {
          result = await userService.getUserPreferences(uid);
          if (!result) {
            return wutil.sendError(res, 400, `Error retrieving preferences after apparently successful update`);
          } else {
            return res.status(200).json(result);
          }
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }

  });

  router.get('/:user_id/saves', async function(req, res, next) {
    let uid = req.params.user_id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let user = await userService.getUserById(uid);
      if (!user) {
        return wutil.sendError(res, 404, `No such user ${uid}`);
      } else {
        let result = await userService.getUserSavesByUid(uid, includeDeleted);
        if (!result || result.length === 0) {
          return res.status(200).json([]);
        } else {
          return res.status(200).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  });
  return router;
}
