'use strict';

import { default as express } from 'express';
import * as wutil from '../lib/webutils.mjs';
import { UserSavedData } from '../models/UserSavedData.mjs';
import { CookieNotFoundError, NoUserForSessionError, SessionExpiredError,
   SessionNotFoundError, SessionNotUsableError, UserDeletedError
  } from '../services/AuthService.mjs';

export { UserAPIController };

class UserAPIController {
  constructor(config, userService, translatorService) {
    this.config = config;
    this.userService = userService;
    this.translatorService = translatorService;
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

  // Preferences
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

  // Saves
  async getUserSaves(req, res, next) {
    let user_id = req.sessionData.user.id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let result = await this.userService.getUserSavesByUid(user_id, includeDeleted);
      if (!result || result.length === 0) {
        return res.status(200).json([]);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async updateUserSaves(req, res, next) {
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
          //await this.translatorService.retainQuery(pk);
        }
      }

      data.user_id = req.sessionData.user.id;
      let saveData = new UserSavedData(data);
      let result = await this.userService.saveUserData(saveData);
      if (!result) {
        return wutil.sendError(res, 400, `Error saving user data`);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async getUserSaveById(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = req.sessionData.user.id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let result = await this.userService.getUserSavesBy(user_id, {id: save_id}, includeDeleted);
      if (!result) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        return res.status(200).json(result[0]);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  /* Slight violation of REST in that PUT is supposed to take as input the entire object,
   * and we out here accepting partial fields. What. evah.
   */
  async updateUserSaveById(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = req.sessionData.user.id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let exists = await this.userService.getUserSavesBy(user_id, {id: save_id}, includeDeleted);
      if (!exists) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.userService.updateUserSave(req.body, includeDeleted);
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
  }

  async deleteUserSaveById(req, res, next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = req.sessionData.user.id;
    try {
      let exists = await this.userService.getUserSavesBy(user_id, {id: save_id});
      if (!exists) {
        return wutil.sendError(res, 404, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.userService.deleteUserSave(save_id);
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
  }

  async getUserWorkspaces(req, res, next) {
    let user_id = req.sessionData.user.id;
    let includeData = req.query.include_data === 'true';
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let result = await this.userService.getUserWorkspaces(user_id, includeData, includeDeleted);
      if (!result) {
        return wutil.sendError(result, 404, `No workspace data found for user id ${user_id}`);
      } else {
        return res.status(200).json(result);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }
}
