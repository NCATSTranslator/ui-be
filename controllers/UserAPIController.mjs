'use strict';
export { UserAPIController };

import * as wutil from '../lib/webutils.mjs';
import { UserSavedData, SAVE_TYPE } from '../models/UserSavedData.mjs';
import * as cmn from '../lib/common.mjs';
import { CanvasRequestError } from "../models/Canvas.mjs";

class UserAPIController {
  constructor(config, user_service, translator_service) {
    this.config = config;
    this.user_service = user_service;
    this.translator_service = translator_service;
  }

  get_user(req, res, _next) {
    let session_data = req.sessionData;
    if (session_data && session_data.user) {
      return res.status(cmn.HTTP_CODE.SUCCESS).json(session_data.user);
    } else {
      wutil.send_internal_server_error(res, 'Server error: couldn\'t find attached user');
    }
  }

  // Preferences
  async get_user_prefs(req, res, _next) {
    let user_id = wutil.request_to_user_id(req);
    try {
      let result = await this.user_service.getUserPreferences(user_id);
      if (!result) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No preference data for user ${user_id}`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_prefs(req, res, _next) {
    let user_id = wutil.request_to_user_id(req);
    let preferences = req.body;
    try {
      let result = await this.user_service.updateUserPreferences(user_id, preferences);
      if (!result || result.length === 0) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Nothing was updated`);
      } else {
        result = await this.user_service.getUserPreferences(user_id);
        if (!result) {
          return wutil.send_error(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Error retrieving preferences after apparently successful update`);
        } else {
          return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
        }
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  // Queries
  async get_user_queries(req, res, next) {
    req = wutil.inject_query_params(req, {type: SAVE_TYPE.QUERY});
    if (req.query.type !== SAVE_TYPE.QUERY) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.get_user_saves(req, res, next);
  }

  // Projects
  async get_user_projects(req, res, _next) {
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === 'true';
    const [projects, err] = await this._get_user_saves_data(user_id, include_deleted, SAVE_TYPE.PROJECT);
    if (err !== null) {
      wutil.log_internal_server_error(req, `Failed to fetch projects from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to fetch projects from the database');
    }
    return res.status(cmn.HTTP_CODE.SUCCESS).json(projects);
  }

  async create_user_project(req, res, next) {
    const project = await req.body;
    if (project.title === undefined) return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing "title" field');
    if (!project.pks) return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing "pks" field');
    const user_save = {
      save_type: SAVE_TYPE.PROJECT,
      data: project
    };
    req.body = user_save;
    return this.update_user_saves(req, res, next);
  }

  async update_user_projects(req, res, _next) {
    const project_updates = await req.body;
    if (!cmn.is_array(project_updates)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_updates)}`);
    }
    for (const update of project_updates) {
      if (!update.id) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'All project updates require an ID');
      }
    }
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === 'true';
    const [projects, err] = await this._get_user_saves_data(user_id, include_deleted, SAVE_TYPE.PROJECT);
    if (err !== null) {
      wutil.log_internal_server_error(res, `Failed to fetch projects from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to fetch projects from the database');
    }
    const database_updates = [];
    for (const update of project_updates) {
      for (const project of projects) {
        if (project.id === update.id) {
          if (update.title) {
            project.data.title = update.title;
          }
          if (update.pks) {
            project.data.pks = update.pks;
          }
          database_updates.push(project);
        }
      }
    }
    try {
      const result = await this.user_service.updateUserSaveBatch(database_updates);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
    } catch (err) {
      return wutil.send_internal_server_error(res, `Error commiting projects updates to database. Got: ${err}`);
    }
  }
  async delete_user_projects(req, res, _next) {
    const project_ids = await req.body;
    if (!cmn.is_array(project_ids)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const user_id = wutil.request_to_user_id(req);
    try {
      const _ = await this.user_service.deleteUserSaveBatch(user_id, project_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, `Failed to update projects from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to update projects from the database');
    }
  }
  async restore_user_projects(req, res, _next) {
    const project_ids = await req.body;
    if (!cmn.is_array(project_ids)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const user_id = wutil.request_to_user_id(req);
    try {
      const _ = await this.user_service.restoreUserSaveBatch(user_id, project_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, `Failed to update projects from the database. Got error: ${err}`);
      return wutil.send_internal_server_error(res, 'Failed to update projects from the database');
    }
  }

  // Bookmarks
  async get_user_bookmarks(req, res, next) {
    req = wutil.inject_query_params(req, {type: SAVE_TYPE.BOOKMARK});
    if (req.query.type !== SAVE_TYPE.BOOKMARK) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.get_user_saves(req, res, next);
  }

  // Tags
  async get_user_tags(req, res, next) {
    req = wutil.inject_query_params(req, {type: SAVE_TYPE.TAG});
    if (req.query.type !== SAVE_TYPE.TAG) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.get_user_saves(req, res, next);
  }

  // Saves
  async get_user_saves(req, res, _next) {
    let user_id = wutil.request_to_user_id(req);
    let include_deleted = req.query.include_deleted === 'true';
    let save_type = req.query.type ? req.query.type : null;
    try {
      let result = await this.user_service.getUserSavesByUid(user_id, include_deleted, save_type);
      if (!result || result.length === 0) {
        return res.status(cmn.HTTP_CODE.SUCCESS).json([]);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_saves(req, res, _next) {
    try {
      let data = {...req.body};
      // TODO: generalize saving object behavior when we start saving more types
      if (data.save_type === SAVE_TYPE.BOOKMARK) {
        const pk = data.ars_pkey
        if (!pk) {
          const error = 'No PK in save for result';
          wutil.log_internal_server_error(req, error);
          return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, error);
        } else {
          req.log.info(`Retaining ${pk}`);
          await this.translator_service.retainQuery(pk);
        }
      }

      data.user_id = wutil.request_to_user_id(req);
      let save_data = new UserSavedData(data);
      let result = await this.user_service.saveUserData(save_data);
      if (!result) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `Error saving user data`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_save_by_id(req, res, _next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = wutil.request_to_user_id(req);
    let include_deleted = req.query.include_deleted === 'true';
    try {
      let result = await this.user_service.getUserSavesBy(user_id, {id: save_id}, include_deleted);
      if (!result) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result[0]);
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  /* Slight violation of REST in that PUT is supposed to take as input the entire object,
   * and we out here accepting partial fields. What. evah.
   */
  async update_user_save_by_id(req, res, _next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = wutil.request_to_user_id(req);
    let include_deleted = req.query.include_deleted === 'true';
    try {
      let exists = await this.user_service.getUserSavesBy(user_id, {id: save_id}, include_deleted);
      if (!exists) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.user_service.updateUserSave(req.body, include_deleted);
        if (!result) {
          return wutil.send_error(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Error saving data`);
        } else {
          return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
        }
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async delete_user_save_by_id(req, res, _next) {
    let save_id = parseInt(req.params.save_id, 10);
    let user_id = wutil.request_to_user_id(req);
    try {
      let exists = await this.user_service.getUserSavesBy(user_id, {id: save_id});
      if (!exists) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.user_service.deleteUserSave(save_id);
        if (!result) {
          return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, `No saved data found for id ${save_id}`);
        } else {
          return res.status(cmn.HTTP_CODE.NO_CONTENT).end();
        }
      }
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_canvases(req, res) {
    const user_id = req.sessionData.user.id;
    // TODO: Remove this check. user_id MUST be valid because of session management
    if (cmn.is_missing(user_id)) {
      wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, "No user ID");
      return;
    }
    // TODO: Make an abstraction for this check.
    const include_deleted = req.query.include_deleted === "true";
    try {
      const user_canvases = await this.user_service.get_user_canvases(user_id, include_deleted);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_canvases);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async create_user_canvas(req, res) {
    const user_id = req.sessionData.user.id;
    if (cmn.is_missing(user_id)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, "No user ID");
    }
    try {
      const canvas_req = req.body;
      const user_canvas = await this.user_service.create_user_canvas(user_id, canvas_req);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_canvas);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      } else {
        wutil.send_internal_server_error(res);
      }
    }
  }

  async _get_user_saves_data(user_id, include_deleted, save_type) {
    try {
      const result = await this.user_service.getUserSavesByUid(user_id, include_deleted, save_type);
      if (!result || result.length === 0) return [[], null];
      return [result, null];
    } catch (err) {
      return [null, err];
    }
  }
}
