'use strict';
export { UserAPIController };
import * as wutil from '../lib/webutils.mjs';
import { UserSavedData, SAVE_TYPE } from '../models/UserSavedData.mjs';
import { UserWorkspace } from '../models/UserWorkspace.mjs';
import * as cmn from '../lib/common.mjs';

class UserAPIController {
  constructor(config, userService, translatorService) {
    this.config = config;
    this.userService = userService;
    this.translatorService = translatorService;
  }

  getUser(req, res, next) {
    let sessionData = req.sessionData;
    if (sessionData && sessionData.user) {
      return res.status(cmn.HTTP_CODE.SUCCESS).json(sessionData.user);
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
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No preference data for user ${user_id}`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
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
        return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Nothing was updated`);
      } else {
        result = await this.userService.getUserPreferences(user_id);
        if (!result) {
          return wutil.sendError(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Error retrieving preferences after apparently successful update`);
        } else {
          return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
        }
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  // Queries
  async getUserQueries(req, res, next) {
    req = wutil.injectQueryParams(req, {type: SAVE_TYPE.QUERY});
    if (req.query.type !== SAVE_TYPE.QUERY) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.getUserSaves(req, res, next);
  }

  // Projects
  async getUserProjects(req, res, next) {
    const user_id = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    const [projects, err] = await this._get_user_saves_data(user_id, include_deleted, SAVE_TYPE.PROJECT);
    if (err !== null) {
      wutil.logInternalServerError(req, `Failed to fetch projects from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to fetch projects from the database');
    }
    return res.status(cmn.HTTP_CODE.SUCCESS).json(projects);
  }
  async createUserProject(req, res, next) {
    const project = await req.body;
    if (!project.title) return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing "title" field');
    if (!project.pks) return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing "pks" field');
    const user_save = {
      save_type: SAVE_TYPE.PROJECT,
      data: project
    };
    req.body = user_save;
    return this.updateUserSaves(req, res, next);
  }
  async updateUserProjects(req, res, next) {
    const project_updates = await req.body;
    if (!cmn.isArray(project_updates)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_updates)}`);
    }
    for (const update of project_updates) {
      if (!update.id) {
        return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, 'All project updates require an ID');
      }
    }
    const user_id = req.sessionData.user.id;
    const include_deleted = req.query.include_deleted === 'true';
    const [projects, err] = await this._get_user_saves_data(user_id, include_deleted, SAVE_TYPE.PROJECT);
    if (err !== null) {
      wutil.logInternalServerError(res, `Failed to fetch projects from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to fetch projects from the database');
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
      const result = await this.userService.updateUserSaveBatch(database_updates);
      if (!result) {
        return res.status(cmn.HTTP_CODE.SUCCESS).json([]);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
      }
    } catch (err) {
      return wutil.sendInternalServerError(res, `Error commiting projects updates to database. Got: ${err}`);
    }
  }
  async deleteUserProjects(req, res, next) {
    const project_ids = await req.body;
    if (!cmn.isArray(project_ids)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const user_id = req.sessionData.user.id;
    try {
      const _ = await this.userService.deleteUserSaveBatch(user_id, project_ids);
      return res.status(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to update projects from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to update projects from the database');
    }
  }
  async restoreUserProjects(req, res, next) {
    const project_ids = await req.body;
    if (!cmn.isArray(project_ids)) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected body to be JSON array. Got: ${JSON.stringify(project_ids)}`);
    }
    const user_id = req.sessionData.user.id;
    try {
      const projects = await this.userService.restoreUserSaveBatch(user_id, project_ids);
      return res.status(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.logInternalServerError(req, `Failed to update projects from the database. Got error: ${err}`);
      return wutil.sendInternalServerError(res, 'Failed to update projects from the database');
    }
  }

  // Bookmarks
  async getUserBookmarks(req, res, next) {
    req = wutil.injectQueryParams(req, {type: SAVE_TYPE.BOOKMARK});
    if (req.query.type !== SAVE_TYPE.BOOKMARK) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.getUserSaves(req, res, next);
  }

  // Tags
  async getUserTags(req, res, next) {
    req = wutil.injectQueryParams(req, {type: SAVE_TYPE.TAG});
    if (req.query.type !== SAVE_TYPE.TAG) {
      return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Expected no save type, got: ${req.query.type}`);
    }
    return this.getUserSaves(req, res, next);
  }

  // Saves
  async getUserSaves(req, res, next) {
    let user_id = req.sessionData.user.id;
    let includeDeleted = req.query.include_deleted === 'true';
    let saveType = req.query.type ? req.query.type : null;
    try {
      let result = await this.userService.getUserSavesByUid(user_id, includeDeleted, saveType);
      if (!result || result.length === 0) {
        return res.status(cmn.HTTP_CODE.SUCCESS).json([]);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
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
      if (data.save_type === SAVE_TYPE.BOOKMARK) {
        const pk = data.ars_pkey
        if (!pk) {
          const error = 'No PK in save for result';
          wutil.logInternalServerError(req, error);
          return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, error);
        } else {
          req.log.info(`Retaining ${pk}`);
          await this.translatorService.retainQuery(pk);
        }
      }

      data.user_id = req.sessionData.user.id;
      let saveData = new UserSavedData(data);
      let result = await this.userService.saveUserData(saveData);
      if (!result) {
        return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `Error saving user data`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
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
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(result[0]);
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
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.userService.updateUserSave(req.body, includeDeleted);
        if (!result) {
          return wutil.sendError(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Error saving data`);
        } else {
          return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
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
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No saved data found for id ${save_id}`);
      } else {
        let result = await this.userService.deleteUserSave(save_id);
        if (!result) {
          return wutil.sendError(res, cmn.HTTP_CODE.BAD_REQUEST, `No saved data found for id ${save_id}`);
        } else {
          return res.status(cmn.HTTP_CODE.NO_CONTENT).end();
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
      let workspaces = await this.userService.getUserWorkspaces(user_id, includeData, includeDeleted);
      if (!workspaces) {
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No workspace data found for user id ${user_id}`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(workspaces);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async getUserWorkspaceById(req, res, next) {
    let user_id = req.sessionData.user.id;
    let ws_id = req.params.ws_id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let workspace = await this.userService.getUserWorkspaceById(ws_id, includeDeleted);
      if (!workspace) {
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `No workspace data found for user id ${user_id}`);
      } else if (user_id !== workspace.user_id) {
        // TODO: logic around is_public
        return wutil.sendError(res, cmn.HTTP_CODE.FORBIDDEN, `User ${user_id} does not have permission to access this workspace`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(workspace);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async createUserWorkspace(req, res, next) {
    try {
      let workspace = await this.userService.createUserWorkspace(new UserWorkspace(req.body));
      if (!workspace) {
        return wutil.sendError(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Server error creating workspace`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(workspace);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  // As with save data updates, we will accept a partial object
  async updateUserWorkspace(req, res, next) {
    let user_id = req.sessionData.user.id;
    let ws_id = req.params.ws_id;
    let includeDeleted = req.query.include_deleted === 'true';
    try {
      let workspace = await this.userService.getUserWorkspaceById(ws_id, includeDeleted);
      if (!workspace) {
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `Could not find workspace id ${ws_id}`);
      }
      if (user_id !== workspace.user_id) {
        return wutil.sendError(res, cmn.HTTP_CODE.FORBIDDEN, `User ${user_id} does not have permission to update this workspace`);
      }
      workspace = new UserWorkspace({...workspace, ...req.body});
      workspace = await this.userService.updateUserWorkspace(workspace);
      if (!workspace) {
        return wutil.sendError(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Server error updating workspace`);
      } else {
        return res.status(cmn.HTTP_CODE.SUCCESS).json(workspace);
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async deleteUserWorkspace(req, res, next) {
    let user_id = req.sessionData.user.id;
    let ws_id = req.params.ws_id;
    try {
      let workspace = await this.userService.getUserWorkspaceById(ws_id, true);
      if (!workspace) {
        return wutil.sendError(res, cmn.HTTP_CODE.NOT_FOUND, `Could not find workspace id ${ws_id}`);
      }
      if (user_id !== workspace.user_id) {
        return wutil.sendError(res, cmn.HTTP_CODE.FORBIDDEN, `User ${user_id} does not have permission to delete this workspace`);
      }
      let success = await this.userService.deleteUserWorkspace(ws_id);
      if (!success) {
        return wutil.sendError(res, cmn.HTTP_CODE.INTERNAL_ERROR, `Server error deleting workspace`);
      } else {
        return res.status(cmn.HTTP_CODE.NO_CONTENT).end();
      }
    } catch (err) {
      wutil.logInternalServerError(req, err);
      return wutil.sendInternalServerError(res);
    }
  }

  async _get_user_saves_data(user_id, include_deleted, save_type) {
    try {
      const result = await this.userService.getUserSavesByUid(user_id, include_deleted, save_type);
      if (!result || result.length === 0) return [[], null];
      return [result, null];
    } catch (err) {
      return [null, err];
    }
  }
}
