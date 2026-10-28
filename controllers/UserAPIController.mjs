'use strict';
export { UserAPIController };

import * as wutil from '../lib/webutils.mjs';
import { UserSavedData, SAVE_TYPE } from '../models/UserSavedData.mjs';
import * as cmn from '../lib/common.mjs';
import { CanvasRequestError, CanvasConflictError } from "../models/Canvas.mjs";
import { UserEntityRequestError } from "../models/UserEntity.mjs";
import { FilterStateRequestError } from "../models/FilterState.mjs";
import { QueryViewRequestError } from "../models/QueryView.mjs";
import { ApiKeyLimitError, API_KEY_NAME_MAX_LEN, API_KEY_MAX_TTL_DAYS, api_key_expiry,
  parse_api_key_expiry } from '../models/ApiKey.mjs';

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

  // API keys
  async get_user_api_keys(req, res, _next) {
    const user_id = wutil.request_to_user_id(req);
    const include_revoked = req.query.include_revoked === 'true';
    const include_expired = req.query.include_expired === 'true';
    try {
      const result = await this.user_service.getUserApiKeys(user_id, include_revoked, include_expired);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  /* The raw key is included in this response and nowhere else. */
  async create_user_api_key(req, res, _next) {
    const user_id = wutil.request_to_user_id(req);
    const raw_name = req.body ? req.body.name : null;
    if (!cmn.is_string(raw_name)) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing or invalid "name" field');
    }
    const name = raw_name.trim();
    if (name.length === 0) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing or invalid "name" field');
    }
    if (name.length > API_KEY_NAME_MAX_LEN) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `"name" exceeds the maximum length of ${API_KEY_NAME_MAX_LEN} characters`);
    }
    const time_expires = req.body.time_expires === undefined
      ? api_key_expiry()
      : parse_api_key_expiry(req.body.time_expires);
    if (time_expires === null) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `"time_expires" must be an ISO 8601 date-time in the future and at most ${API_KEY_MAX_TTL_DAYS} days away`);
    }
    try {
      const result = await this.user_service.createUserApiKey(user_id, name, time_expires);
      if (result === null) {
        return wutil.send_internal_server_error(res, 'Failed to create API key');
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json({
        api_key: result.apiKey,
        key: result.key
      });
    } catch (err) {
      if (err instanceof ApiKeyLimitError) {
        return wutil.send_error(res, cmn.HTTP_CODE.CONFLICT, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async revoke_user_api_key(req, res, _next) {
    const user_id = wutil.request_to_user_id(req);
    const key_id = req.key_id;
    try {
      const result = await this.user_service.revokeUserApiKey(user_id, key_id);
      if (result === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND,
          `No active API key ${key_id} for user ${user_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(result);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
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
    if (project?.title === undefined) return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, 'Missing "title" field');
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
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === "true";
    try {
      const user_canvases = await this.user_service.get_user_canvases(user_id, include_deleted);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_canvases);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_canvas_graph(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const include_deleted = req.query.include_deleted === "true";
    try {
      const graph = await this.user_service.get_canvas_graph(user_id, canvas_id, include_deleted);
      if (graph === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(graph);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_canvas(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const canvas = await this.user_service.update_canvas(user_id, canvas_id, req.body);
      if (canvas === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(canvas);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async trash_user_canvases(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_ids = req.body;
    if (!cmn.is_array(canvas_ids) || !canvas_ids.every((id) => Number.isInteger(id))) {
      req.log.warn(`Malformed canvas trash request: ${JSON.stringify(canvas_ids)}`);
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `Expected body to be a JSON array of canvas IDs. Got: ${JSON.stringify(canvas_ids)}`);
    }
    try {
      await this.user_service.trash_canvases(user_id, canvas_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async restore_user_canvases(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_ids = req.body;
    if (!cmn.is_array(canvas_ids) || !canvas_ids.every((id) => Number.isInteger(id))) {
      req.log.warn(`Malformed canvas restore request: ${JSON.stringify(canvas_ids)}`);
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `Expected body to be a JSON array of canvas IDs. Got: ${JSON.stringify(canvas_ids)}`);
    }
    try {
      await this.user_service.restore_canvases(user_id, canvas_ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async merge_user_canvas_graph(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const graph = await this.user_service.merge_canvas_graph(user_id, canvas_id, req.body);
      if (graph === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(graph);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      if (err instanceof CanvasConflictError) {
        return wutil.send_error(res, cmn.HTTP_CODE.CONFLICT, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_canvas_node(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const entity_id = req.entity_id;
    try {
      const node = await this.user_service.update_canvas_node(user_id, canvas_id, entity_id, req.body);
      if (node === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No node found for id ${entity_id} on canvas ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(node);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      if (err instanceof CanvasConflictError) {
        return wutil.send_error(res, cmn.HTTP_CODE.CONFLICT, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_canvas_edge(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const entity_id = req.entity_id;
    try {
      const edge = await this.user_service.update_canvas_edge(user_id, canvas_id, entity_id, req.body);
      if (edge === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No edge found for id ${entity_id} on canvas ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(edge);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      if (err instanceof CanvasConflictError) {
        return wutil.send_error(res, cmn.HTTP_CODE.CONFLICT, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async set_user_canvas_graph_geometry(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const geometry = await this.user_service.set_canvas_graph_geometry(user_id, canvas_id, req.body);
      if (geometry === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(geometry);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async trash_user_canvas_graph(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const graph = await this.user_service.trash_canvas_graph(user_id, canvas_id, req.body);
      if (graph === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(graph);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async restore_user_canvas_graph(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const graph = await this.user_service.restore_canvas_graph(user_id, canvas_id, req.body);
      if (graph === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(graph);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async create_user_canvas_annotation(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    try {
      const annotation = await this.user_service.create_canvas_annotation(user_id, canvas_id, req.body);
      if (annotation === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No canvas found for id ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(annotation);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async update_user_canvas_annotation_content(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const annotation_id = req.annotation_id;
    try {
      const annotation = await this.user_service.update_canvas_annotation_content(
        user_id, canvas_id, annotation_id, req.body);
      if (annotation === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND,
          `No annotation found for id ${annotation_id} on canvas ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(annotation);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_canvas_node_data(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const entity_id = req.entity_id;
    try {
      const data = await this.user_service.get_node_data(user_id, canvas_id, entity_id);
      if (data === null) {
        req.log.warn(`No node found for id ${entity_id} on canvas ${canvas_id}`);
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No node found for id ${entity_id} on canvas ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(data);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_canvas_edge_data(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const canvas_id = req.canvas_id;
    const entity_id = req.entity_id;
    try {
      const data = await this.user_service.get_edge_data(user_id, canvas_id, entity_id);
      if (data === null) {
        req.log.warn(`No edge found for id ${entity_id} on canvas ${canvas_id}`);
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No edge found for id ${entity_id} on canvas ${canvas_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(data);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async get_user_nodes(req, res) {
    return this._get_user_entities(req, res,
      (user_id, include_deleted) => this.user_service.get_user_nodes(user_id, include_deleted));
  }

  async get_user_edges(req, res) {
    return this._get_user_entities(req, res,
      (user_id, include_deleted) => this.user_service.get_user_edges(user_id, include_deleted));
  }

  async get_user_node(req, res) {
    return this._get_user_entities(req, res, async (user_id, include_deleted) =>
      (await this.user_service.get_user_nodes(user_id, include_deleted, [req.entity_id]))[0]);
  }

  async get_user_edge(req, res) {
    return this._get_user_entities(req, res, async (user_id, include_deleted) =>
      (await this.user_service.get_user_edges(user_id, include_deleted, [req.entity_id]))[0]);
  }

  async create_user_node(req, res) {
    return this._create_user_entity(req, res, async (user_id, entity_req) =>
      (await this.user_service.create_user_nodes(user_id, [entity_req]))[0]);
  }

  async create_user_edge(req, res) {
    return this._create_user_entity(req, res, async (user_id, entity_req) =>
      (await this.user_service.create_user_edges(user_id, [entity_req]))[0]);
  }

  async update_user_node(req, res) {
    return this._update_user_entity(req, res, async (user_id, update_req) =>
      (await this.user_service.update_user_nodes(user_id, [{ id: req.entity_id, update_req: update_req }]))[0]);
  }

  async update_user_edge(req, res) {
    return this._update_user_entity(req, res, async (user_id, update_req) =>
      (await this.user_service.update_user_edges(user_id, [{ id: req.entity_id, update_req: update_req }]))[0]);
  }

  async trash_user_nodes(req, res) {
    return this._set_user_entities_deleted(req, res,
      (user_id, ids) => this.user_service.trash_user_nodes(user_id, ids));
  }

  async trash_user_edges(req, res) {
    return this._set_user_entities_deleted(req, res,
      (user_id, ids) => this.user_service.trash_user_edges(user_id, ids));
  }

  async restore_user_nodes(req, res) {
    return this._set_user_entities_deleted(req, res,
      (user_id, ids) => this.user_service.restore_user_nodes(user_id, ids));
  }

  async restore_user_edges(req, res) {
    return this._set_user_entities_deleted(req, res,
      (user_id, ids) => this.user_service.restore_user_edges(user_id, ids));
  }

  async get_filter_states(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === "true";
    const include_hidden = req.query.include_hidden === "true";
    try {
      const filter_states = await this.user_service.get_filter_states(user_id, include_deleted, include_hidden);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(filter_states);
    } catch (err) {
      return this._send_filter_state_error(req, res, err);
    }
  }

  async get_filter_state(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === "true";
    try {
      const [filter_state] = await this.user_service.get_filter_states(
        user_id, include_deleted, true, [req.filter_state_id]);
      if (filter_state === undefined) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No filter state found for id ${req.filter_state_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(filter_state);
    } catch (err) {
      return this._send_filter_state_error(req, res, err);
    }
  }

  async create_filter_state(req, res) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const [filter_state] = await this.user_service.create_filter_states(user_id, [req.body]);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(filter_state);
    } catch (err) {
      return this._send_filter_state_error(req, res, err);
    }
  }

  async update_filter_state(req, res) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const [filter_state] = await this.user_service.update_filter_states(
        user_id, [{ id: req.filter_state_id, update_req: req.body }]);
      if (filter_state === undefined) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No filter state found for id ${req.filter_state_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(filter_state);
    } catch (err) {
      return this._send_filter_state_error(req, res, err);
    }
  }

  async trash_filter_states(req, res) {
    return this._set_filter_states_deleted(req, res,
      (user_id, ids) => this.user_service.trash_filter_states(user_id, ids));
  }

  async restore_filter_states(req, res) {
    return this._set_filter_states_deleted(req, res,
      (user_id, ids) => this.user_service.restore_filter_states(user_id, ids));
  }

  async get_query_views(req, res) {
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === "true";
    try {
      const query_views = await this.user_service.get_query_views(user_id, req.qid, include_deleted);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(query_views);
    } catch (err) {
      return this._send_query_view_error(req, res, err);
    }
  }

  async create_query_view(req, res) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const query_view = await this.user_service.create_query_view(user_id, req.qid, req.body);
      if (query_view === null) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No query found for qid ${req.qid}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(query_view);
    } catch (err) {
      return this._send_query_view_error(req, res, err);
    }
  }

  async trash_query_views(req, res) {
    return this._set_query_views_deleted(req, res,
      (user_id, query_pk, ids) => this.user_service.trash_query_views(user_id, query_pk, ids));
  }

  async restore_query_views(req, res) {
    return this._set_query_views_deleted(req, res,
      (user_id, query_pk, ids) => this.user_service.restore_query_views(user_id, query_pk, ids));
  }

  async _get_user_entities(req, res, get_entities) {
    const user_id = wutil.request_to_user_id(req);
    const include_deleted = req.query.include_deleted === "true";
    try {
      const entities = await get_entities(user_id, include_deleted);
      if (entities === undefined) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No user entity found for id ${req.entity_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(entities);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async _create_user_entity(req, res, create_entity) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const entity = await create_entity(user_id, req.body);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(entity);
    } catch (err) {
      if (err instanceof UserEntityRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async _update_user_entity(req, res, update_entity) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const entity = await update_entity(user_id, req.body);
      if (entity === undefined) {
        return wutil.send_error(res, cmn.HTTP_CODE.NOT_FOUND, `No user entity found for id ${req.entity_id}`);
      }
      return res.status(cmn.HTTP_CODE.SUCCESS).json(entity);
    } catch (err) {
      if (err instanceof UserEntityRequestError) {
        return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      }
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async _set_user_entities_deleted(req, res, set_deleted) {
    const user_id = wutil.request_to_user_id(req);
    const ids = req.body;
    if (!cmn.is_array(ids) || !ids.every((id) => Number.isInteger(id))) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `Expected body to be a JSON array of user entity IDs. Got: ${JSON.stringify(ids)}`);
    }
    try {
      await set_deleted(user_id, ids);
      return res.sendStatus(cmn.HTTP_CODE.SUCCESS);
    } catch (err) {
      wutil.log_internal_server_error(req, err);
      return wutil.send_internal_server_error(res);
    }
  }

  async _set_filter_states_deleted(req, res, set_deleted) {
    const user_id = wutil.request_to_user_id(req);
    const ids = req.body;
    if (!cmn.is_array(ids) || !ids.every((id) => Number.isInteger(id))) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `Expected body to be a JSON array of filter state IDs. Got: ${JSON.stringify(ids)}`);
    }
    try {
      const affected_ids = await set_deleted(user_id, ids);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(affected_ids);
    } catch (err) {
      return this._send_filter_state_error(req, res, err);
    }
  }

  _send_filter_state_error(req, res, err) {
    if (err instanceof FilterStateRequestError) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
    }
    wutil.log_internal_server_error(req, err);
    return wutil.send_internal_server_error(res);
  }

  async _set_query_views_deleted(req, res, set_deleted) {
    const user_id = wutil.request_to_user_id(req);
    const ids = req.body;
    if (!cmn.is_array(ids) || !ids.every((id) => Number.isInteger(id))) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST,
        `Expected body to be a JSON array of query view IDs. Got: ${JSON.stringify(ids)}`);
    }
    try {
      const affected_ids = await set_deleted(user_id, req.qid, ids);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(affected_ids);
    } catch (err) {
      return this._send_query_view_error(req, res, err);
    }
  }

  _send_query_view_error(req, res, err) {
    if (err instanceof QueryViewRequestError) {
      return wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
    }
    wutil.log_internal_server_error(req, err);
    return wutil.send_internal_server_error(res);
  }

  async create_user_canvas(req, res) {
    const user_id = wutil.request_to_user_id(req);
    try {
      const canvas_req = req.body;
      const user_canvas = await this.user_service.create_user_canvas(user_id, canvas_req);
      return res.status(cmn.HTTP_CODE.SUCCESS).json(user_canvas);
    } catch (err) {
      if (err instanceof CanvasRequestError) {
        wutil.send_error(res, cmn.HTTP_CODE.BAD_REQUEST, err.message);
      } else if (err instanceof CanvasConflictError) {
        wutil.send_error(res, cmn.HTTP_CODE.CONFLICT, err.message);
      } else {
        wutil.log_internal_server_error(req, err);
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
