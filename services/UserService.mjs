'use strict';
export { UserService };
import { UserPreference } from '../models/UserPreference.mjs';
import { UserSavedData, UserQueryData, SAVE_TYPE } from '../models/UserSavedData.mjs';
import { UserCanvas, CanvasGraph, make_user_canvas_from_req, make_canvas_update_from_req, make_canvas_element_update_from_req, make_graph_merge_from_req, make_graph_selection_from_req, make_graph_move_from_req, Graph } from "#model/Canvas.mjs";

class UserService {
  constructor(
      userStore,
      userPreferenceStore,
      userSavedDataStore,
      canvasStore,
      entitySigningSecret = null) {
    this.userStore = userStore;
    this.preferenceStore = userPreferenceStore;
    this.savedDataStore = userSavedDataStore;
    this.canvasStore = canvasStore;
    this.entitySigningSecret = entitySigningSecret;
  }

  async getUserById(uid) {
    return this.userStore.retrieveUserById(uid);
  }

  // Preferences
  async getUserPreferences(uid) {
    let res = await this.preferenceStore.retrieveUserPreferencesById(uid);
    if (!res) {
      return null;
    } else {
      return this.preferenceArrayToObject(res);
    }
  }

  async updateUserPreferences(uid, userPreferences) {
    let prefArray = this.preferenceObjectToArray(userPreferences).map((e) => new UserPreference(e));
    return this.preferenceStore.updateUserPreferences(uid, prefArray);
  }

  // Queries
  async createUserQuery(uid, pk, query) {
    const userSavedData = new UserSavedData({
      user_id: uid,
      save_type: SAVE_TYPE.QUERY,
      ars_pkey: pk,
      data: new UserQueryData(query)
    });
    return this.saveUserData(userSavedData);
  }

  async get_user_queries_map(uid, include_deleted, use_status) {
    return this.savedDataStore.retrieve_queries_map(uid, include_deleted, use_status);
  }

  // Saves
  async getUserSavesByUid(uid, includeDeleted=false, saveType=null) {
    return this.savedDataStore.retrieveUserSavedDataByUserId(uid, includeDeleted, saveType);
  }

  async saveUserData(userSavedData) {
    return this.savedDataStore.createUserSavedData(userSavedData);
  }

  async getUserSavesBy(uid, fields, includeDeleted=false) {
    return this.savedDataStore.retrieveUserSavedDataBy(uid, fields, includeDeleted);
  }

  async updateUserSave(saveData, includeDeleted=false) {
    return this.savedDataStore.updateUserSavedDataPartial(saveData, includeDeleted);
  }

  async updateUserSaveBatch(saveData) {
    return this.savedDataStore.updateUserSavedDataBatch(saveData);
  }

  async deleteUserSave(save_id) {
    return this.savedDataStore.deleteUserSavedDataById(save_id);
  }

  async deleteUserSaveBatch(uid, sids) {
    return this.savedDataStore.deleteUserSavedDataBatch(uid, sids);
  }

  async restoreUserSaveBatch(uid, sids) {
    return this.savedDataStore.restoreUserSavedDataBatch(uid, sids);
  }

  async get_user_canvases(user_id, include_deleted=false) {
    const rows = await this.canvasStore.get_canvases_by_user(user_id, include_deleted);
    const canvases = rows.map(row => new UserCanvas(row));
    return canvases;
  }

  async get_canvas_graph(user_id, canvas_id, include_deleted=false) {
    const graph = await this.canvasStore.get_canvas_graph_by_user(user_id, canvas_id, include_deleted);
    if (graph === null) return null;
    return new CanvasGraph(graph);
  }

  async merge_canvas_graph(user_id, canvas_id, graph_req) {
    const { graph, tag_descriptions } = make_graph_merge_from_req(graph_req, this.entitySigningSecret);
    const merged = await this.canvasStore.merge_canvas_graph(user_id, canvas_id, graph, tag_descriptions);
    if (merged === null) return null;
    return new CanvasGraph(merged);
  }

  async update_canvas_node(user_id, canvas_id, data_id, node_req) {
    const fields = make_canvas_element_update_from_req(node_req);
    return this.canvasStore.update_canvas_node_by_user(user_id, canvas_id, data_id, fields);
  }

  async update_canvas_edge(user_id, canvas_id, data_id, edge_req) {
    const fields = make_canvas_element_update_from_req(edge_req);
    return this.canvasStore.update_canvas_edge_by_user(user_id, canvas_id, data_id, fields);
  }

  async move_canvas_nodes(user_id, canvas_id, move_req) {
    const { moves } = make_graph_move_from_req(move_req);
    return this.canvasStore.move_canvas_nodes_by_user(user_id, canvas_id, moves);
  }

  async trash_canvas_graph(user_id, canvas_id, graph_req) {
    const { node_ids, edge_ids } = make_graph_selection_from_req(graph_req);
    const graph = await this.canvasStore.trash_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids);
    if (graph === null) return null;
    return new CanvasGraph(graph);
  }

  async restore_canvas_graph(user_id, canvas_id, graph_req) {
    const { node_ids, edge_ids } = make_graph_selection_from_req(graph_req);
    const graph = await this.canvasStore.restore_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids);
    if (graph === null) return null;
    return new CanvasGraph(graph);
  }

  async get_node_data(user_id, canvas_id, data_id) {
    return this.canvasStore.get_node_data(user_id, canvas_id, data_id);
  }

  async get_edge_data(user_id, canvas_id, data_id) {
    return this.canvasStore.get_edge_data(user_id, canvas_id, data_id);
  }

  async update_canvas(user_id, canvas_id, canvas_req) {
    const fields = make_canvas_update_from_req(canvas_req);
    const row = await this.canvasStore.update_canvas_by_user(user_id, canvas_id, fields);
    if (row === null) return null;
    const canvas = new UserCanvas({ user_id: user_id });
    canvas.populate_from_raw(row);
    return canvas;
  }

  async trash_canvases(user_id, canvas_ids) {
    return this.canvasStore.trash_canvases_by_user(user_id, canvas_ids);
  }

  async restore_canvases(user_id, canvas_ids) {
    return this.canvasStore.restore_canvases_by_user(user_id, canvas_ids);
  }

  async create_user_canvas(user_id, canvas_req) {
    const user_canvas = make_user_canvas_from_req(user_id, canvas_req);
    const graph = Graph.from_req(canvas_req, this.entitySigningSecret);
    graph.assert_edges_reference_nodes();
    const canvas = await this.canvasStore.create_user_canvas(user_canvas, graph);
    user_canvas.populate_from_raw(canvas);
    return user_canvas;
  }

  // Utils
  preferenceArrayToObject(arr)  {
    var retval = {
      user_id: arr[0].user_id,
      preferences: {}
    };

    arr.forEach(function(pref) {
      retval.preferences[pref.pref_name] = {
        pref_value: pref.pref_value,
        pref_data_type: pref.pref_data_type,
        time_created: pref.time_created,
        time_updated: pref.time_updated
      };
    });
    return retval;
  }

  preferenceObjectToArray(obj) {
    var retval = [];

    for (var pref_name in obj.preferences) {
      retval.push({
        user_id: obj.user_id,
        pref_name: pref_name,
        pref_value: obj.preferences[pref_name].pref_value,
        pref_data_type: obj.preferences[pref_name].pref_data_type,
        time_created: obj.preferences[pref_name].time_created,
        time_updated: obj.preferences[pref_name].time_updated
      });
    }
    return retval;
  }
}
