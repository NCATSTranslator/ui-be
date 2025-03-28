'use strict';
export { UserService };
import { UserPreference } from '../models/UserPreference.mjs';
import { UserWorkspace } from '../models/UserWorkspace.mjs';
import { UserSavedData, UserQueryData, UserTagData, SAVE_TYPE } from '../models/UserSavedData.mjs';

class UserService {
  constructor(userStore, userPreferenceStore, userSavedDataStore, userWorkspaceStore) {
    this.userStore = userStore;
    this.preferenceStore = userPreferenceStore;
    this.savedDataStore = userSavedDataStore;
    this.userWorkspaceStore = userWorkspaceStore;
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
  async createUserQuery(uid, queryModel) {
    const userSavedData = new UserSavedData({
      user_id: uid,
      save_type: SAVE_TYPE.QUERY,
      ars_pkey: queryModel.pk,
      data: new UserQueryData(queryModel.metadata.query)
    });
    return this.saveUserData(userSavedData);
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

  async deleteUserSave(save_id) {
    return this.savedDataStore.deleteUserSavedDataById(save_id);
  }

  // Workspaces
  async getUserWorkspaces(uid, includeData=false, includeDeleted=false) {
    return this.userWorkspaceStore.retrieveWorkspacesByUserId(uid, includeData, includeDeleted);
  }

  async getUserWorkspaceById(ws_id, includeDeleted=false) {
    return this.userWorkspaceStore.retrieveWorkspaceById(ws_id, includeDeleted);
  }

  async createUserWorkspace(workspace) {
    return this.userWorkspaceStore.createUserWorkspace(workspace);
  }

  async updateUserWorkspace(workspace) {
    return this.userWorkspaceStore.updateUserWorkspace(workspace);
  }

  async deleteUserWorkspace(ws_id) {
    return this.userWorkspaceStore.deleteUserWorkspace(ws_id);
  }

  async updateUserWorkspaceVisibility(ws_id, is_public) {
    return this.userWorkspaceStore.updateUserWorkspaceVisibility(ws_id, is_public);
  }

  async updateUserWorkspaceLastUpdated(ws_id, last_updated=new Date()) {
    return this.userWorkspaceStore.updateUserWorkspaceLastUpdated(ws_id, last_updated);
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
