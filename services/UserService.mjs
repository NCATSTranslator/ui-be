'use strict';

import { UserPreference } from "../models/UserPreference.mjs";

export { UserService };

class UserService {
  constructor(userStore, userPreferenceStore, userSavedDataStore) {
    this.userStore = userStore;
    this.preferenceStore = userPreferenceStore;
    this.savedDataStore = userSavedDataStore;
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

  // Saves
  async getUserSavesByUid(uid, includeDeleted=false) {
    return this.savedDataStore.retrieveUserSavedDataByUserId(uid, includeDeleted);
  }

  async saveUserData(userData) {
    return this.savedDataStore.createUserSavedData(userData);
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
