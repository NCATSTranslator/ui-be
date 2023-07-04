'use strict';

import { UserPreference } from "./models/UserPreference.mjs";
import { User } from './models/User.mjs';

export { UserService };

class UserService {
  constructor(userStore, userPreferenceStore, userSavedDataStore) {
    this.userStore = userStore;
    this.preferenceStore = userPreferenceStore;
    this.saveDataStore = userSavedDataStore;
  }

  async getUserById(uid) {
    return this.userStore.retrieveUserById(uid);
  }

  async getUserPreferences(uid) {
    let res = await this.preferenceStore.retrieveUserPreferencesById(uid);
    if (!res) {
      return null;
    } else {
      return this.preferenceArrayToObject(res);
    }
  }

  async updateUserPreferences(uid, userPreferences) {
    userPreferences = this.preferenceObjectToArray(userPreferences).map((e) => new UserPreference(e));
    return this.preferenceStore.updateUserPreferences(uid, userPreferences);
  }

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
