'use strict';

export { UserPreference };

class UserPreference {
  constructor({
      user_id,
      pref_name,
      pref_value,
      pref_data_type = null,
      time_created = new Date(),
      time_updated = new Date()
  }) {
      this.user_id = user_id;
      this.pref_name = pref_name;
      this.pref_value = pref_value;
      this.pref_data_type = pref_data_type;
      this.time_created = time_created;
      this.time_updated = time_updated;
  }

  updatePreference(fields) {
      for (let key in fields) {
          if (this.hasOwnProperty(key)) {
              this[key] = fields[key];
          }
      }
      this.time_updated = new Date();
      return this;
  }

  updateValue(value) {
      this.pref_value = value;
      this.time_updated = new Date();
      return this;
  }
}
