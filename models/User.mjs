'use strict';

import { v4 as uuidv4 } from 'uuid';

export { User };

class User {
  constructor({
    id = uuidv4(),
    name,
    email,
    time_created = new Date(),
    time_updated = new Date(),
    profile_pic_url = null,
    data = null,
  } = {}) {

    if (!name) {
      throw new Error("Name is required");
    } else if (!email) {
      throw new Error("Email is required");
    }

    this.id = id;
    this.name = name;
    this.email = email;
    this.timeCreated = time_created;
    this.timeUpdated = time_updated;
    this.profilePicUrl = profile_pic_url;
    this.data = data;
  }

  updateUpdatedTime(time = new Date()) {
    this.timeUpdated = time;
    return this;
  }

  updateUser(updatedFields = {}) {
    for (let key in updatedFields) {
      if (this.hasOwnProperty(key)) {
        this[key] = updatedFields[key];
      }
    }
    return this;
  }
}
