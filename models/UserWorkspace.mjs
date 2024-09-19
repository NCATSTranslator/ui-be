'use strict';
import { v4 as uuidv4 } from 'uuid';
import { generateRandomAlphaNumString } from '../lib/common.mjs';
export { UserWorkspace };

class UserWorkspace {
  constructor({
    id = uuidv4(),
    name,
    description = null,
    label = generateRandomAlphaNumString(3,4),
    user_id,
    deleted = false,
    is_public = false,
    time_created = new Date(),
    time_updated = new Date(),
    data = null,
  } = {}) {
    if (!(user_id && name)) {
      throw new Error('user_id and name are both required');
    }
    this.id = id;
    this.name = name;
    this.description = description;
    this.label = label;
    this.user_id = user_id;
    this.deleted = deleted;
    this.is_public = is_public;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.data = data;
  }

  update(fields) {
    for (let key in fields) {
      if (this.hasOwnProperty(key)) {
        this[key] = fields[key];
      }
    }
    this.time_updated = new Date();
    return this;
  }

  delete() {
    this.deleted = true;
    this.time_updated = new Date();
    return this;
  }
}
