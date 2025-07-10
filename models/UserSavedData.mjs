'use strict';
export { UserSavedData, UserQueryData, UserTagData, SAVE_TYPE, as_project };

const SAVE_TYPE = Object.freeze({
  PROJECT: 'project',
  QUERY: 'query',
  BOOKMARK: 'bookmark',
  TAG: 'tag'
});

class UserSavedData {
  constructor({
    id = null,
    user_id,
    save_type = null,
    label = null,
    notes = null,
    ars_pkey = null,
    object_ref = null,
    time_created = new Date(),
    time_updated = new Date(),
    data = null,
    deleted = false
  } = {}) {
    if (!user_id) {
      throw new Error('user_id is required');
    }
    this.id = id;
    this.user_id = user_id;
    this.save_type = save_type;
    this.label = label;
    this.notes = notes;
    this.ars_pkey = ars_pkey;
    this.object_ref = object_ref;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.data = data;
    this.deleted = deleted;
  }

  updateSavedData(fields) {
    for (let key in fields) {
      if (this.hasOwnProperty(key)) {
        this[key] = fields[key];
      }
    }
    this.time_updated = new Date();
    return this;
  }

  deleteSavedData() {
    this.deleted = true;
    this.time_updated = new Date();
    return this;
  }
}

function as_project(user_save) {
  if (!user_save.data.title || !user_save.data.pks) {
    throw Error(`Error constructing Project from: ${JSON.stringify(user_save)}`);
  }
  return {
    id: user_save.id,
    title: user_save.data.title,
    qids: user_save.data.pks,
    time_created: user_save.time_created,
    time_update: user_save.time_updated,
    deleted: user_save.deleted
  }
}

class UserQueryData {
  constructor(description) {
    if (!description) throw new Error('description is required');
    this.description = description;
    this.bookmark_ids = [];
    this.tag_ids = [];
  }
}

class UserTagData {
  constructor(label) {
    if (!label) throw new Error('label is required');
    this.label = label;
  }
}
