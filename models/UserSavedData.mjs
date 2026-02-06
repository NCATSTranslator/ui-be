'use strict';
export {
  SAVE_TYPE,
  mark_user_query_unseen,
  mark_user_query_seen,
  UserSavedData,
  UserQueryData,
  UserTagData
};

const SAVE_TYPE = Object.freeze({
  PROJECT: 'project',
  QUERY: 'query',
  BOOKMARK: 'bookmark',
  TAG: 'tag'
});

function mark_user_query_unseen(saved_data) {
  if (!saved_data.save_type === SAVE_TYPE.QUERY) {
    throw Error('Developer Error in UserSavedData: mark_user_query_unseen given saved_data that does not have save_type === query');
  }
  saved_data.data.seen = false;
  return saved_data;
}

function mark_user_query_seen(saved_data) {
  if (!saved_data.save_type === SAVE_TYPE.QUERY) {
    throw Error('Developer Error in UserSavedData: mark_user_query_seen given saved_data that does not have save_type === query');
  }
  saved_data.data.seen = true;
  return saved_data
}

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

class UserQueryData {
  constructor(query) {
    this.description = query;
    this.last_seen = new Date();
    this.title = null;
    this.bookmark_ids = [];
    this.tag_ids = [];
    this.seen = false;
  }
}

class UserTagData {
  constructor(label) {
    if (!label) throw new Error('label is required');
    this.label = label;
  }
}
