export { Query, QueryMetadata, gen_user_query };

import * as cmn from '../lib/common.mjs';

class Query {
  constructor({
    id = null,
    pk,
    status = cmn.QUERY_STATUS.RUNNING,
    time_created = new Date(),
    time_updated = new Date(),
    deleted = false,
    metadata = new QueryMetadata(null)
  } = {}) {
    if (!pk) {
      throw new Error('PK is required');
    }
    this.id = id;
    this.pk = pk;
    this.status = status;
    this.time_created = time_created;
    this.time_updated = time_updated;
    this.deleted = deleted;
    this.metadata = metadata;
  }

  setAras(aras) {
    this.metadata.aras = aras;
    this.time_updated = new Date();
    return this;
  }

  setStatus(status) {
    this.status = status;
    this.time_updated = new Date();
    return this;
  }

  delete() {
    this.deleted = true;
    this.time_updated = new Date();
    return this;
  }
}

class QueryMetadata {
  constructor(query, aras = []) {
    this.query = query;
    this.aras = aras;
  }
}

class UserQuery {
  constructor(kwargs) {
    const {
      sid,
      status,
      pk,
      aras,
      query,
      title,
      time_created,
      time_updated,
      deleted
    } = kwargs;
    const is_invalid = !sid
      || !status
      || !pk
      || aras === undefined
      || deleted === undefined;
    if (is_invalid) throw Error(`Invalid data when trying to construct UserQuery: ${kwargs}`);
    this.sid = sid;
    this.status = status;
    this.data = {
      qid: pk,
      aras: aras,
      title: title,
      query: query,
      bookmark_ids: [],
      note_count: 0,
      time_created: time_created,
      time_updated: time_updated,
      deleted: deleted
    }
  }

  push_bookmark(bid) {
    this.data.bookmark_ids.push(bid);
  }

  add_note() {
    this.data.note_count += 1;
  }
}

function gen_user_query(data) {
  let status = null;
  let aras = null;
  if (data.status && data.metadata.aras ) {
    status = data.status;
    aras = data.metadata.aras;
  } else if (data.status === undefined) {
    status = cmn.QUERY_STATUS.RUNNING;
    aras = [];
  } else {
    throw new Error(`Could not generate UserQuery from given data: ${data}`);
  }
  return new UserQuery({
    sid: data.sid,
    status: status,
    pk: data.pk,
    aras: aras,
    query: data.data.description,
    title: data.data.title || null,
    time_created: data.time_created,
    time_updated: data.time_updated,
    deleted: data.deleted
  });
}
