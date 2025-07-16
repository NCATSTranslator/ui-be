export { Query, QueryMetadata, gen_query_status};

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

class QueryStatus {
  constructor(kwargs) {
    const {
      status,
      pk,
      metadata,
      time_created,
      time_updated,
      data,
      deleted
    } = kwargs;
    const is_invalid = !status
      || !data
      || !pk
      || metadata.aras === undefined
      || deleted === undefined
    if (is_invalid) throw Error(`Invalid data when trying to construct QueryStatus: ${kwargs}`);
    this.status = (status === 'complete' ? 'success' : status);
    this.data = {
      qid: pk,
      aras: metadata.aras,
      title: 'dummy',
      bookmark_ids: data.bookmark_ids,
      note_ids: data.note_ids,
      time_created: time_created,
      time_updated: time_updated,
      deleted: deleted
    }
  }
}

function gen_query_status(data) {
  return new QueryStatus(data);
}
