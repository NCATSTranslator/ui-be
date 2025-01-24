export { Query, QueryMetadata };

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
