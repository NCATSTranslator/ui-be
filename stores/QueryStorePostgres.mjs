export { QueryStorePostgres };
import { pg, pgExec } from '../lib/postgres_preamble.mjs';
import { Query } from '../models/Query.mjs';

class QueryStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveQueryById(id) {
    return await this._retrieveQueryById(id);
  }

  async retrieveQueryByPk(pk) {
    return await this._retrieveQueryById(pk, 'pk');
  }

  async createQuery(model) {
    let query = null;
    const res = await pgExec(this.pool, `
      INSERT INTO queries (pk, status, time_created, time_updated, deleted, metadata)
      VALUES ($1, $2, $3, $4, $5, $6)
      RETURNING *
    `, [model.pk, model.status, model.time_created, model.time_updated, model.deleted, model.metadata]);
    if (res.rows.length > 0) {
      query = new Query(res.rows[0]);
    }
    return query;
  }

  async updateQuery(model) {
    let query = null;
    const res = await pgExec(this.pool, `
      UPDATE queries
      SET status = $1,
          time_updated = $2,
          metadata = $3
      WHERE pk = $4
      RETURNING *
    `, [model.status, model.time_updated, model.metadata, model.pk]);
    if (res.rows.length > 0) {
      query = new Query(res.rows[0]);
    }
    return query;
  }

  async _retrieveQueryById(id, idType = 'id') {
    let query = null;
    const res = await pgExec(this.pool, `SELECT * FROM queries WHERE ${idType} = $1`, [id]);
    if (res.rows.length > 0) {
      query = new Query(res.rows[0]);
    }
    return query;
  }
}
