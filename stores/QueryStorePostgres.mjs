export { QueryStorePostgres };
import { pg, pgExec } from '../lib/postgres_preamble.mjs';
import { Query } from '../models/Query.mjs';

class QueryStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveQueryById(id) {
    return this._retrieveQueryById(id);
  }

  async retrieveQueryByPk(pk) {
    return this._retrieveQueryById(pk, 'pk');
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

  async addQueryUserRelationship(queryModel, userQueryModel) {
    const res = await pgExec(this.pool, `
      INSERT INTO query_to_user (qid, uid)
      VALUES ($1, $2)
      RETURNING *
    `, [queryModel.id, userQueryModel.user_id]);
    return res.rows.length === 1;
  }

  async get_stale_queries() {
    const res = await pgExec(this.pool, `
      SELECT *
      FROM queries
      WHERE status = 'running'
        AND time_updated < NOW() - INTERVAL '2 minutes';`);
    return res.rows.map(row => new Query(row));
  }

  async UNSAFE_batch_update(queries) {
    const sql_values = [];
    const params = [];
    let p = 1;
    for (let query of queries) {
      query.time_updated = new Date();
      sql_values.push(`($${p++}, $${p++}, $${p++}::jsonb`);
      params.push(query.pk, query.status, query.time_updated, query.metadata);
    }
    const res = await pgExec(this.pool `
      UPDATE queries AS q
      SET
        status = v.status,
        time_updated = v.time_updates,
        metadata = v.metadata
      FROM (
        VALUES ${sql_values.join(",\n")}
      ) AS v (pk, status, time_updated, metadata)
      WHERE q.pk = v.pk`);
    return true;
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
