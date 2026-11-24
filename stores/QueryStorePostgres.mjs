export { QueryStorePostgres };
import { pg, pgExec, pgExecTrans } from '../lib/postgres_preamble.mjs';
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

  async get_stale_queries() {
    const res = await pgExec(this.pool, `
      SELECT *
      FROM queries
      WHERE status = 'running'
        AND time_updated < NOW() - INTERVAL '2 minutes';`);
    return res.rows.map(row => new Query(row));
  }

  async delete_unowned_queries() {
    const res = await pgExec(this.pool, `
      DELETE FROM queries AS q
      WHERE q.time_created < NOW() - INTERVAL '30 days'
        AND NOT EXISTS (SELECT 1 FROM query_to_user AS qtu WHERE qtu.qid = q.id);`);
    return res.rowCount;
  }

  async delete_lost_queries(pks) {
    return pgExecTrans(this.pool, async (client) => {
      const lost = await client.query(`
        SELECT id, pk
        FROM queries
        WHERE pk = ANY($1::uuid[])
          AND status = 'running'
          AND time_created < NOW() - INTERVAL '24 hours'
        FOR UPDATE
      `, [pks]);
      if (lost.rows.length === 0) return 0;
      const lost_ids = lost.rows.map(row => row.id);
      const lost_pks = lost.rows.map(row => row.pk);
      await client.query(`DELETE FROM query_working_view WHERE qid = ANY($1::int[])`, [lost_ids]);
      await client.query(`DELETE FROM query_view WHERE qid = ANY($1::int[])`, [lost_ids]);
      await client.query(`DELETE FROM query_to_user WHERE qid = ANY($1::int[])`, [lost_ids]);
      await client.query(`
        DELETE FROM user_saved_data
        WHERE save_type = 'query'
          AND ars_pkey = ANY($1::uuid[])
      `, [lost_pks]);
      await client.query(`
        UPDATE user_saved_data
        SET data = jsonb_set(data, '{pks}', (
          SELECT COALESCE(jsonb_agg(project_pk ORDER BY position), '[]'::jsonb)
          FROM jsonb_array_elements(data->'pks') WITH ORDINALITY AS project_pks(project_pk, position)
          WHERE NOT ((project_pk #>> '{}') = ANY($1::text[]))
        ))
        WHERE save_type = 'project'
          AND data->'pks' ?| $1::text[]
      `, [lost_pks]);
      const res = await client.query(`DELETE FROM queries WHERE id = ANY($1::int[])`, [lost_ids]);
      return res.rowCount;
    });
  }

  async UNSAFE_batch_update(queries) {
    const sql_values = [];
    const params = [];
    let p = 1;
    for (let query of queries) {
      query.time_updated = new Date();
      sql_values.push(`($${p++}::UUID, $${p++}, $${p++}::TIMESTAMPTZ, $${p++}::JSONB)`);
      params.push(query.pk, query.status, query.time_updated, query.metadata);
    }
    const res = await pgExec(this.pool, `
      UPDATE queries AS q
      SET
        status = v.status,
        time_updated = v.time_updated,
        metadata = v.metadata
      FROM (
        VALUES ${sql_values.join(",\n")}
      ) AS v(pk, status, time_updated, metadata)
      WHERE q.pk = v.pk`, params);
    return res.rowCount;
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
