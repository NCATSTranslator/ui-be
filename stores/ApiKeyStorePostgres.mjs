'use strict';

export { ApiKeyStorePostgres };

import { pg, pgExec, pgExecTrans } from '../lib/postgres_preamble.mjs';
import { ApiKey } from '../models/ApiKey.mjs';

const API_KEYS_LOCK_NAMESPACE = 1788356959;

class ApiKeyStorePostgres {

  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async create_api_key(api_key, max_active, now=new Date()) {
    return pgExecTrans(this.pool, async (client) => {
      await client.query('SELECT pg_advisory_xact_lock($1, hashtext($2))',
        [API_KEYS_LOCK_NAMESPACE, api_key.user_id]);
      const count = await client.query(`
        SELECT COUNT(*)::int AS active
        FROM api_keys
        WHERE user_id = $1 AND time_revoked IS NULL AND time_expires > $2
      `, [api_key.user_id, now]);
      if (count.rows[0].active >= max_active) return null;
      const res = await client.query(`
        INSERT INTO api_keys
          (id, user_id, name, key_hash, key_display,
          time_created, time_last_used, time_revoked, time_expires)
        VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
        RETURNING *
      `, [api_key.id, api_key.user_id, api_key.name, api_key.key_hash, api_key.key_display,
        api_key.time_created, api_key.time_last_used, api_key.time_revoked, api_key.time_expires]);
      return new ApiKey(res.rows[0]);
    });
  }

  async retrieve_api_key_by_hash(key_hash) {
    const sql = `
      SELECT *
      FROM api_keys
      WHERE key_hash = $1
    `;
    const res = await pgExec(this.pool, sql, [key_hash]);
    if (res.rows.length <= 0) return null;
    return new ApiKey(res.rows[0]);
  }

  async retrieve_api_keys_by_user_id(user_id, include_revoked=false, include_expired=false, now=new Date()) {
    const params = [user_id];
    const with_revoked = include_revoked ? '' : ' AND time_revoked IS NULL ';
    let with_expired = '';
    if (!include_expired) {
      params.push(now);
      with_expired = ` AND time_expires > $${params.length} `;
    }
    const sql = `
      SELECT *
      FROM api_keys
      WHERE user_id = $1 ${with_revoked} ${with_expired}
      ORDER BY time_created DESC
    `;
    const res = await pgExec(this.pool, sql, params);
    return res.rows.map((row) => new ApiKey(row));
  }

  async revoke_api_key_by_id(id, user_id, time=new Date()) {
    const sql = `
      UPDATE api_keys
      SET time_revoked = $3
      WHERE id = $1 AND user_id = $2 AND time_revoked IS NULL
      RETURNING *
    `;
    const res = await pgExec(this.pool, sql, [id, user_id, time]);
    if (res.rows.length <= 0) return null;
    return new ApiKey(res.rows[0]);
  }

  async update_last_used_by_id(id, user_id, time=new Date()) {
    const sql = `
      UPDATE api_keys
      SET time_last_used = $3
      WHERE id = $1 AND user_id = $2
    `;
    const res = await pgExec(this.pool, sql, [id, user_id, time]);
    return res.rowCount > 0;
  }
}
