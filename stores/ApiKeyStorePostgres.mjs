'use strict';

import { pg, pgExec } from '../lib/postgres_preamble.mjs';
import { ApiKey } from '../models/ApiKey.mjs';

export { ApiKeyStorePostgres };

class ApiKeyStorePostgres {

  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async create_api_key(api_key) {
    let retval = null;
    const sql = `
      INSERT INTO api_keys
        (id, user_id, name, key_hash, key_display,
        time_created, time_last_used, time_revoked, time_expires)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
      RETURNING *
    `;
    const res = await pgExec(this.pool, sql,
      [api_key.id, api_key.user_id, api_key.name, api_key.key_hash, api_key.key_display,
      api_key.time_created, api_key.time_last_used, api_key.time_revoked, api_key.time_expires]);
    if (res.rows.length > 0) {
      retval = new ApiKey(res.rows[0]);
    }
    return retval;
  }

  async retrieve_api_key_by_hash(key_hash) {
    let retval = null;
    const sql = `
      SELECT *
      FROM api_keys
      WHERE key_hash = $1
    `;
    const res = await pgExec(this.pool, sql, [key_hash]);
    if (res.rows.length > 0) {
      retval = new ApiKey(res.rows[0]);
    }
    return retval;
  }

  async retrieve_api_keys_by_user_id(user_id, include_revoked=false) {
    const with_revoked = include_revoked ? '' : ' AND time_revoked IS NULL ';
    const sql = `
      SELECT *
      FROM api_keys
      WHERE user_id = $1 ${with_revoked}
      ORDER BY time_created DESC
    `;
    const res = await pgExec(this.pool, sql, [user_id]);
    return res.rows.map((row) => new ApiKey(row));
  }

  async revoke_api_key_by_id(id, user_id, time=new Date()) {
    let retval = null;
    const sql = `
      UPDATE api_keys
      SET time_revoked = $3
      WHERE id = $1 AND user_id = $2 AND time_revoked IS NULL
      RETURNING *
    `;
    const res = await pgExec(this.pool, sql, [id, user_id, time]);
    if (res.rows.length > 0) {
      retval = new ApiKey(res.rows[0]);
    }
    return retval;
  }

  async update_last_used_by_id(id, time=new Date()) {
    let retval = null;
    const sql = `
      UPDATE api_keys
      SET time_last_used = $2
      WHERE id = $1
      RETURNING *
    `;
    const res = await pgExec(this.pool, sql, [id, time]);
    if (res.rows.length > 0) {
      retval = new ApiKey(res.rows[0]);
    }
    return retval;
  }
}
