'use strict';

import { pg, pgExec } from '../lib/postgres_preamble.mjs';
import { Session } from  '../models/Session.mjs';
import { iSessionStore } from './iSessionStore.mjs';

export { SessionStorePostgres };
// export { getAuthDBClient, retrieveSessionByToken,  createNewSession, updateSession };

class SessionStorePostgres extends iSessionStore {

  constructor(pool, config=null) {
    super();
    // https://node-postgres.com/apis/pool#new-pool
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveSessionByToken(token) {
    let retval = null;
    const sql = `
      SELECT *
      FROM sessions
      WHERE token = $1
    `;
    let res = await pgExec(this.pool, sql, [token]);
    if (res.rows.length > 0) {
      retval = new Session(res.rows[0]);
    }
    return retval;
  }

  async createNewSession(session) {
    let retval = null;
    const sql = `
      INSERT INTO sessions
        (token, time_token_created, time_session_created,
        time_session_updated, linked_from, force_kill,
        user_id, data, auth_provider)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
      RETURNING *
    `;
    let res = await pgExec(this.pool, sql,
      [session.token, session.time_token_created, session.time_session_created,
      session.time_session_updated, session.linked_from, session.force_kill,
      session.user_id, session.data, session.auth_provider]);
    if (res.rows.length > 0) {
      retval = new Session(res.rows[0]);
    }
    return retval;
  }

  async updateSession(session) {
    let retval = null;
    const sql = `
      UPDATE sessions
      SET
        token = $1,
        time_token_created = $2,
        time_session_created = $3,
        time_session_updated = $4,
        linked_from = $5,
        force_kill = $6,
        user_id = $7,
        data = $8,
        auth_provider = $9
      WHERE id = $10
      RETURNING *
    `;
    let res = await pgExec(this.pool, sql,
      [session.token, session.time_token_created, session.time_session_created,
      session.time_session_updated, session.linked_from, session.force_kill,
      session.user_id, session.data, session.auth_provider, session.id]);
    if (res.rows.length > 0) {
      retval = new Session(res.rows[0]);
    }
    return retval;
  }

  async expireSessionByToken(token) {
    let retval = null;
    let sql = `UPDATE sessions SET force_kill = true WHERE token = $1 RETURNING *`;
    let res = await pgExec(this.pool, sql, [token]);
    if (res.rows.length > 0) {
      retval = new Session(res.rows[0]);
    }
    return retval;
  }

  async updateSessionTimeByToken(token, time=new Date()) {
    let retval = null;
    const sql = `
      UPDATE sessions
      SET
        time_session_updated = $1
      WHERE token = $2
      RETURNING *
    `;
    let res = await pgExec(this.pool, sql, [time, token]);
    if (res.rows.length > 0) {
      retval = new Session(res.rows[0]);
    }
    return retval;
  }
}
