'use strict';

import { pg } from '../postgres_preamble.mjs';
import { Session } from  '../models/Session.mjs';
import { SessionAnalytics } from '../models/SessionAnalytics.mjs';
import { iSessionStore } from './iSessionStore.mjs';

export { SessionStorePostgres };
// export { getAuthDBClient, retrieveSessionByToken,  createNewSession, updateSession };

class SessionStorePostgres extends iSessionStore {

  constructor(config) {
    super();
    this.pool = new pg.Pool(config);
  }

  async retrieveSessionByToken(token) {
    let res = null;
    let client = null;
    try {
      client = await this.pool.connect();
      res = await client.query('select * from sessions where token = $1', [token]);
      if (res.rowCount > 0) {
        res = new Session(res.rows[0]);
      } else {
        res = null;
      }
    } catch (err) {
      console.log(err);
    } finally {
      client.release();
      return res;
    }
  }

  async createNewSession(sessionData) {
    let res = null;
    let client = null;
    try {
      client = await this.pool.connect();
      res = await client.query('insert into sessions '
        + '(token, time_token_created, time_session_created, time_session_updated, '
        + ' linked_from, force_kill, user_id, data, auth_provider) '
        + 'values ($1, $2, $3, $4, $5, $6, $7, $8, $9) returning id',
        [sessionData.token, sessionData.time_token_created, sessionData.time_session_created,
        sessionData.time_session_updated, sessionData.linked_from, sessionData.force_kill,
        sessionData.user_id, sessionData.data, sessionData.auth_provider
        ]);
      if (res.rows.length > 0) {
        res = res.rows[0];
      } else {
        res = null;
      }
    } catch (err) {
      console.log(err);
    } finally {
      client.release();
      return res;
    }
  }

  async updateSession(sessionData) {
    let res = null;
    let client = null;
    try {
      client = await this.pool.connect();
      res = await client.query('update sessions set '
      + 'token =  $1, '
      + 'time_token_created = $2, '
      + 'time_session_created = $3, '
      + 'time_session_updated = $4, '
      + 'linked_from = $5, '
      + 'force_kill = $6, '
      + 'user_id = $7, '
      + 'data = $8, '
      + 'auth_provider = $9 '
      + 'where id = $10',
        [sessionData.token, sessionData.time_token_created, sessionData.time_session_created,
          sessionData.time_session_updated, sessionData.linked_from, sessionData.force_kill,
          sessionData.user_id, sessionData.data, sessionData.auth_provider, sessionData.id
        ]);
      if (res.rowCount > 0) {
        res = true;
      } else {
        res = false;
      }
    } catch (err) {
      console.log(err);
    } finally {
      client.release();
      return res;
    }
  }
}
