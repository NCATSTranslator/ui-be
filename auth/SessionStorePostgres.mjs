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
    // https://node-postgres.com/apis/pool#new-pool
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

  async createNewSession(session_data) {
    console.log(`DBDB 01: ${JSON.stringify(session_data)}`);
    let res = null;
    let client = null;
    try {
      client = await this.pool.connect();
      res = await client.query('insert into sessions '
        + '(token, time_token_created, time_session_created, time_session_updated, '
        + ' linked_from, force_kill, user_id, data, auth_provider) '
        + 'values ($1, $2, $3, $4, $5, $6, $7, $8, $9) returning *',
        [session_data.token, session_data.time_token_created, session_data.time_session_created,
        session_data.time_session_updated, session_data.linked_from, session_data.force_kill,
        session_data.user_id, session_data.data, session_data.auth_provider
        ]);
      if (res.rows.length > 0) {
        res = new Session(res.rows[0]);
        console.log(`DBDB 2: ${JSON.stringify(res)}`);
      } else {
        console.log('it is actually null wtf');
        res = null;
      }
    } catch (err) {
      console.log(err);
    } finally {
      client.release();
      console.log(`DBDB 3: ${JSON.stringify(res)}`);
      return res;
    }
  }

  async updateSession(session_data) {
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
      + 'where id = $10 returning *',
        [session_data.token, session_data.time_token_created, session_data.time_session_created,
          session_data.time_session_updated, session_data.linked_from, session_data.force_kill,
          session_data.user_id, session_data.data, session_data.auth_provider, session_data.id
        ]);
      if (res.rows.length > 0) {
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
}
