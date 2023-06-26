'use strict';

import { pg } from '../postgres_preamble.mjs';
import { User } from '../models/User.mjs';

export { UserStorePostgres };

class UserStorePostgres {
  constructor(config) {
    this.pool = new pg.Pool(config);
  }

  async retrieveUserById(id) {
    let client = null;
    let retval = null;
    try {
      client = await this.pool.connect();
      const res = await client.query('SELECT * FROM users WHERE id = $1', [id]);
      if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    } catch (err) {
      console.log(err);
    } finally {
      if (client) {
        client.release();
      }
      return retval;
    }
  }

  async createNewUser(userData) {
    let client = null;
    let retval = null;
    try {
      client = await this.pool.connect();
      const res = await client.query(`
        INSERT INTO users (id, name, email, time_created, time_updated, profile_pic_url, data)
        VALUES ($1, $2, $3, $4, $5, $6, $7)
        RETURNING *
      `, [userData.id, userData.name, userData.email, userData.timeCreated,
          userData.timeUpdated, userData.profilePicUrl, userData.data]);
      if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    } catch (err) {
      console.log(err);
    } finally {
      if (client) {
        client.release();
      }
      return retval;
    }
  }

  async updateUser(userData) {
    let retval = null;
    let client = null;
    if (!userData.time_updated) {
      userData.updateUpdatedTime();
    }
    try {
      client = await this.pool.connect();
      const res = await client.query(`
        UPDATE users
        SET name = $2, email = $3, time_created = $4, time_updated = $5, profile_pic_url = $6, data = $7
        WHERE id = $1
        RETURNING *
      `, [userData.id, userData.name, userData.email, userData.time_created,
          userData.time_updated, userData.profile_pic_url, userData.data]);
      if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    } catch (err) {
      console.log(err);
    } finally {
      if (client) {
        client.release();
      }
      return retval;
    }
  }

  async updateUpdatedTime(id, datetime = new Date()) {
    let retval = null;
    let client = null;
    try {
      client = await this.pool.connect();
      const res = await client.query(`
        UPDATE users
        SET time_updated = $2
        WHERE id = $1
        RETURNING *
      `, [id, datetime]);
      if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    } catch (err) {
      console.log(err);
    } finally {
      if (client) {
        client.release();
      }
      return retval;
    }
  }
}
