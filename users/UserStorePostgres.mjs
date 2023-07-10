'use strict';

import { pg, pgExec } from '../postgres_preamble.mjs';
import { User } from '../models/User.mjs';

export { UserStorePostgres };

class UserStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveUserById(id) {
    let retval = null;
    const res = await pgExec(this.pool, 'SELECT * FROM users WHERE id = $1', [id]);
    if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    return retval;
  }

  async retrieveUserByEmail(email) {
    let retval = null;
    const res = await pgExec(this.pool, 'SELECT * FROM users WHERE email = $1', [email]);
    if (res.rows.length > 0) {
        retval = new User(res.rows[0]);
      }
    return retval;
  }

  async createNewUser(user) {
    let retval = null;
    const res = await pgExec(this.pool, `
      INSERT INTO users (id, name, email, time_created, time_updated, profile_pic_url, data, deleted)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
      RETURNING *
    `, [user.id, user.name, user.email, user.time_created,
        user.time_updated, user.profile_pic_url, user.data, user.deleted]);
    if (res.rows.length > 0) {
      retval = new User(res.rows[0]);
    }
    return retval;
  }

  async updateUser(user) {
    let retval = null;
    user.updateUpdatedTime();
    const res = await pgExec(this.pool, `
      UPDATE users
      SET name = $2, email = $3, time_created = $4, time_updated = $5, profile_pic_url = $6,
      data = $7, deleted = $8
      WHERE id = $1
      RETURNING *
    `, [user.id, user.name, user.email, user.time_created,
        user.time_updated, user.profile_pic_url, user.data, user.deleted]);
    if (res.rows.length > 0) {
      retval = new User(res.rows[0]);
    }
    return retval;
  }

  async updateUpdatedTime(id, datetime = new Date()) {
    let retval = null;
    const res = await pgExec(this.pool, `
      UPDATE users
      SET time_updated = $2
      WHERE id = $1
      RETURNING *
    `, [id, datetime]);
    if (res.rows.length > 0) {
      retval = new User(res.rows[0]);
    }
    return retval;
  }

  async deleteUser(user) {
    user.deleteUser();
    return this.updateUser(user);
  }
}
