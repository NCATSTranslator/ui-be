import { pg, pgExec } from '../lib/postgres_preamble.mjs';
import { UserSavedData } from '../models/UserSavedData.mjs'; // assuming UserSavedData is exported from this module

export { UserSavedDataStorePostgres };

class UserSavedDataStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveUserSavedDataByUserId(uid, includeDeleted=false, saveType=null) {
    const withDel = includeDeleted ? '' : ' AND deleted = false ';
    let sql = `SELECT * FROM user_saved_data WHERE user_id = $1 ${withDel}`;
    let args = [uid];
    if (saveType) {
      sql += ` AND save_type = $2`;
      args.push(saveType);
    }
    const res = await pgExec(this.pool, sql, args);
    const data = res.rows.map(row => new UserSavedData(row));
    return data.length > 0 ? data : null;
  }

  async createUserSavedData(userSavedData) {
    userSavedData.time_updated = new Date();
    const res = await pgExec(this.pool, `
      INSERT INTO user_saved_data
        (user_id, save_type, label, notes, ars_pkey, object_ref,
        time_created, time_updated, data, deleted)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
      RETURNING *
    `, [userSavedData.user_id, userSavedData.save_type, userSavedData.label,
        userSavedData.notes, userSavedData.ars_pkey, userSavedData.object_ref,
        userSavedData.time_created, userSavedData.time_updated,
        userSavedData.data, userSavedData.deleted]);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async updateUserSavedData(userSavedData) {
    userSavedData.time_updated = new Date();
    const res = await pgExec(this.pool, `
      UPDATE user_saved_data SET
        user_id = $1, save_type = $2, label = $3, notes = $4,
        ars_pkey = $5, object_ref = $6, time_created = $7,
        time_updated = $8, data = $9, deleted = $10
      WHERE id = $11
      RETURNING *
    `, [userSavedData.user_id, userSavedData.save_type, userSavedData.label,
        userSavedData.notes, userSavedData.ars_pkey, userSavedData.object_ref,
        userSavedData.time_created, userSavedData.time_updated,
        userSavedData.data, userSavedData.deleted, userSavedData.id]);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async retrieveUserSavedDataBy(uid, fields, includeDeleted=false) {
    const withDel = includeDeleted ? '' : ' AND deleted = false ';
    let sql = `
      SELECT * FROM user_saved_data
      WHERE user_id = $1 ${withDel} AND `;
    let values = [uid];
    let conditions = [];
    for (let field in fields) {
      conditions.push(`${field} = $${values.length + 1}`);
      values.push(fields[field]);
    }
    sql += conditions.join(' AND ');
    const res = await pgExec(this.pool, sql, values);
    const data = res.rows.map(row => new UserSavedData(row));
    return data.length > 0 ? data : null;
  }

  async updateUserSavedDataPartial(userSavedData, includeDeleted=false) {
    const withDel = includeDeleted ? '' : ' AND deleted = false ';
    if (!userSavedData.id) {
      throw new Error("id must be provided for update");
    }
    userSavedData.time_updated = new Date();
    let sql = `UPDATE user_saved_data SET `;
    let updates = [];
    let values = [];
    let count = 1;
    for (let field in userSavedData) {
      if (userSavedData.hasOwnProperty(field) && field !== 'id') {
        updates.push(`${field} = $${count}`);
        values.push(userSavedData[field]);
        count++;
      }
    }

    values.push(userSavedData.id);
    sql += updates.join(', ');
    sql += ` WHERE id = $${count} ${withDel} RETURNING *`;
    const res = await pgExec(this.pool, sql, values);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async deleteUserSavedDataById(id) {
    return this.updateUserSavedDataPartial({id: id, deleted: true});
  }
}
