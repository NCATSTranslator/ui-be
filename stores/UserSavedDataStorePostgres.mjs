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

  async createUserSavedData(userSavedDataModel) {
    userSavedDataModel.time_updated = new Date();
    const res = await pgExec(this.pool, `
      INSERT INTO user_saved_data
        (user_id, save_type, label, notes, ars_pkey, object_ref,
        time_created, time_updated, data, deleted)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10)
      RETURNING *
    `, [userSavedDataModel.user_id, userSavedDataModel.save_type, userSavedDataModel.label,
        userSavedDataModel.notes, userSavedDataModel.ars_pkey, userSavedDataModel.object_ref,
        userSavedDataModel.time_created, userSavedDataModel.time_updated,
        userSavedDataModel.data, userSavedDataModel.deleted]);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async updateUserSavedDataBatch(userSavedDataModels) {
    const args = [];
    const params = [];
    const keyCount = Object.keys(userSavedDataModels[0]).length
    for (let i = 0; i < userSavedDataModels.length; i++) {
      const model = userSavedDataModels[i];
      model.time_updated = new Date();
      const template = [];
      for (let j = 0; j < keyCount; j++) {
        template.push(`$${i*keyCount+j+1}::${_SAVED_DATA_TYPES[j]}`);
      }
      args.push(`(${template.join(',')})`);
      params.push(
        model.id, model.user_id, model.save_type, model.label,
        model.notes, model.ars_pkey, model.object_ref, model.time_created,
        model.time_updated, model.data, model.deleted
      );
    }
    const res = await pgExec(this.pool, `
      UPDATE user_saved_data AS usd
      SET user_id = up.user_id, save_type = up.save_type, label = up.label, notes = up.notes,
          ars_pkey = up.ars_pkey, object_ref = up.object_ref, time_created = up.time_created,
          time_updated = up.time_updated, data = up.data, deleted = up.deleted
      FROM (
        VALUES ${args.join(',')}
      ) AS up(id, user_id, save_type, label,
              notes, ars_pkey, object_ref, time_created,
              time_updated, data, deleted)
      WHERE usd.id = up.id
      RETURNING *
    `, params);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async updateUserSavedData(userSavedDataModel) {
    userSavedDataModel.time_updated = new Date();
    const res = await pgExec(this.pool, `
      UPDATE user_saved_data SET
        user_id = $1, save_type = $2, label = $3, notes = $4,
        ars_pkey = $5, object_ref = $6, time_created = $7,
        time_updated = $8, data = $9, deleted = $10
      WHERE id = $11
      RETURNING *
    `, [userSavedDataModel.user_id, userSavedDataModel.save_type, userSavedDataModel.label,
        userSavedDataModel.notes, userSavedDataModel.ars_pkey, userSavedDataModel.object_ref,
        userSavedDataModel.time_created, userSavedDataModel.time_updated,
        userSavedDataModel.data, userSavedDataModel.deleted, userSavedDataModel.id]);
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

  async updateUserSavedDataPartial(userSavedDataModel, includeDeleted=false) {
    const withDel = includeDeleted ? '' : ' AND deleted = false ';
    if (!userSavedDataModel.id) {
      throw new Error("id must be provided for update");
    }
    userSavedDataModel.time_updated = new Date();
    let sql = `UPDATE user_saved_data SET `;
    let updates = [];
    let values = [];
    let count = 1;
    for (let field in userSavedDataModel) {
      if (userSavedDataModel.hasOwnProperty(field) && field !== 'id') {
        updates.push(`${field} = $${count}`);
        values.push(userSavedDataModel[field]);
        count++;
      }
    }

    values.push(userSavedDataModel.id);
    sql += updates.join(', ');
    sql += ` WHERE id = $${count} ${withDel} RETURNING *`;
    const res = await pgExec(this.pool, sql, values);
    return res.rows.length > 0 ? new UserSavedData(res.rows[0]) : null;
  }

  async deleteUserSavedDataById(id) {
    return this.updateUserSavedDataPartial({id: id, deleted: true});
  }
}

const _SAVED_DATA_TYPES = ['INT', 'UUID', 'TEXT', 'TEXT', 'TEXT', 'UUID', 'TEXT', 'DATE', 'DATE', 'JSONB', 'BOOLEAN'];
