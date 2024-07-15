import { pg, pgExec, pgExecTrans } from '../lib/postgres_preamble.mjs';
import { UserWorkspace } from '../models/UserWorkspace.mjs';

export { UserWorkspaceStorePostgres };

class UserWorkspaceStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveWorkspacesByUserId(uid, includeData=false, includeDeleted=false) {
    const withDel = includeDeleted ? '' : ' AND t0.deleted = false ';
    let sql;
    if (includeData) {
      sql = 'SELECT id, name, description, label, user_id, deleted, is_public, '
       + 'time_created, time_updated, data from user_workspaces t0 '
       + 'JOIN user_workspace_data t1 on t0.id = t1.ws_id';
    } else {
      sql = 'SELECT id, name, description, label, user_id, deleted, is_public, '
       + 'time_created, time_updated from user_workspaces t0';
    }
    sql = `${sql} WHERE t0.user_id = $1 ${withDel};`;
    const res = await pgExec(this.pool, sql, [uid]);
    const data = res.rows.map(row => new UserWorkspace(row));
    return data.length > 0 ? data : null;
  }

  async retrieveWorkspaceById(ws_id, includeDeleted=false) {
    const withDel = includeDeleted ? '' : ' AND t0.deleted = false ';
    let sql = 'SELECT id, name, description, label, user_id, deleted, is_public, '
       + 'time_created, time_updated, data from user_workspaces t0 '
       + 'JOIN user_workspace_data t1 on t0.id = t1.ws_id';
    sql = `${sql} WHERE id = $1 ${withDel};`;
    const res = await pgExec(this.pool, sql, [ws_id]);
    return res.rows.length > 0 ? new UserWorkspace(res.rows[0]) : null;
  }

  async createUserWorkspace(userWorkspace) {
    return pgExecTrans(this.pool, this.createUserWorkspaceAux, userWorkspace);
  }

  async createUserWorkspaceAux(client, userWorkspace) {
    userWorkspace.time_updated = new Date();
    const res01 = await client.query(`
      INSERT INTO user_workspaces
        (id, name, description, label, user_id, deleted,
        is_public, time_created, time_updated)
      VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
      RETURNING *
    `, [userWorkspace.id, userWorkspace.name, userWorkspace.description,
        userWorkspace.label, userWorkspace.user_id, userWorkspace.deleted,
        userWorkspace.is_public, userWorkspace.time_created,
        userWorkspace.time_updated]);
    const res02 = await client.query(`
      INSERT INTO user_workspace_data(ws_id, data) VALUES ($1, $2)
      RETURNING *`, [userWorkspace.id, userWorkspace.data]);
    if (!(res01.rows.length && res02.rows.length)) {
      return null;
    }
    return new UserWorkspace({...res01.rows[0], data: res02.rows[0].data});
  }

  async updateUserWorkspace(userWorkspace) {
    return pgExecTrans(this.pool, this.updateUserWorkspaceAux, userWorkspace);
  }

  async updateUserWorkspaceAux(client, userWorkspace) {
    userWorkspace.time_updated = new Date();
    const res01 = await client.query(`
      UPDATE user_workspaces SET
        name = $1, description = $2, label = $3, user_id = $4,
        deleted = $5, is_public = $6, time_created = $7,
        time_updated = $8
      WHERE id = $9
      RETURNING *
    `, [userWorkspace.name, userWorkspace.description, userWorkspace.label, userWorkspace.user_id,
        userWorkspace.deleted, userWorkspace.is_public, userWorkspace.time_created,
        userWorkspace.time_updated, userWorkspace.id
    ]);
    const res02 = await client.query(`
      UPDATE user_workspace_data SET data = $1 where ws_id = $2 RETURNING *`,
      [userWorkspace.data, userWorkspace.id]);
    if (!(res01.rows.length && res02.rows.length)) {
      return null;
    }
    //res01.rows[0].data = res02.rows[0].data;
    return new UserWorkspace({...res01.rows[0], data: res02.rows[0].data});
  }

  async deleteUserWorkspace(wsid) {
    const res = await pgExec(this.pool,
      'UPDATE user_workspaces set deleted = true, time_updated = $1 where id = $2',
      [new Date(), wsid]);
    return res.rowCount === 1;
  }

  async updateUserWorkspaceVisibility(wsid, is_public) {
    const res = await pgExec(this.pool,
      'UPDATE user_workspaces set is_public = $1, time_updated = $2 where id = $3',
      [is_public, new Date(), wsid]);
    return res.rowCount === 1;
  }
}

/*

// testing code
export { UserWorkspace };

export var uu = new UserWorkspaceStorePostgres(null, {
  host: "localhost",
  user: "app_data",
  port: "5432",
  database: "app_data",
  password: 'yourpassword',
  ssl: false
});

export var m1 = new UserWorkspace({user_id: 'de327795-0848-4ce6-bd44-908efe561c7c', name: 'my workspace'});
*/
