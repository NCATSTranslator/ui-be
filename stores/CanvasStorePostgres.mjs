export { CanvasStorePostgres }

import { pgExec } from "#lib/postgres_preamble.mjs";

class CanvasStorePostgres {
  constructor(db_pool) {
    this._db_pool = db_pool;
  }

  async get_canvases_by_user(user_id, include_deleted) {
    const sql_include_deleted = include_deleted ? "" : " AND time_deleted IS NULL";
    const res = await pgExec(this._db_pool, `
      SELECT user_to_canvas.user_id, canvas.id,
             canvas.label, canvas.layout, canvas.data,
             canvas.time_created, canvas.time_updated, canvas.time_deleted
      FROM user_to_canvas
      JOIN canvas ON user_to_canvas.canvas_id = canvas.id
      WHERE user_to_canvas.user_id = $1${sql_include_deleted}`, [user_id]);
    return res.rows;
  }

  async create_user_canvas(user_canvas) {
    const client = await this._db_pool.connect();
    try {
      await client.query("BEGIN");
      const canvas_id = await this._create_canvas(client, user_canvas);
      await this._create_user_to_canvas(client, user_canvas.user_id, canvas_id);
      await client.query("COMMIT");
      return canvas_id;
    } catch (err) {
      await client.query("ROLLBACK");
      throw err;
    } finally {
      client.release();
    }
  }

  async _create_canvas(client, user_canvas) {
    const res = await client.query(`
      INSERT INTO canvas(label, layout, data)
      VALUES($1, $2, $3)
      RETURNING id`,
      [user_canvas.label, user_canvas.layout, user_canvas.data]);
    return res.rows[0].id;
  }

  async _create_user_to_canvas(client, user_id, canvas_id) {
    await client.query(`
      INSERT INTO user_to_canvas(user_id, canvas_id)
      VALUES($1, $2)`, [user_id, canvas_id]);
  }
}
