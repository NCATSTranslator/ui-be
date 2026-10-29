export { FilterStateStorePostgres }

import { pgExec } from "#lib/postgres_preamble.mjs";
import { SQL_TYPES, models_to_params_and_args } from "#model/common.mjs";
import { FilterState, FilterStateRequestError } from "#model/FilterState.mjs";

const _TABLE = "user_filter_state";
const _CHECK_VIOLATION = "23514";

const _COLUMN_TYPES = Object.freeze({
  id: SQL_TYPES.BIGINT,
  user_id: SQL_TYPES.UUID,
  label: SQL_TYPES.TEXT,
  hidden: SQL_TYPES.BOOL,
  version: SQL_TYPES.INT,
  state: SQL_TYPES.JSONB
});

class FilterStateStorePostgres {
  constructor(db_pool) {
    this._db_pool = db_pool;
  }

  async get_filter_states(user_id, include_deleted, include_hidden, ids = null) {
    const sql_include_deleted = include_deleted ? "" : " AND time_deleted IS NULL";
    const sql_include_hidden = include_hidden ? "" : " AND NOT hidden";
    const args = [user_id];
    let sql_ids = "";
    if (ids !== null) {
      sql_ids = " AND id = ANY($2::bigint[])";
      args.push(ids);
    }
    const res = await pgExec(this._db_pool, `
      SELECT * FROM ${_TABLE}
      WHERE user_id = $1${sql_include_deleted}${sql_include_hidden}${sql_ids}
      ORDER BY id`, args);
    return res.rows;
  }

  async create_filter_states(filter_states) {
    if (filter_states.length === 0) return [];
    const columns = ["user_id", ...FilterState.create_fields()];
    const [params, args] = models_to_params_and_args(
      filter_states.map((filter_state) => ({ ...filter_state, state: JSON.stringify(filter_state.state) })),
      columns,
      columns.map((column) => _COLUMN_TYPES[column]));
    const res = await pgExec(this._db_pool, `
      INSERT INTO ${_TABLE} (${columns.join(", ")})
      VALUES ${params}
      ON CONFLICT (user_id, version, state_hash) DO UPDATE
      SET label = CASE
            WHEN EXCLUDED.hidden THEN ${_TABLE}.label
            WHEN ${_TABLE}.hidden OR ${_TABLE}.time_deleted IS NOT NULL THEN EXCLUDED.label
            ELSE ${_TABLE}.label END,
          hidden = ${_TABLE}.hidden AND EXCLUDED.hidden,
          time_deleted = CASE WHEN EXCLUDED.hidden THEN ${_TABLE}.time_deleted ELSE NULL END,
          time_updated = CASE WHEN EXCLUDED.hidden THEN ${_TABLE}.time_updated ELSE CURRENT_TIMESTAMP END
      RETURNING *`, args);
    return res.rows;
  }

  async update_filter_states(user_id, updates) {
    if (updates.length === 0) return [];
    const update_fields = FilterState.update_fields();
    const columns = ["id", ...update_fields];
    const [params, args] = models_to_params_and_args(
      updates.map(({ id, fields }) => ({
        id: id,
        ...Object.fromEntries(update_fields.map((field) => [field, fields[field] ?? null]))
      })),
      columns,
      columns.map((column) => _COLUMN_TYPES[column]));
    const user_id_param = args.length + 1;
    args.push(user_id);
    try {
      const res = await pgExec(this._db_pool, `
        UPDATE ${_TABLE}
        SET ${update_fields.map((field) => `${field} = COALESCE(changed.${field}, ${_TABLE}.${field})`).join(", ")},
            time_updated = CURRENT_TIMESTAMP
        FROM (VALUES ${params}) AS changed(${columns.join(", ")})
        WHERE ${_TABLE}.id = changed.id
          AND ${_TABLE}.user_id = $${user_id_param}
          AND ${_TABLE}.time_deleted IS NULL
        RETURNING ${_TABLE}.*`, args);
      return res.rows;
    } catch (err) {
      if (err.code === _CHECK_VIOLATION) {
        throw new FilterStateRequestError("A filter state must have a label to be visible");
      }
      throw err;
    }
  }

  async trash_filter_states(user_id, ids) {
    return this._set_filter_states_deleted(user_id, ids, true);
  }

  async restore_filter_states(user_id, ids) {
    return this._set_filter_states_deleted(user_id, ids, false);
  }

  async _set_filter_states_deleted(user_id, ids, deleted) {
    if (ids.length === 0) return [];
    const sql_time_deleted = deleted ? "CURRENT_TIMESTAMP" : "NULL";
    const sql_current_state = deleted ? "time_deleted IS NULL" : "time_deleted IS NOT NULL";
    const res = await pgExec(this._db_pool, `
      UPDATE ${_TABLE}
      SET time_deleted = ${sql_time_deleted}, time_updated = CURRENT_TIMESTAMP
      WHERE id = ANY($1::bigint[]) AND user_id = $2 AND label IS NOT NULL AND ${sql_current_state}
      RETURNING id`, [ids, user_id]);
    return res.rows.map((row) => row.id);
  }
}
