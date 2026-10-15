export { UserEntityStorePostgres }

import { pgExec, pgExecTrans } from "#lib/postgres_preamble.mjs";
import { SQL_TYPES, ENTITY_KIND, models_to_params_and_args } from "#model/common.mjs";
import { UserNode, UserEdge, UserEntityRequestError } from "#model/UserEntity.mjs";
import { user_data_table } from "#store/common.mjs";

const _ENTITY_CLASS = Object.freeze({
  [ENTITY_KIND.NODE]: UserNode,
  [ENTITY_KIND.EDGE]: UserEdge
});

const _COLUMN_TYPES = Object.freeze({
  id: SQL_TYPES.BIGINT,
  user_id: SQL_TYPES.UUID,
  label: SQL_TYPES.TEXT,
  type: SQL_TYPES.TEXT,
  data: SQL_TYPES.JSONB,
  subject_ref: SQL_TYPES.TEXT,
  subject_user_node_id: SQL_TYPES.BIGINT,
  object_ref: SQL_TYPES.TEXT,
  object_user_node_id: SQL_TYPES.BIGINT
});

class UserEntityStorePostgres {
  constructor(db_pool) {
    this._db_pool = db_pool;
  }

  async get_user_nodes(user_id, include_deleted, ids = null) {
    return this._get_entities(ENTITY_KIND.NODE, user_id, include_deleted, ids);
  }

  async get_user_edges(user_id, include_deleted, ids = null) {
    return this._get_entities(ENTITY_KIND.EDGE, user_id, include_deleted, ids);
  }

  async create_user_nodes(user_nodes) {
    if (user_nodes.length === 0) return [];
    const columns = ["user_id", ...UserNode.create_fields()];
    const [params, args] = models_to_params_and_args(
      user_nodes, columns, columns.map((column) => _COLUMN_TYPES[column]));
    const res = await pgExec(this._db_pool, `
      INSERT INTO ${user_data_table(ENTITY_KIND.NODE)} (${columns.join(", ")})
      VALUES ${params}
      RETURNING *`, args);
    return res.rows;
  }

  async create_user_edges(user_edges) {
    if (user_edges.length === 0) return [];
    const columns = ["user_id", ...UserEdge.create_fields()];
    const [params, args] = models_to_params_and_args(
      user_edges, columns, columns.map((column) => _COLUMN_TYPES[column]));
    const input_alias = "input";
    const owned_endpoint = (end) => `(${input_alias}.${end}_user_node_id IS NULL OR EXISTS (
          SELECT 1 FROM ${user_data_table(ENTITY_KIND.NODE)} un
          WHERE un.id = ${input_alias}.${end}_user_node_id AND un.user_id = ${input_alias}.user_id AND un.time_deleted IS NULL))`;
    return pgExecTrans(this._db_pool, async (client) => {
      const { rows } = await client.query(`
        INSERT INTO ${user_data_table(ENTITY_KIND.EDGE)} (${columns.join(", ")})
        SELECT * FROM (VALUES ${params}) AS ${input_alias}(${columns.join(", ")})
        WHERE ${owned_endpoint("subject")}
          AND ${owned_endpoint("object")}
        RETURNING *`, args);
      if (rows.length !== user_edges.length) {
        throw new UserEntityRequestError("User edge endpoints must be active user nodes owned by the current user");
      }
      return rows;
    });
  }

  async update_user_nodes(user_id, updates) {
    return this._update_entities(ENTITY_KIND.NODE, user_id, updates);
  }

  async update_user_edges(user_id, updates) {
    return this._update_entities(ENTITY_KIND.EDGE, user_id, updates);
  }

  async trash_user_nodes(user_id, ids) {
    return this._set_entities_deleted(ENTITY_KIND.NODE, user_id, ids, true);
  }

  async trash_user_edges(user_id, ids) {
    return this._set_entities_deleted(ENTITY_KIND.EDGE, user_id, ids, true);
  }

  async restore_user_nodes(user_id, ids) {
    return this._set_entities_deleted(ENTITY_KIND.NODE, user_id, ids, false);
  }

  async restore_user_edges(user_id, ids) {
    return this._set_entities_deleted(ENTITY_KIND.EDGE, user_id, ids, false);
  }

  async _get_entities(kind, user_id, include_deleted, ids) {
    const table = user_data_table(kind);
    const sql_include_deleted = include_deleted ? "" : " AND time_deleted IS NULL";
    const sql_ids = ids === null ? "" : " AND id = ANY($2::bigint[])";
    const args = ids === null ? [user_id] : [user_id, ids];
    const res = await pgExec(this._db_pool, `
      SELECT * FROM ${table}
      WHERE user_id = $1${sql_include_deleted}${sql_ids}
      ORDER BY id`, args);
    return res.rows;
  }

  async _update_entities(kind, user_id, updates) {
    if (updates.length === 0) return [];
    const table = user_data_table(kind);
    const update_fields = _ENTITY_CLASS[kind].update_fields();
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
    const res = await pgExec(this._db_pool, `
      UPDATE ${table}
      SET ${update_fields.map((field) => `${field} = COALESCE(changed.${field}, ${table}.${field})`).join(", ")},
          time_updated = CURRENT_TIMESTAMP
      FROM (VALUES ${params}) AS changed(${columns.join(", ")})
      WHERE ${table}.id = changed.id
        AND ${table}.user_id = $${user_id_param}
        AND ${table}.time_deleted IS NULL
      RETURNING ${table}.*`, args);
    return res.rows;
  }

  async _set_entities_deleted(kind, user_id, ids, deleted) {
    if (ids.length === 0) return [];
    const table = user_data_table(kind);
    const sql_time_deleted = deleted ? "CURRENT_TIMESTAMP" : "NULL";
    const sql_current_state = deleted ? "time_deleted IS NULL" : "time_deleted IS NOT NULL";
    const res = await pgExec(this._db_pool, `
      UPDATE ${table}
      SET time_deleted = ${sql_time_deleted}, time_updated = CURRENT_TIMESTAMP
      WHERE id = ANY($1::bigint[]) AND user_id = $2 AND ${sql_current_state}
      RETURNING id`, [ids, user_id]);
    return res.rows.map((row) => row.id);
  }
}
