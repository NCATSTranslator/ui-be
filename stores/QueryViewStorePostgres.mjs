export { QueryViewStorePostgres }

import { pgExec } from "#lib/postgres_preamble.mjs";
import { QueryView, QueryViewRequestError } from "#model/QueryView.mjs";
import { WorkingView } from "#model/WorkingView.mjs";
import { FilterState } from "#model/FilterState.mjs";

const _TABLE = "query_view";
const _VIEW = "v";
const _WORKING_TABLE = "query_working_view";
const _WORKING = "w";
const _FOREIGN_KEY_VIOLATION = "23503";
const _UNIQUE_VIOLATION = "23505";

class QueryViewStorePostgres {
  constructor(db_pool) {
    this._db_pool = db_pool;
  }

  async get_query_views(user_id, query_pk, include_deleted, ids = null) {
    const sql_include_deleted = include_deleted ? "" : ` AND ${_VIEW}.time_deleted IS NULL`;
    const args = [user_id, query_pk];
    let sql_ids = "";
    if (ids !== null) {
      sql_ids = ` AND ${_VIEW}.id = ANY($3::bigint[])`;
      args.push(ids);
    }
    return this._select_query_views(`${_VIEW}.uid = $1 AND q.pk = $2${sql_include_deleted}${sql_ids}`, args);
  }

  async get_query_view(query_pk, id) {
    const [query_view] = await this._select_query_views(`${_VIEW}.id = $1 AND q.pk = $2`, [id, query_pk]);
    return query_view ?? null;
  }

  async create_query_view(user_id, query_pk, filter_state_id) {
    try {
      const res = await pgExec(this._db_pool, `
        INSERT INTO ${_TABLE} (uid, qid, filter_state_id)
        SELECT $1, uq.id, $3::bigint
        FROM (${_SQL_OWNED_QUERY}) uq
        ON CONFLICT (uid, qid, filter_state_id) WHERE time_deleted IS NULL DO UPDATE
        SET time_updated = CURRENT_TIMESTAMP
        RETURNING id`, [user_id, query_pk, filter_state_id]);
      if (res.rows.length === 0) return null;
      const [{ id }] = res.rows;
      const [query_view] = await this.get_query_views(user_id, query_pk, true, [id]);
      return query_view;
    } catch (err) {
      if (err.code === _FOREIGN_KEY_VIOLATION) {
        throw new QueryViewRequestError(`Unknown filter state id: ${filter_state_id}`);
      }
      throw err;
    }
  }

  async trash_query_views(user_id, query_pk, ids) {
    return this._set_query_views_deleted(user_id, query_pk, ids, true);
  }

  async restore_query_views(user_id, query_pk, ids) {
    return this._set_query_views_deleted(user_id, query_pk, ids, false);
  }

  async get_working_view(user_id, query_pk) {
    return this._select_working_view(`${_WORKING}.uid = $1 AND q.pk = $2`, [user_id, query_pk]);
  }

  async set_working_view(user_id, query_pk, version, state) {
    const res = await pgExec(this._db_pool, `
      INSERT INTO ${_WORKING_TABLE} (uid, qid, version, state)
      SELECT $1, uq.id, $3::integer, $4::jsonb
      FROM (${_SQL_OWNED_QUERY}) uq
      ON CONFLICT (uid, qid) DO UPDATE
      SET version = EXCLUDED.version, state = EXCLUDED.state, time_updated = CURRENT_TIMESTAMP
      RETURNING qid`, [user_id, query_pk, version, JSON.stringify(state)]);
    if (res.rows.length === 0) return null;
    const [{ qid }] = res.rows;
    return this._select_working_view(`${_WORKING}.uid = $1 AND ${_WORKING}.qid = $2`, [user_id, qid]);
  }

  async _select_query_views(sql_where, args) {
    const res = await pgExec(this._db_pool, `
      SELECT ${_VIEW}.id, ${_VIEW}.uid, ${_VIEW}.time_created, ${_VIEW}.time_updated, ${_VIEW}.time_deleted, q.pk,
             ${_SQL_FILTER_STATE_COLUMNS}
      FROM ${_TABLE} ${_VIEW}
      JOIN queries q ON q.id = ${_VIEW}.qid
      JOIN user_filter_state fs ON fs.id = ${_VIEW}.filter_state_id
      WHERE ${sql_where}
      ORDER BY ${_VIEW}.time_updated DESC, ${_VIEW}.id DESC`, args);
    return res.rows.map(_row_to_query_view);
  }

  async _select_working_view(sql_where, args) {
    const res = await pgExec(this._db_pool, `
      SELECT ${_WORKING}.uid, ${_WORKING}.version, ${_WORKING}.state, ${_WORKING}.time_updated, q.pk
      FROM ${_WORKING_TABLE} ${_WORKING}
      JOIN queries q ON q.id = ${_WORKING}.qid
      WHERE ${sql_where}
      ORDER BY q.id DESC
      LIMIT 1`, args);
    if (res.rows.length === 0) return null;
    return _row_to_working_view(res.rows[0]);
  }

  async _set_query_views_deleted(user_id, query_pk, ids, deleted) {
    if (ids.length === 0) return [];
    const sql_time_deleted = deleted ? "CURRENT_TIMESTAMP" : "NULL";
    const sql_current_state = deleted ? `${_VIEW}.time_deleted IS NULL` : `${_VIEW}.time_deleted IS NOT NULL`;
    const sql_no_active_duplicate = deleted ? "" : ` AND NOT EXISTS (
        SELECT 1 FROM ${_TABLE} active
        WHERE active.uid = ${_VIEW}.uid
          AND active.qid = ${_VIEW}.qid
          AND active.filter_state_id = ${_VIEW}.filter_state_id
          AND active.time_deleted IS NULL)`;
    try {
      const res = await pgExec(this._db_pool, `
        UPDATE ${_TABLE} ${_VIEW}
        SET time_deleted = ${sql_time_deleted}, time_updated = CURRENT_TIMESTAMP
        FROM queries q
        WHERE ${_VIEW}.id = ANY($1::bigint[]) AND ${_VIEW}.uid = $2 AND q.id = ${_VIEW}.qid AND q.pk = $3
          AND ${sql_current_state}${sql_no_active_duplicate}
        RETURNING ${_VIEW}.id`, [ids, user_id, query_pk]);
      return res.rows.map((row) => row.id);
    } catch (err) {
      if (err.code === _UNIQUE_VIOLATION) {
        throw new QueryViewRequestError("Query views with the same filter state cannot both be restored");
      }
      throw err;
    }
  }
}

const _SQL_OWNED_QUERY = `
          SELECT q.id FROM queries q
          JOIN query_to_user qu ON qu.qid = q.id AND qu.uid = $1
          WHERE q.pk = $2
          ORDER BY q.id DESC
          LIMIT 1`;

const _SQL_FILTER_STATE_COLUMNS = `fs.id AS filter_state_id, fs.label, fs.hidden, fs.version, fs.state,
             fs.time_created AS filter_state_time_created,
             fs.time_updated AS filter_state_time_updated,
             fs.time_deleted AS filter_state_time_deleted`;

function _row_to_query_view(row) {
  return new QueryView({
    id: row.id,
    user_id: row.uid,
    pk: row.pk,
    filter_state: _row_to_filter_state(row),
    time_created: row.time_created,
    time_updated: row.time_updated,
    time_deleted: row.time_deleted
  });
}

function _row_to_working_view(row) {
  return new WorkingView({
    user_id: row.uid,
    pk: row.pk,
    version: row.version,
    state: row.state,
    time_updated: row.time_updated
  });
}

function _row_to_filter_state(row) {
  return new FilterState({
    id: row.filter_state_id,
    user_id: row.uid,
    label: row.label,
    hidden: row.hidden,
    version: row.version,
    state: row.state,
    time_created: row.filter_state_time_created,
    time_updated: row.filter_state_time_updated,
    time_deleted: row.filter_state_time_deleted
  });
}
