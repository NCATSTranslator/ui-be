export { CanvasStorePostgres }

import { pgExec, pgExecTrans } from "#lib/postgres_preamble.mjs";
import { SQL_TYPES, ENTITY_KIND, models_to_params_and_args } from "#model/common.mjs";
import { Graph, CanvasRequestError, CanvasConflictError } from "#model/Canvas.mjs";
import { canvas_table, user_data_table, translator_data_table, rows_from_json } from "#store/common.mjs";

const _WRITE_COLUMNS = Object.freeze({
  [ENTITY_KIND.NODE]: Object.freeze({
    insert: Object.freeze(["canvas_id", "data_id", "user_data_id", "ref", "label", "type", "x", "y", "hidden", "tags"]),
    revive: Object.freeze(["label", "type", "x", "y", "hidden", "tags"])
  }),
  [ENTITY_KIND.EDGE]: Object.freeze({
    insert: Object.freeze([
      "canvas_id", "data_id", "user_data_id", "subject_id", "object_id", "ref", "label", "hidden", "tags"
    ]),
    revive: Object.freeze(["subject_id", "object_id", "label", "hidden", "tags"])
  })
});

const _PG_UNIQUE_VIOLATION = "23505";
const _PG_CHECK_VIOLATION = "23514";

const _COLUMN_TYPES = Object.freeze({
  id: SQL_TYPES.BIGINT,
  canvas_id: SQL_TYPES.BIGINT,
  data_id: SQL_TYPES.BIGINT,
  user_data_id: SQL_TYPES.BIGINT,
  subject_id: SQL_TYPES.BIGINT,
  object_id: SQL_TYPES.BIGINT,
  ref: SQL_TYPES.TEXT,
  label: SQL_TYPES.TEXT,
  type: SQL_TYPES.TEXT,
  x: SQL_TYPES.DOUBLE,
  y: SQL_TYPES.DOUBLE,
  hidden: SQL_TYPES.BOOL,
  tags: SQL_TYPES.JSONB
});

function _user_edge_connects_sql(user_edge, canvas_edge, subject_node, object_node) {
  return `(${user_edge}.subject_ref = ${subject_node}.ref
        OR (${canvas_edge}.data_id IS NULL AND ${user_edge}.subject_user_node_id = ${subject_node}.user_data_id))
      AND (${user_edge}.object_ref = ${object_node}.ref
        OR (${canvas_edge}.data_id IS NULL AND ${user_edge}.object_user_node_id = ${object_node}.user_data_id))`;
}

function _user_data_guard_sql(kind, user_data_param, user_id_param) {
  const target = canvas_table(kind);
  const owned = (alias) =>
    `${alias}.id = $${user_data_param} AND ${alias}.user_id = $${user_id_param} AND ${alias}.time_deleted IS NULL`;
  if (kind === ENTITY_KIND.NODE) {
    return `
        AND ($${user_data_param}::bigint IS NULL
          OR EXISTS (SELECT 1 FROM ${user_data_table(ENTITY_KIND.NODE)} un WHERE ${owned("un")}))
        AND (${target}.user_data_id IS NOT DISTINCT FROM $${user_data_param}::bigint
          OR NOT EXISTS (
            SELECT 1 FROM ${canvas_table(ENTITY_KIND.EDGE)} ce
            JOIN ${user_data_table(ENTITY_KIND.EDGE)} ue ON ue.id = ce.user_data_id
            WHERE ce.canvas_id = ${target}.canvas_id
              AND ce.time_deleted IS NULL
              AND ((ce.subject_id = ${target}.id AND ue.subject_user_node_id = ${target}.user_data_id)
                OR (ce.object_id = ${target}.id AND ue.object_user_node_id = ${target}.user_data_id))))`;
  }
  return `
        AND ($${user_data_param}::bigint IS NULL
          OR EXISTS (
            SELECT 1 FROM ${user_data_table(ENTITY_KIND.EDGE)} ue
            JOIN ${canvas_table(ENTITY_KIND.NODE)} s ON s.id = ${target}.subject_id
            JOIN ${canvas_table(ENTITY_KIND.NODE)} o ON o.id = ${target}.object_id
            WHERE ${owned("ue")}
              AND ${_user_edge_connects_sql("ue", target, "s", "o")}))`;
}

function _as_conflict(err) {
  if (err.code !== _PG_UNIQUE_VIOLATION && err.code !== _PG_CHECK_VIOLATION) return err;
  return new CanvasConflictError(`Canvas change conflicts with the canvas's existing user data (${err.constraint})`);
}

function _split_by_translator_data(graph_entities) {
  return [
    graph_entities.filter((entity) => entity.has_translator_data()),
    graph_entities.filter((entity) => !entity.has_translator_data())
  ];
}

function _user_edge_endpoints(user_edge) {
  const endpoint = (end) => user_edge[`${end}_ref`] !== null
    ? { ref: user_edge[`${end}_ref`] }
    : { user_node_id: user_edge[`${end}_user_node_id`] };
  return [endpoint("subject"), endpoint("object")];
}

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

  async get_canvas_graph_by_user(user_id, canvas_id, include_deleted) {
    const sql_entities = include_deleted ? "" : " AND entity.time_deleted IS NULL";
    const sql_canvas = include_deleted ? "" : " AND canvas.time_deleted IS NULL";
    const res = await pgExec(this._db_pool, `
      SELECT canvas.data,
        (SELECT COALESCE(json_agg(entity ORDER BY entity.id), '[]'::json)
         FROM ${canvas_table(ENTITY_KIND.NODE)} entity WHERE entity.canvas_id = canvas.id${sql_entities}) AS nodes,
        (SELECT COALESCE(json_agg(entity ORDER BY entity.id), '[]'::json)
         FROM ${canvas_table(ENTITY_KIND.EDGE)} entity WHERE entity.canvas_id = canvas.id${sql_entities}) AS edges,
        (SELECT COALESCE(json_agg(entity ORDER BY entity.id), '[]'::json)
         FROM canvas_annotation entity WHERE entity.canvas_id = canvas.id${sql_entities}) AS annotations
      FROM user_to_canvas
      JOIN canvas ON user_to_canvas.canvas_id = canvas.id
      WHERE user_to_canvas.user_id = $1 AND canvas.id = $2${sql_canvas}`,
      [user_id, canvas_id]);
    if (res.rows.length === 0) return null;
    const row = res.rows[0];
    return {
      nodes: rows_from_json(row.nodes),
      edges: rows_from_json(row.edges),
      annotations: rows_from_json(row.annotations),
      tags: row.data?.tags ?? null
    };
  }

  async _query(client, sql, args) {
    return client ? client.query(sql, args) : pgExec(this._db_pool, sql, args);
  }

  async get_node_data(user_id, canvas_id, id) {
    const res = await pgExec(this._db_pool, `
      SELECT node.data
      FROM canvas_node
      JOIN node ON node.id = canvas_node.data_id
      JOIN user_to_canvas ON user_to_canvas.canvas_id = canvas_node.canvas_id
      JOIN canvas ON canvas.id = canvas_node.canvas_id
      WHERE canvas_node.canvas_id = $1
        AND canvas_node.id = $2
        AND user_to_canvas.user_id = $3
        AND canvas.time_deleted IS NULL`, [canvas_id, id, user_id]);
    if (res.rows.length === 0) return null;
    return res.rows[0].data;
  }

  async get_edge_data(user_id, canvas_id, id) {
    const res = await pgExec(this._db_pool, `
      SELECT edge.data
      FROM canvas_edge
      JOIN edge ON edge.id = canvas_edge.data_id
      JOIN user_to_canvas ON user_to_canvas.canvas_id = canvas_edge.canvas_id
      JOIN canvas ON canvas.id = canvas_edge.canvas_id
      WHERE canvas_edge.canvas_id = $1
        AND canvas_edge.id = $2
        AND user_to_canvas.user_id = $3
        AND canvas.time_deleted IS NULL`, [canvas_id, id, user_id]);
    if (res.rows.length === 0) return null;
    return res.rows[0].data;
  }

  async update_canvas_by_user(user_id, canvas_id, fields) {
    const set_clauses = [];
    const values = [];
    let i = 1;
    for (const col of Object.keys(fields)) {
      set_clauses.push(`${col} = $${i}`);
      values.push(fields[col]);
      i += 1;
    }
    set_clauses.push("time_updated = CURRENT_TIMESTAMP");
    const canvas_id_param = i;
    values.push(canvas_id);
    i += 1;
    const user_id_param = i;
    values.push(user_id);
    const res = await pgExec(this._db_pool, `
      UPDATE canvas
      SET ${set_clauses.join(", ")}
      WHERE canvas.id = $${canvas_id_param}
        AND canvas.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          WHERE user_to_canvas.canvas_id = canvas.id
            AND user_to_canvas.user_id = $${user_id_param})
      RETURNING *`, values);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async update_canvas_node_by_user(user_id, canvas_id, id, fields) {
    return this._update_canvas_entity_by_user(ENTITY_KIND.NODE, user_id, canvas_id, id, fields);
  }

  async update_canvas_edge_by_user(user_id, canvas_id, id, fields) {
    return this._update_canvas_entity_by_user(ENTITY_KIND.EDGE, user_id, canvas_id, id, fields);
  }

  async _update_canvas_entity_by_user(kind, user_id, canvas_id, id, fields) {
    if (fields.user_data_id === undefined) return this._update_canvas_entity(null, kind, user_id, canvas_id, id, fields);
    return this._write_trans(async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return null;
      return this._update_canvas_entity(client, kind, user_id, canvas_id, id, fields);
    });
  }

  async _update_canvas_entity(client, kind, user_id, canvas_id, id, fields) {
    const table = canvas_table(kind);
    const [set_clause, values] = this._build_element_update(fields, canvas_id, id, user_id);
    const sql_user_data = fields.user_data_id === undefined
      ? ""
      : _user_data_guard_sql(kind, Object.keys(fields).indexOf("user_data_id") + 1, values.user_id_param);
    const res = await this._query(client, `
      UPDATE ${table}
      SET ${set_clause}
      WHERE ${table}.canvas_id = $${values.canvas_id_param}
        AND ${table}.id = $${values.id_param}
        AND ${table}.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          JOIN canvas ON user_to_canvas.canvas_id = canvas.id
          WHERE user_to_canvas.canvas_id = ${table}.canvas_id
            AND user_to_canvas.user_id = $${values.user_id_param}
            AND canvas.time_deleted IS NULL)${sql_user_data}
      RETURNING *`, values.args);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async _write_trans(fun) {
    try {
      return await pgExecTrans(this._db_pool, fun);
    } catch (err) {
      throw _as_conflict(err);
    }
  }

  _build_element_update(fields, canvas_id, id, user_id) {
    const set_clauses = [];
    const args = [];
    let i = 1;
    for (const col of Object.keys(fields)) {
      set_clauses.push(`${col} = $${i}`);
      args.push(fields[col]);
      i += 1;
    }
    set_clauses.push("time_updated = CURRENT_TIMESTAMP");
    const canvas_id_param = i;
    args.push(canvas_id);
    i += 1;
    const id_param = i;
    args.push(id);
    i += 1;
    const user_id_param = i;
    args.push(user_id);
    return [set_clauses.join(", "), { args, canvas_id_param, id_param, user_id_param }];
  }

  async create_canvas_annotation(user_id, canvas_id, annotation) {
    const res = await pgExec(this._db_pool, `
      INSERT INTO canvas_annotation (canvas_id, content, x, y, width, height)
      SELECT $1, $2, $3, $4, $5, $6
      WHERE EXISTS (
        SELECT 1 FROM user_to_canvas
        JOIN canvas ON user_to_canvas.canvas_id = canvas.id
        WHERE canvas.id = $1
          AND user_to_canvas.user_id = $7
          AND canvas.time_deleted IS NULL)
      RETURNING *`,
      [canvas_id, annotation.content, annotation.x, annotation.y,
       annotation.width, annotation.height, user_id]);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async update_canvas_annotation_content_by_user(user_id, canvas_id, annotation_id, content) {
    const res = await pgExec(this._db_pool, `
      UPDATE canvas_annotation
      SET content = $1, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_annotation.canvas_id = $2
        AND canvas_annotation.id = $3
        AND canvas_annotation.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          JOIN canvas ON user_to_canvas.canvas_id = canvas.id
          WHERE user_to_canvas.canvas_id = canvas_annotation.canvas_id
            AND user_to_canvas.user_id = $4
            AND canvas.time_deleted IS NULL)
      RETURNING *`, [content, canvas_id, annotation_id, user_id]);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async _set_canvas_annotation_geometry(client, canvas_id, geometries) {
    if (geometries.length === 0) return [];
    const [params, args] = models_to_params_and_args(
      geometries,
      ["id", "x", "y", "width", "height"],
      [SQL_TYPES.BIGINT, SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE]);
    const canvas_id_param = args.length + 1;
    args.push(canvas_id);
    const res = await client.query(`
      UPDATE canvas_annotation AS ca
      SET x = v.x,
          y = v.y,
          width = COALESCE(v.width, ca.width),
          height = COALESCE(v.height, ca.height),
          time_updated = CURRENT_TIMESTAMP
      FROM (VALUES ${params}) AS v(id, x, y, width, height)
      WHERE ca.canvas_id = $${canvas_id_param}
        AND ca.id = v.id
        AND ca.time_deleted IS NULL
      RETURNING ca.id, ca.canvas_id, ca.content, ca.x, ca.y, ca.width, ca.height,
                ca.time_created, ca.time_updated, ca.time_deleted`, args);
    return res.rows;
  }

  async trash_canvases_by_user(user_id, canvas_ids) {
    if (canvas_ids.length === 0) return [];
    const res = await pgExec(this._db_pool, `
      UPDATE canvas
      SET time_deleted = CURRENT_TIMESTAMP
      WHERE canvas.id = ANY($1::bigint[])
        AND canvas.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          WHERE user_to_canvas.canvas_id = canvas.id
            AND user_to_canvas.user_id = $2)
      RETURNING id`, [canvas_ids, user_id]);
    return res.rows.map((row) => row.id);
  }

  async restore_canvases_by_user(user_id, canvas_ids) {
    if (canvas_ids.length === 0) return [];
    const res = await pgExec(this._db_pool, `
      UPDATE canvas
      SET time_deleted = NULL
      WHERE canvas.id = ANY($1::bigint[])
        AND canvas.time_deleted IS NOT NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          WHERE user_to_canvas.canvas_id = canvas.id
            AND user_to_canvas.user_id = $2)
      RETURNING id`, [canvas_ids, user_id]);
    return res.rows.map((row) => row.id);
  }

  async create_user_canvas(user_canvas, graph = new Graph()) {
    return this._write_trans(async (client) => {
      const canvas = await this._create_canvas(client, user_canvas);
      // TODO:[canvas] Test doing DB calls in parallel
      await this._create_user_to_canvas(client, user_canvas.user_id, canvas.id);
      await this._write_graph(client, user_canvas.user_id, canvas.id, graph);
      return canvas;
    });
  }

  async merge_canvas_graph(user_id, canvas_id, graph, tag_descriptions) {
    const merged = await this._write_trans(async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      await this._write_graph(client, user_id, canvas_id, graph);
      await this._merge_canvas_tags(client, canvas, tag_descriptions);
      return true;
    });
    if (!merged) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async set_canvas_graph_geometry_by_user(user_id, canvas_id, node_moves, annotation_geometries) {
    return pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return null;
      const nodes = await this._move_canvas_nodes(client, canvas_id, node_moves);
      const annotations = await this._set_canvas_annotation_geometry(client, canvas_id, annotation_geometries);
      return { nodes: nodes, annotations: annotations };
    });
  }

  async trash_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids, annotation_ids) {
    const trashed = await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      await this._trash_canvas_edges(client, canvas_id, node_ids, edge_ids);
      await this._trash_canvas_nodes(client, canvas_id, node_ids);
      await this._trash_canvas_annotations(client, canvas_id, annotation_ids);
      return true;
    });
    if (!trashed) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async restore_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids, annotation_ids) {
    const restored = await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      await this._restore_canvas_nodes(client, canvas_id, node_ids);
      await this._restore_canvas_edges(client, canvas_id, edge_ids);
      await this._restore_canvas_annotations(client, canvas_id, annotation_ids);
      return true;
    });
    if (!restored) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async _move_canvas_nodes(client, canvas_id, moves) {
    if (moves.length === 0) return [];
    const [params, args] = models_to_params_and_args(
      moves,
      ["id", "x", "y"],
      [SQL_TYPES.BIGINT, SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE]);
    const canvas_id_param = args.length + 1;
    args.push(canvas_id);
    const res = await client.query(`
      UPDATE canvas_node AS cn
      SET x = v.x, y = v.y, time_updated = CURRENT_TIMESTAMP
      FROM (VALUES ${params}) AS v(id, x, y)
      WHERE cn.canvas_id = $${canvas_id_param}
        AND cn.id = v.id
        AND cn.time_deleted IS NULL
      RETURNING cn.id, cn.canvas_id, cn.data_id, cn.user_data_id, cn.ref, cn.label, cn.type, cn.x, cn.y,
                cn.hidden, cn.tags, cn.time_created, cn.time_updated, cn.time_deleted`, args);
    return res.rows;
  }

  async _trash_canvas_edges(client, canvas_id, node_ids, edge_ids) {
    await client.query(`
      UPDATE canvas_edge
      SET time_deleted = CURRENT_TIMESTAMP, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NULL
        AND (id = ANY($2::bigint[])
             OR subject_id = ANY($3::bigint[])
             OR object_id = ANY($3::bigint[]))`,
      [canvas_id, edge_ids, node_ids]);
  }

  async _trash_canvas_nodes(client, canvas_id, node_ids) {
    await client.query(`
      UPDATE canvas_node
      SET time_deleted = CURRENT_TIMESTAMP, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NULL AND id = ANY($2::bigint[])`,
      [canvas_id, node_ids]);
  }

  async _trash_canvas_annotations(client, canvas_id, annotation_ids) {
    await client.query(`
      UPDATE canvas_annotation
      SET time_deleted = CURRENT_TIMESTAMP, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NULL AND id = ANY($2::bigint[])`,
      [canvas_id, annotation_ids]);
  }

  async _restore_canvas_annotations(client, canvas_id, annotation_ids) {
    await client.query(`
      UPDATE canvas_annotation
      SET time_deleted = NULL, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NOT NULL AND id = ANY($2::bigint[])`,
      [canvas_id, annotation_ids]);
  }

  async _restore_canvas_nodes(client, canvas_id, node_ids) {
    await client.query(`
      UPDATE canvas_node
      SET time_deleted = NULL, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NOT NULL AND id = ANY($2::bigint[])`,
      [canvas_id, node_ids]);
  }

  async _restore_canvas_edges(client, canvas_id, edge_ids) {
    await client.query(`
      UPDATE ${canvas_table(ENTITY_KIND.EDGE)} ce
      SET time_deleted = NULL, time_updated = CURRENT_TIMESTAMP
      FROM ${canvas_table(ENTITY_KIND.NODE)} s, ${canvas_table(ENTITY_KIND.NODE)} o
      WHERE ce.canvas_id = $1 AND ce.time_deleted IS NOT NULL AND ce.id = ANY($2::bigint[])
        AND s.id = ce.subject_id AND s.time_deleted IS NULL
        AND o.id = ce.object_id AND o.time_deleted IS NULL
        AND (ce.user_data_id IS NULL OR EXISTS (
          SELECT 1 FROM ${user_data_table(ENTITY_KIND.EDGE)} ue
          WHERE ue.id = ce.user_data_id
            AND ${_user_edge_connects_sql("ue", "ce", "s", "o")}))`,
      [canvas_id, edge_ids]);
  }

  async _lock_active_canvas_for_user(client, user_id, canvas_id) {
    const res = await client.query(`
      SELECT canvas.id, canvas.data
      FROM user_to_canvas
      JOIN canvas ON user_to_canvas.canvas_id = canvas.id
      WHERE user_to_canvas.user_id = $1 AND canvas.id = $2 AND canvas.time_deleted IS NULL
      FOR UPDATE OF canvas`, [user_id, canvas_id]);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async _merge_canvas_tags(client, canvas, tag_descriptions) {
    if (tag_descriptions === null || Object.keys(tag_descriptions).length === 0) return;
    const existing = canvas.data?.tags ?? {};
    const merged = { ...existing, ...tag_descriptions };
    const new_data = { ...(canvas.data ?? {}), tags: merged };
    await client.query(
      `UPDATE canvas SET data = $1, time_updated = CURRENT_TIMESTAMP WHERE id = $2`,
      [new_data, canvas.id]);
  }

  async _write_graph(client, user_id, canvas_id, graph) {
    const [translator_nodes, user_only_nodes] = _split_by_translator_data(graph.nodes());
    const [translator_edges, user_only_edges] = _split_by_translator_data(graph.edges());
    const user_nodes = await this._read_owned_user_data(client, ENTITY_KIND.NODE, user_id, graph.nodes());
    const user_edges = await this._read_owned_user_data(client, ENTITY_KIND.EDGE, user_id, graph.edges());
    const node_data_ids = await this._upsert_translator_data(client, ENTITY_KIND.NODE,
      translator_nodes.map((gn) => gn.to_canvas_node_data()));
    await this._upsert_translator_canvas_entities(client, ENTITY_KIND.NODE,
      translator_nodes.map((gn) => gn.to_canvas_node(canvas_id, node_data_ids.get(gn.ref()), null)));
    await this._upsert_user_canvas_entities(client, ENTITY_KIND.NODE,
      user_only_nodes.map((gn) => gn.to_canvas_node(canvas_id, null, user_nodes.get(gn.user_data_id))));
    if (graph.edges().length === 0) return;
    for (const ge of translator_edges) {
      if (ge.has_user_data() && !ge.has_same_endpoints_as(user_edges.get(ge.user_data_id))) {
        throw new CanvasConflictError(`User edge ${ge.user_data_id} does not connect the endpoints of graph edge ${ge.ref()}`);
      }
    }
    const endpoints = [
      ...translator_edges.flatMap((ge) => [{ ref: ge.subject_ref() }, { ref: ge.object_ref() }]),
      ...user_only_edges.flatMap((ge) => _user_edge_endpoints(user_edges.get(ge.user_data_id)))
    ];
    const node_id = await this._read_active_node_ids(client, canvas_id, endpoints);
    const edge_data_ids = await this._upsert_translator_data(client, ENTITY_KIND.EDGE,
      translator_edges.map((ge) => ge.to_canvas_edge_data()));
    await this._upsert_translator_canvas_entities(client, ENTITY_KIND.EDGE,
      translator_edges.map((ge) => ge.to_canvas_edge(canvas_id, edge_data_ids.get(ge.ref()),
        node_id({ ref: ge.subject_ref() }), node_id({ ref: ge.object_ref() }), null)));
    await this._upsert_user_canvas_entities(client, ENTITY_KIND.EDGE, user_only_edges.map((ge) => {
      const user_edge = user_edges.get(ge.user_data_id);
      const [subject, object] = _user_edge_endpoints(user_edge);
      return ge.to_canvas_edge(canvas_id, null, node_id(subject), node_id(object), user_edge);
    }));
  }

  async _read_owned_user_data(client, kind, user_id, graph_entities) {
    const ids = graph_entities.filter((entity) => entity.has_user_data()).map((entity) => entity.user_data_id);
    if (ids.length === 0) return new Map();
    const res = await client.query(`
      SELECT * FROM ${user_data_table(kind)}
      WHERE id = ANY($1::bigint[]) AND user_id = $2 AND time_deleted IS NULL`, [ids, user_id]);
    if (res.rows.length !== ids.length) {
      throw new CanvasRequestError("Graph references user data that does not exist");
    }
    return new Map(res.rows.map((row) => [row.id, row]));
  }

  async _read_active_node_ids(client, canvas_id, endpoints) {
    const refs = endpoints.filter((endpoint) => endpoint.ref !== undefined).map((endpoint) => endpoint.ref);
    const user_node_ids = endpoints.filter((endpoint) => endpoint.user_node_id !== undefined)
      .map((endpoint) => endpoint.user_node_id);
    const res = await client.query(`
      SELECT id, ref, user_data_id FROM ${canvas_table(ENTITY_KIND.NODE)}
      WHERE canvas_id = $1 AND time_deleted IS NULL
        AND (ref = ANY($2::text[]) OR user_data_id = ANY($3::bigint[]))`, [canvas_id, refs, user_node_ids]);
    const by_ref = new Map(res.rows.filter((row) => row.ref !== null).map((row) => [row.ref, row.id]));
    const by_user_node_id = new Map(
      res.rows.filter((row) => row.user_data_id !== null).map((row) => [row.user_data_id, row.id]));
    return (endpoint) => {
      const id = endpoint.ref !== undefined ? by_ref.get(endpoint.ref) : by_user_node_id.get(endpoint.user_node_id);
      if (id === undefined) throw new CanvasRequestError("Graph edge references a node that is not on the canvas");
      return id;
    };
  }

  async _upsert_translator_data(client, kind, entities) {
    if (entities.length === 0) return new Map();
    const rows = await this._batch_create_entity(kind, entities, client);
    return new Map(rows.map((row) => [row.ref, row.id]));
  }

  async _upsert_translator_canvas_entities(client, kind, canvas_entities) {
    if (canvas_entities.length === 0) return;
    const table = canvas_table(kind);
    const columns = _WRITE_COLUMNS[kind];
    const [params, args] = models_to_params_and_args(
      canvas_entities, columns.insert, columns.insert.map((column) => _COLUMN_TYPES[column]));
    const revive = (column) =>
      `${column} = CASE WHEN ${table}.time_deleted IS NULL THEN ${table}.${column} ELSE EXCLUDED.${column} END`;
    const res = await client.query(`
      INSERT INTO ${table} (${columns.insert.join(", ")})
      VALUES ${params}
      ON CONFLICT (canvas_id, data_id) DO UPDATE
        SET ${columns.revive.map(revive).join(", ")},
            user_data_id = CASE WHEN ${table}.time_deleted IS NULL
              THEN COALESCE(${table}.user_data_id, EXCLUDED.user_data_id)
              ELSE COALESCE(EXCLUDED.user_data_id, ${table}.user_data_id) END,
            time_deleted = NULL,
            time_updated = CURRENT_TIMESTAMP
        WHERE ${table}.time_deleted IS NOT NULL
          OR ${table}.user_data_id IS DISTINCT FROM COALESCE(EXCLUDED.user_data_id, ${table}.user_data_id)
      RETURNING data_id, user_data_id`, args);
    const requested = new Map(canvas_entities.map((entity) => [entity.data_id, entity.user_data_id]));
    if (res.rows.some((row) => (requested.get(row.data_id) ?? row.user_data_id) !== row.user_data_id)) {
      throw new CanvasConflictError("Graph user data conflicts with user data already on the canvas");
    }
  }

  async _upsert_user_canvas_entities(client, kind, canvas_entities) {
    if (canvas_entities.length === 0) return;
    const table = canvas_table(kind);
    const columns = _WRITE_COLUMNS[kind];
    const [params, args] = models_to_params_and_args(
      canvas_entities, columns.insert, columns.insert.map((column) => _COLUMN_TYPES[column]));
    await client.query(`
      INSERT INTO ${table} (${columns.insert.join(", ")})
      VALUES ${params}
      ON CONFLICT (canvas_id, user_data_id) DO UPDATE
        SET ${columns.revive.map((column) => `${column} = EXCLUDED.${column}`).join(", ")},
            time_deleted = NULL,
            time_updated = CURRENT_TIMESTAMP
        WHERE ${table}.time_deleted IS NOT NULL AND ${table}.data_id IS NULL`, args);
  }

  async batch_create_node(nodes, client = null) {
    return this._batch_create_entity(ENTITY_KIND.NODE, nodes, client);
  }

  async batch_create_edge(edges, client = null) {
    return this._batch_create_entity(ENTITY_KIND.EDGE, edges, client);
  }

  async _batch_create_entity(kind, entities, client = null) {
    const table = translator_data_table(kind);
    if (entities.length === 0) return [];
    const [params, args] = models_to_params_and_args(
      entities,
      ["ref", "data"],
      [SQL_TYPES.TEXT, SQL_TYPES.JSONB]);
    // TODO:[performance] Characterize when saving on writes is more performant
    // Ensure we do not write if we do not need to
    const sql = `
      WITH input(ref, data) AS (
        VALUES ${params}
      ),
      upserted AS (
        INSERT INTO ${table} (ref, data)
        SELECT ref, data FROM input
        ON CONFLICT (ref) DO UPDATE
          SET data = EXCLUDED.data, time_updated = CURRENT_TIMESTAMP
          WHERE (${table}.data - 'source_time') IS DISTINCT FROM (EXCLUDED.data - 'source_time')
            AND (EXCLUDED.data ->> 'source_time')::timestamptz
                > (${table}.data ->> 'source_time')::timestamptz
        RETURNING id, ref
      )
      SELECT id, ref FROM upserted
      UNION
      SELECT t.id, t.ref FROM ${table} t
        WHERE t.ref IN (SELECT ref FROM input)
          AND NOT EXISTS (SELECT 1 FROM upserted u WHERE u.ref = t.ref)`;
    const res = client
      ? await client.query(sql, args)
      : await pgExec(this._db_pool, sql, args);
    return res.rows;
  }

  async _create_canvas(client, user_canvas) {
    const res = await client.query(`
      INSERT INTO canvas(label, layout, data)
      VALUES($1, $2, $3)
      RETURNING *`,
      [user_canvas.label, user_canvas.layout, user_canvas.data]);
    return res.rows[0];
  }

  async _create_user_to_canvas(client, user_id, canvas_id) {
    await client.query(`
      INSERT INTO user_to_canvas(user_id, canvas_id)
      VALUES($1, $2)`, [user_id, canvas_id]);
  }
}
