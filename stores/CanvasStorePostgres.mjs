export { CanvasStorePostgres }

import { pgExec, pgExecTrans } from "#lib/postgres_preamble.mjs";
import { SQL_TYPES, models_to_params_and_args } from "#model/common.mjs";
import { Graph } from "#model/Canvas.mjs";

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
    const sql_canvas_deleted = include_deleted ? "" : " AND canvas.time_deleted IS NULL";
    const canvas_res = await pgExec(this._db_pool, `
      SELECT canvas.id, canvas.data
      FROM user_to_canvas
      JOIN canvas ON user_to_canvas.canvas_id = canvas.id
      WHERE user_to_canvas.user_id = $1 AND canvas.id = $2${sql_canvas_deleted}`,
      [user_id, canvas_id]);
    if (canvas_res.rows.length === 0) return null;
    const canvas = canvas_res.rows[0];
    const sql_entity_deleted = include_deleted ? "" : " AND time_deleted IS NULL";
    const node_res = await pgExec(this._db_pool, `
      SELECT canvas_id, data_id, ref, label, type, x, y, hidden, tags,
             time_created, time_updated, time_deleted
      FROM canvas_node
      WHERE canvas_id = $1${sql_entity_deleted}`, [canvas_id]);
    const edge_res = await pgExec(this._db_pool, `
      SELECT canvas_id, data_id, subject_id, object_id, ref, label, hidden, tags,
             time_created, time_updated, time_deleted
      FROM canvas_edge
      WHERE canvas_id = $1${sql_entity_deleted}`, [canvas_id]);
    return {
      nodes: node_res.rows,
      edges: edge_res.rows,
      tags: canvas.data?.tags ?? null
    };
  }

  async get_node_data(user_id, canvas_id, data_id) {
    const res = await pgExec(this._db_pool, `
      SELECT node.data
      FROM canvas_node
      JOIN node ON node.id = canvas_node.data_id
      JOIN user_to_canvas ON user_to_canvas.canvas_id = canvas_node.canvas_id
      JOIN canvas ON canvas.id = canvas_node.canvas_id
      WHERE canvas_node.canvas_id = $1
        AND canvas_node.data_id = $2
        AND user_to_canvas.user_id = $3
        AND canvas.time_deleted IS NULL`, [canvas_id, data_id, user_id]);
    if (res.rows.length === 0) return null;
    return res.rows[0].data;
  }

  async get_edge_data(user_id, canvas_id, data_id) {
    const res = await pgExec(this._db_pool, `
      SELECT edge.data
      FROM canvas_edge
      JOIN edge ON edge.id = canvas_edge.data_id
      JOIN user_to_canvas ON user_to_canvas.canvas_id = canvas_edge.canvas_id
      JOIN canvas ON canvas.id = canvas_edge.canvas_id
      WHERE canvas_edge.canvas_id = $1
        AND canvas_edge.data_id = $2
        AND user_to_canvas.user_id = $3
        AND canvas.time_deleted IS NULL`, [canvas_id, data_id, user_id]);
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

  async update_canvas_node_by_user(user_id, canvas_id, data_id, fields) {
    const [set_clause, values] = this._build_element_update(fields, canvas_id, data_id, user_id);
    const res = await pgExec(this._db_pool, `
      UPDATE canvas_node
      SET ${set_clause}
      WHERE canvas_node.canvas_id = $${values.canvas_id_param}
        AND canvas_node.data_id = $${values.data_id_param}
        AND canvas_node.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          JOIN canvas ON user_to_canvas.canvas_id = canvas.id
          WHERE user_to_canvas.canvas_id = canvas_node.canvas_id
            AND user_to_canvas.user_id = $${values.user_id_param}
            AND canvas.time_deleted IS NULL)
      RETURNING *`, values.args);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  async update_canvas_edge_by_user(user_id, canvas_id, data_id, fields) {
    const [set_clause, values] = this._build_element_update(fields, canvas_id, data_id, user_id);
    const res = await pgExec(this._db_pool, `
      UPDATE canvas_edge
      SET ${set_clause}
      WHERE canvas_edge.canvas_id = $${values.canvas_id_param}
        AND canvas_edge.data_id = $${values.data_id_param}
        AND canvas_edge.time_deleted IS NULL
        AND EXISTS (
          SELECT 1 FROM user_to_canvas
          JOIN canvas ON user_to_canvas.canvas_id = canvas.id
          WHERE user_to_canvas.canvas_id = canvas_edge.canvas_id
            AND user_to_canvas.user_id = $${values.user_id_param}
            AND canvas.time_deleted IS NULL)
      RETURNING *`, values.args);
    return res.rows.length > 0 ? res.rows[0] : null;
  }

  _build_element_update(fields, canvas_id, data_id, user_id) {
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
    const data_id_param = i;
    args.push(data_id);
    i += 1;
    const user_id_param = i;
    args.push(user_id);
    return [set_clauses.join(", "), { args, canvas_id_param, data_id_param, user_id_param }];
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
    return await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._create_canvas(client, user_canvas);
      // TODO:[canvas] Test doing DB calls in parallel
      await this._create_user_to_canvas(client, user_canvas.user_id, canvas.id);
      await this._create_canvas_graph(client, canvas.id, graph);
      return canvas;
    });
  }

  async merge_canvas_graph(user_id, canvas_id, graph, tag_descriptions) {
    const merged = await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      const known_node_refs = await this._get_canvas_node_refs(client, canvas_id);
      graph.assert_edges_reference_nodes(known_node_refs);
      await this._create_canvas_graph(client, canvas_id, graph);
      await this._merge_canvas_tags(client, canvas, tag_descriptions);
      return true;
    });
    if (!merged) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async move_canvas_nodes_by_user(user_id, canvas_id, moves) {
    return pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return null;
      return this._move_canvas_nodes(client, canvas_id, moves);
    });
  }

  async trash_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids) {
    const trashed = await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      await this._trash_canvas_edges(client, canvas_id, node_ids, edge_ids);
      await this._trash_canvas_nodes(client, canvas_id, node_ids);
      return true;
    });
    if (!trashed) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async restore_canvas_graph_by_user(user_id, canvas_id, node_ids, edge_ids) {
    const restored = await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._lock_active_canvas_for_user(client, user_id, canvas_id);
      if (canvas === null) return false;
      await this._restore_canvas_nodes(client, canvas_id, node_ids);
      await this._restore_canvas_edges(client, canvas_id, edge_ids);
      return true;
    });
    if (!restored) return null;
    return this.get_canvas_graph_by_user(user_id, canvas_id, false);
  }

  async _move_canvas_nodes(client, canvas_id, moves) {
    const [params, args] = models_to_params_and_args(
      moves,
      ["data_id", "x", "y"],
      [SQL_TYPES.BIGINT, SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE]);
    const canvas_id_param = args.length + 1;
    args.push(canvas_id);
    const res = await client.query(`
      UPDATE canvas_node AS cn
      SET x = v.x, y = v.y, time_updated = CURRENT_TIMESTAMP
      FROM (VALUES ${params}) AS v(data_id, x, y)
      WHERE cn.canvas_id = $${canvas_id_param}
        AND cn.data_id = v.data_id
        AND cn.time_deleted IS NULL
      RETURNING cn.canvas_id, cn.data_id, cn.ref, cn.label, cn.type, cn.x, cn.y, cn.hidden,
                cn.tags, cn.time_created, cn.time_updated, cn.time_deleted`, args);
    return res.rows;
  }

  async _trash_canvas_edges(client, canvas_id, node_ids, edge_ids) {
    await client.query(`
      UPDATE canvas_edge
      SET time_deleted = CURRENT_TIMESTAMP, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NULL
        AND (data_id = ANY($2::bigint[])
             OR subject_id = ANY($3::bigint[])
             OR object_id = ANY($3::bigint[]))`,
      [canvas_id, edge_ids, node_ids]);
  }

  async _trash_canvas_nodes(client, canvas_id, node_ids) {
    await client.query(`
      UPDATE canvas_node
      SET time_deleted = CURRENT_TIMESTAMP, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NULL AND data_id = ANY($2::bigint[])`,
      [canvas_id, node_ids]);
  }

  async _restore_canvas_nodes(client, canvas_id, node_ids) {
    await client.query(`
      UPDATE canvas_node
      SET time_deleted = NULL, time_updated = CURRENT_TIMESTAMP
      WHERE canvas_id = $1 AND time_deleted IS NOT NULL AND data_id = ANY($2::bigint[])`,
      [canvas_id, node_ids]);
  }

  async _restore_canvas_edges(client, canvas_id, edge_ids) {
    await client.query(`
      UPDATE canvas_edge ce
      SET time_deleted = NULL, time_updated = CURRENT_TIMESTAMP
      WHERE ce.canvas_id = $1 AND ce.time_deleted IS NOT NULL AND ce.data_id = ANY($2::bigint[])
        AND EXISTS (SELECT 1 FROM canvas_node sn
                    WHERE sn.canvas_id = ce.canvas_id AND sn.data_id = ce.subject_id
                      AND sn.time_deleted IS NULL)
        AND EXISTS (SELECT 1 FROM canvas_node obn
                    WHERE obn.canvas_id = ce.canvas_id AND obn.data_id = ce.object_id
                      AND obn.time_deleted IS NULL)`,
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

  async _get_canvas_node_refs(client, canvas_id) {
    const res = await client.query(
      `SELECT ref FROM canvas_node WHERE canvas_id = $1 AND time_deleted IS NULL`, [canvas_id]);
    return res.rows.map((row) => row.ref);
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

  async _create_canvas_graph(client, canvas_id, graph) {
    const graph_nodes = graph.nodes();
    const graph_edges = graph.edges();
    let node_data_id_by_ref = new Map();
    if (graph_nodes.length > 0) {
      node_data_id_by_ref = await this._create_canvas_nodes(client, canvas_id, graph_nodes);
    }
    if (graph_edges.length > 0) {
      await this._create_canvas_edges(client, canvas_id, graph_edges, node_data_id_by_ref);
    }
  }

  async _create_canvas_nodes(client, canvas_id, graph_nodes) {
    const node_data = graph_nodes.map((gn) => gn.to_canvas_node_data());
    const upserted = await this.batch_create_node(node_data, client);
    const data_id_by_ref = new Map(upserted.map((row) => [row.ref, row.id]));
    const canvas_nodes = graph_nodes.map((gn) =>
      gn.to_canvas_node(canvas_id, data_id_by_ref.get(gn.ref())));
    await this.batch_create_canvas_node(canvas_nodes, client);
    return data_id_by_ref;
  }

  async _create_canvas_edges(client, canvas_id, graph_edges, node_data_id_by_ref) {
    const edge_data = graph_edges.map((ge) => ge.to_canvas_edge_data());
    const upserted = await this.batch_create_edge(edge_data, client);
    const data_id_by_ref = new Map(upserted.map((row) => [row.ref, row.id]));
    const endpoint_ids = await this._resolve_edge_endpoint_ids(client, graph_edges, node_data_id_by_ref);
    const canvas_edges = graph_edges.map((ge) =>
      ge.to_canvas_edge(
        canvas_id,
        data_id_by_ref.get(ge.ref()),
        endpoint_ids.get(ge.subject_ref()),
        endpoint_ids.get(ge.object_ref())));
    return this.batch_create_canvas_edge(canvas_edges, client);
  }

  async _resolve_edge_endpoint_ids(client, graph_edges, node_data_id_by_ref) {
    // NOTE: assert_edges_reference_nodes (run under lock before this) guarantees every endpoint ref
    // is on the canvas or in this submission, so each resolves to an id here. If that invariant ever
    // breaks, an unresolved ref yields undefined and surfaces as a NOT NULL violation on insert.
    const missing = new Set();
    for (const ge of graph_edges) {
      for (const ref of [ge.subject_ref(), ge.object_ref()]) {
        if (!node_data_id_by_ref.has(ref)) missing.add(ref);
      }
    }
    if (missing.size === 0) return node_data_id_by_ref;
    const res = await client.query(
      `SELECT id, ref FROM node WHERE ref = ANY($1::text[])`, [[...missing]]);
    const resolved = new Map(node_data_id_by_ref);
    for (const row of res.rows) resolved.set(row.ref, row.id);
    return resolved;
  }

  async batch_create_node(nodes, client = null) {
    return this._batch_create_entity("node", nodes, client);
  }

  async batch_create_edge(edges, client = null) {
    return this._batch_create_entity("edge", edges, client);
  }

  async _batch_create_entity(type, entities, client = null) {
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
        INSERT INTO ${type} (ref, data)
        SELECT ref, data FROM input
        ON CONFLICT (ref) DO UPDATE
          SET data = EXCLUDED.data, time_updated = CURRENT_TIMESTAMP
          WHERE (${type}.data - 'source_time') IS DISTINCT FROM (EXCLUDED.data - 'source_time')
            AND (EXCLUDED.data ->> 'source_time')::timestamptz
                > (${type}.data ->> 'source_time')::timestamptz
        RETURNING id, ref
      )
      SELECT id, ref FROM upserted
      UNION
      SELECT t.id, t.ref FROM ${type} t
        WHERE t.ref IN (SELECT ref FROM input)
          AND NOT EXISTS (SELECT 1 FROM upserted u WHERE u.ref = t.ref)`;
    const res = client
      ? await client.query(sql, args)
      : await pgExec(this._db_pool, sql, args);
    return res.rows;
  }

  async batch_create_canvas_node(canvas_nodes, client = null) {
    if (canvas_nodes.length === 0) return [];
    const [params, args] = models_to_params_and_args(
      canvas_nodes,
      ["canvas_id", "data_id", "ref", "label", "type", "x", "y", "hidden", "tags"],
      [SQL_TYPES.BIGINT, SQL_TYPES.BIGINT, SQL_TYPES.TEXT, SQL_TYPES.TEXT, SQL_TYPES.TEXT,
       SQL_TYPES.DOUBLE, SQL_TYPES.DOUBLE, SQL_TYPES.BOOL, SQL_TYPES.JSONB]);
    const sql = `
      INSERT INTO canvas_node (canvas_id, data_id, ref, label, type, x, y, hidden, tags)
      VALUES ${params}
      ON CONFLICT (canvas_id, data_id) DO UPDATE
        SET time_deleted = NULL,
            label = EXCLUDED.label,
            type = EXCLUDED.type,
            x = EXCLUDED.x,
            y = EXCLUDED.y,
            hidden = EXCLUDED.hidden,
            tags = EXCLUDED.tags,
            time_updated = CURRENT_TIMESTAMP
        WHERE canvas_node.time_deleted IS NOT NULL
      RETURNING *`;
    const res = client
      ? await client.query(sql, args)
      : await pgExec(this._db_pool, sql, args);
    return res.rows;
  }

  async batch_create_canvas_edge(canvas_edges, client = null) {
    if (canvas_edges.length === 0) return [];
    const [params, args] = models_to_params_and_args(
      canvas_edges,
      ["canvas_id", "data_id", "subject_id", "object_id", "ref", "label", "hidden", "tags"],
      [SQL_TYPES.BIGINT, SQL_TYPES.BIGINT, SQL_TYPES.BIGINT, SQL_TYPES.BIGINT, SQL_TYPES.TEXT,
       SQL_TYPES.TEXT, SQL_TYPES.BOOL, SQL_TYPES.JSONB]);
    const sql = `
      INSERT INTO canvas_edge (canvas_id, data_id, subject_id, object_id, ref, label, hidden, tags)
      VALUES ${params}
      ON CONFLICT (canvas_id, data_id) DO UPDATE
        SET time_deleted = NULL,
            subject_id = EXCLUDED.subject_id,
            object_id = EXCLUDED.object_id,
            label = EXCLUDED.label,
            hidden = EXCLUDED.hidden,
            tags = EXCLUDED.tags,
            time_updated = CURRENT_TIMESTAMP
        WHERE canvas_edge.time_deleted IS NOT NULL
      RETURNING *`;
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
