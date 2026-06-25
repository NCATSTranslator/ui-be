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

  async get_node_data(data_id) {
    const res = await pgExec(this._db_pool,
      `SELECT data FROM node WHERE id = $1`, [data_id]);
    if (res.rows.length === 0) return null;
    return res.rows[0].data;
  }

  async get_edge_data(data_id) {
    const res = await pgExec(this._db_pool,
      `SELECT data FROM edge WHERE id = $1`, [data_id]);
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

  async create_user_canvas(user_canvas, graph = new Graph()) {
    return await pgExecTrans(this._db_pool, async (client) => {
      const canvas = await this._create_canvas(client, user_canvas);
      // TODO:[canvas] Test doing DB calls in parallel
      await this._create_user_to_canvas(client, user_canvas.user_id, canvas.id);
      await this._create_canvas_graph(client, canvas.id, graph);
      return canvas;
    });
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
    const canvas_edges = graph_edges.map((ge) =>
      ge.to_canvas_edge(
        canvas_id,
        data_id_by_ref.get(ge.ref()),
        node_data_id_by_ref.get(ge.subject_ref()),
        node_data_id_by_ref.get(ge.object_ref())));
    return this.batch_create_canvas_edge(canvas_edges, client);
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
