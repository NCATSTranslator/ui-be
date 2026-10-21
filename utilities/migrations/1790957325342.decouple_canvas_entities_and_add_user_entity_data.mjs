'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1790957325342 };

class Migration_1790957325342 extends BaseMigration {

  static identifier = '1790957325342';

  static expected_constraints = [
    'canvas_node_pkey',
    'canvas_node_canvas_id_id_key',
    'canvas_node_canvas_id_data_id_key',
    'canvas_node_canvas_id_user_data_id_key',
    'canvas_node_user_data_id_fkey',
    'canvas_node_ref_matches_data',
    'canvas_node_has_source',
    'canvas_edge_pkey',
    'canvas_edge_canvas_id_data_id_key',
    'canvas_edge_canvas_id_user_data_id_key',
    'canvas_edge_canvas_id_subject_id_fkey',
    'canvas_edge_canvas_id_object_id_fkey',
    'canvas_edge_user_data_id_fkey',
    'canvas_edge_ref_matches_data',
    'canvas_edge_has_source',
    'user_edge_subject_check',
    'user_edge_object_check',
    'user_edge_subject_user_node_id_fkey',
    'user_edge_object_user_node_id_fkey'
  ];

  constructor(dbPool) {
      super(dbPool);
      this.sql = [[
        "CREATE TABLE IF NOT EXISTS user_node (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, user_id UUID NOT NULL REFERENCES users(id), label TEXT NOT NULL, type TEXT NOT NULL DEFAULT 'Other', data JSONB NOT NULL CHECK (jsonb_typeof(data) = 'object'), time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL);",
        "CREATE INDEX IF NOT EXISTS user_node_active_idx ON user_node(user_id) WHERE time_deleted IS NULL;",
        "CREATE TABLE IF NOT EXISTS user_edge (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, user_id UUID NOT NULL REFERENCES users(id), label TEXT NOT NULL, data JSONB NOT NULL CHECK (jsonb_typeof(data) = 'object'), subject_ref TEXT DEFAULT NULL, subject_user_node_id BIGINT DEFAULT NULL REFERENCES user_node(id), object_ref TEXT DEFAULT NULL, object_user_node_id BIGINT DEFAULT NULL REFERENCES user_node(id), time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL, CONSTRAINT user_edge_subject_check CHECK ((subject_ref IS NULL) <> (subject_user_node_id IS NULL)), CONSTRAINT user_edge_object_check CHECK ((object_ref IS NULL) <> (object_user_node_id IS NULL)));",
        "CREATE INDEX IF NOT EXISTS user_edge_active_idx ON user_edge(user_id) WHERE time_deleted IS NULL;",
        "CREATE INDEX IF NOT EXISTS user_edge_subject_user_node_idx ON user_edge(subject_user_node_id) WHERE subject_user_node_id IS NOT NULL;",
        "CREATE INDEX IF NOT EXISTS user_edge_object_user_node_idx ON user_edge(object_user_node_id) WHERE object_user_node_id IS NOT NULL;",
        "ALTER TABLE canvas_edge DROP CONSTRAINT canvas_edge_canvas_id_subject_id_fkey, DROP CONSTRAINT canvas_edge_canvas_id_object_id_fkey;",
        "ALTER TABLE canvas_node ADD COLUMN id BIGINT GENERATED ALWAYS AS IDENTITY;",
        "ALTER TABLE canvas_node DROP CONSTRAINT canvas_node_pkey;",
        "ALTER TABLE canvas_node ADD PRIMARY KEY (id), ADD UNIQUE (canvas_id, id), ADD UNIQUE (canvas_id, data_id);",
        "UPDATE canvas_edge ce SET subject_id = sn.id, object_id = obn.id FROM canvas_node sn, canvas_node obn WHERE sn.canvas_id = ce.canvas_id AND sn.data_id = ce.subject_id AND obn.canvas_id = ce.canvas_id AND obn.data_id = ce.object_id;",
        "ALTER TABLE canvas_edge ADD FOREIGN KEY (canvas_id, subject_id) REFERENCES canvas_node(canvas_id, id) ON DELETE CASCADE, ADD FOREIGN KEY (canvas_id, object_id) REFERENCES canvas_node(canvas_id, id) ON DELETE CASCADE;",
        "ALTER TABLE canvas_edge ADD COLUMN id BIGINT GENERATED ALWAYS AS IDENTITY;",
        "ALTER TABLE canvas_edge DROP CONSTRAINT canvas_edge_pkey;",
        "ALTER TABLE canvas_edge ADD PRIMARY KEY (id), ADD UNIQUE (canvas_id, data_id);",
        "ALTER TABLE canvas_node ALTER COLUMN data_id DROP NOT NULL, ALTER COLUMN ref DROP NOT NULL, ADD COLUMN user_data_id BIGINT REFERENCES user_node(id), ADD UNIQUE (canvas_id, user_data_id), ADD CONSTRAINT canvas_node_ref_matches_data CHECK ((data_id IS NULL) = (ref IS NULL)), ADD CONSTRAINT canvas_node_has_source CHECK (data_id IS NOT NULL OR user_data_id IS NOT NULL);",
        "CREATE INDEX IF NOT EXISTS canvas_node_user_data_idx ON canvas_node(user_data_id) WHERE user_data_id IS NOT NULL;",
        "ALTER TABLE canvas_edge ALTER COLUMN data_id DROP NOT NULL, ALTER COLUMN ref DROP NOT NULL, ADD COLUMN user_data_id BIGINT REFERENCES user_edge(id), ADD UNIQUE (canvas_id, user_data_id), ADD CONSTRAINT canvas_edge_ref_matches_data CHECK ((data_id IS NULL) = (ref IS NULL)), ADD CONSTRAINT canvas_edge_has_source CHECK (data_id IS NOT NULL OR user_data_id IS NOT NULL);",
        "CREATE INDEX IF NOT EXISTS canvas_edge_user_data_idx ON canvas_edge(user_data_id) WHERE user_data_id IS NOT NULL;"
      ].join("\n")];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const expected = Migration_1790957325342.expected_constraints;
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_constraint WHERE conname = ANY($1::text[])", [expected]);
      return res.rows[0].found === expected.length;
  }

  success_message(obj=null) {
      return `decouple_canvas_entities_and_add_user_entity_data`;
  }

}
