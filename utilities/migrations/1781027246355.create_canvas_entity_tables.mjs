'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1781027246355 };

class Migration_1781027246355 extends BaseMigration {

  static identifier = '1781027246355';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS canvas_node (canvas_id BIGINT NOT NULL REFERENCES canvas(id) ON DELETE CASCADE, data_id BIGINT NOT NULL, ref TEXT NOT NULL, label TEXT NOT NULL, type TEXT NOT NULL, x DOUBLE PRECISION NOT NULL, y DOUBLE PRECISION NOT NULL, hidden BOOLEAN NOT NULL DEFAULT false, tags JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL, PRIMARY KEY (canvas_id, data_id), FOREIGN KEY (data_id, ref) REFERENCES node(id, ref) ON DELETE CASCADE);",
        "CREATE INDEX IF NOT EXISTS canvas_node_active_idx ON canvas_node(canvas_id) WHERE time_deleted IS NULL;",
        "CREATE INDEX IF NOT EXISTS canvas_node_data_idx ON canvas_node(data_id, ref);",
        "CREATE TABLE IF NOT EXISTS canvas_edge (canvas_id BIGINT NOT NULL REFERENCES canvas(id) ON DELETE CASCADE, data_id BIGINT NOT NULL, subject_id BIGINT NOT NULL, object_id BIGINT NOT NULL, ref TEXT NOT NULL, label TEXT NOT NULL, hidden BOOLEAN NOT NULL DEFAULT false, tags JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL, PRIMARY KEY (canvas_id, data_id), FOREIGN KEY (data_id, ref) REFERENCES edge(id, ref) ON DELETE CASCADE, FOREIGN KEY (canvas_id, subject_id) REFERENCES canvas_node(canvas_id, data_id) ON DELETE CASCADE, FOREIGN KEY (canvas_id, object_id) REFERENCES canvas_node(canvas_id, data_id) ON DELETE CASCADE);",
        "CREATE INDEX IF NOT EXISTS canvas_edge_active_idx ON canvas_edge(canvas_id) WHERE time_deleted IS NULL;",
        "CREATE INDEX IF NOT EXISTS canvas_edge_data_idx ON canvas_edge(data_id, ref);",
        "CREATE INDEX IF NOT EXISTS canvas_edge_subject_idx ON canvas_edge(canvas_id, subject_id);",
        "CREATE INDEX IF NOT EXISTS canvas_edge_object_idx ON canvas_edge(canvas_id, object_id);"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_canvas_entity_tables`;
  }

}
