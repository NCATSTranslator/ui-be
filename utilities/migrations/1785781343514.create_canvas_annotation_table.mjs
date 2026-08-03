'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1785781343514 };

class Migration_1785781343514 extends BaseMigration {

  static identifier = '1785781343514';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS canvas_annotation (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, canvas_id BIGINT NOT NULL REFERENCES canvas(id) ON DELETE CASCADE, content TEXT NOT NULL, x DOUBLE PRECISION NOT NULL, y DOUBLE PRECISION NOT NULL, width DOUBLE PRECISION NOT NULL, height DOUBLE PRECISION NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL);",
        "CREATE INDEX IF NOT EXISTS canvas_annotation_active_idx ON canvas_annotation(canvas_id) WHERE time_deleted IS NULL;"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_canvas_annotation_table`;
  }

}
