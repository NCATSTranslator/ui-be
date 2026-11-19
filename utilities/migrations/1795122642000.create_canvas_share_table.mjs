'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1795122642000 };

class Migration_1795122642000 extends BaseMigration {

  static identifier = '1795122642000';

  static expected_constraints = [
    'canvas_share_pkey',
    'canvas_share_canvas_id_fkey',
    'canvas_share_user_id_fkey'
  ];

  constructor(db_pool) {
      super(db_pool);
      this.sql = [[
        "CREATE TABLE IF NOT EXISTS canvas_share (id UUID PRIMARY KEY, canvas_id BIGINT NOT NULL REFERENCES canvas(id) ON DELETE CASCADE, user_id UUID NOT NULL REFERENCES users(id), time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);",
        "CREATE INDEX IF NOT EXISTS canvas_share_canvas_id_idx ON canvas_share(canvas_id);"
      ].join("\n")];
  }

  async verify(obj=null) {
      const expected = Migration_1795122642000.expected_constraints;
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_constraint WHERE conname = ANY($1::text[])", [expected]);
      return res.rows[0].found === expected.length;
  }

  success_message(obj=null) {
      return `create_canvas_share_table`;
  }

}
