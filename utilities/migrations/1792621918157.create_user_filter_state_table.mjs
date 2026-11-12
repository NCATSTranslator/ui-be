'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1792621918157 };

class Migration_1792621918157 extends BaseMigration {

  static identifier = '1792621918157';

  static expected_constraints = [
    'user_filter_state_pkey',
    'user_filter_state_user_id_fkey',
    'user_filter_state_id_user_id_key',
    'user_filter_state_visible_has_label'
  ];

  constructor(dbPool) {
      super(dbPool);
      this.sql = [[
        "CREATE TABLE IF NOT EXISTS user_filter_state (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, user_id UUID NOT NULL REFERENCES users(id), label TEXT DEFAULT NULL, hidden BOOLEAN NOT NULL DEFAULT FALSE, version INTEGER NOT NULL, state JSONB NOT NULL, state_hash TEXT GENERATED ALWAYS AS (md5(state::text)) STORED, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL, CONSTRAINT user_filter_state_visible_has_label CHECK (hidden OR label IS NOT NULL), UNIQUE (id, user_id));",
        "CREATE UNIQUE INDEX IF NOT EXISTS user_filter_state_state_idx ON user_filter_state(user_id, version, state_hash);",
        "CREATE INDEX IF NOT EXISTS user_filter_state_preset_idx ON user_filter_state(user_id) WHERE time_deleted IS NULL AND NOT hidden;"
      ].join("\n")];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const expected = Migration_1792621918157.expected_constraints;
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_constraint WHERE conname = ANY($1::text[])", [expected]);
      return res.rows[0].found === expected.length;
  }

  success_message(obj=null) {
      return `create_user_filter_state_table`;
  }

}
