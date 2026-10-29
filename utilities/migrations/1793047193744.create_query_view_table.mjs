'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1793047193744 };

class Migration_1793047193744 extends BaseMigration {

  static identifier = '1793047193744';

  static expected_constraints = [
    'query_view_pkey',
    'query_view_uid_fkey',
    'query_view_qid_fkey',
    'query_view_query_owner_fkey',
    'query_view_filter_state_owner_fkey'
  ];

  constructor(dbPool) {
      super(dbPool);
      this.sql = [[
        "CREATE TABLE IF NOT EXISTS query_view (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, uid UUID NOT NULL REFERENCES users(id), qid INTEGER NOT NULL REFERENCES queries(id), filter_state_id BIGINT NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL, CONSTRAINT query_view_query_owner_fkey FOREIGN KEY (qid, uid) REFERENCES query_to_user(qid, uid), CONSTRAINT query_view_filter_state_owner_fkey FOREIGN KEY (filter_state_id, uid) REFERENCES user_filter_state(id, user_id));",
        "CREATE UNIQUE INDEX IF NOT EXISTS query_view_active_triple_idx ON query_view(uid, qid, filter_state_id) WHERE time_deleted IS NULL;",
        "CREATE INDEX IF NOT EXISTS query_view_query_idx ON query_view(uid, qid) WHERE time_deleted IS NULL;"
      ].join("\n")];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const expected = Migration_1793047193744.expected_constraints;
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_constraint WHERE conname = ANY($1::text[])", [expected]);
      return res.rows[0].found === expected.length;
  }

  success_message(obj=null) {
      return `create_query_view_table`;
  }

}
