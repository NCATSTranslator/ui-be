'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1793218190293 };

class Migration_1793218190293 extends BaseMigration {

  static identifier = '1793218190293';

  static expected_constraints = [
    'query_working_view_pkey',
    'query_working_view_uid_fkey',
    'query_working_view_qid_fkey',
    'query_working_view_query_owner_fkey'
  ];

  constructor(dbPool) {
      super(dbPool);
      this.sql = [[
        "CREATE TABLE IF NOT EXISTS query_working_view (uid UUID NOT NULL REFERENCES users(id), qid INTEGER NOT NULL REFERENCES queries(id), version INTEGER NOT NULL, state JSONB NOT NULL, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, PRIMARY KEY (uid, qid), CONSTRAINT query_working_view_query_owner_fkey FOREIGN KEY (qid, uid) REFERENCES query_to_user(qid, uid));"
      ].join("\n")];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const expected = Migration_1793218190293.expected_constraints;
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_constraint WHERE conname = ANY($1::text[])", [expected]);
      return res.rows[0].found === expected.length;
  }

  success_message(obj=null) {
      return `create_query_working_view_table`;
  }

}
