'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1795552209969 };

class Migration_1795552209969 extends BaseMigration {

  static identifier = '1795552209969';

  static expected_index = 'queries_pk_idx';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE UNIQUE INDEX IF NOT EXISTS queries_pk_idx ON queries(pk);"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS found FROM pg_indexes WHERE tablename = 'queries' AND indexname = $1",
        [Migration_1795552209969.expected_index]);
      return res.rows[0].found === 1;
  }

  success_message(obj=null) {
      return `add_unique_index_on_queries_pk`;
  }

}
