'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1786994798873 };

class Migration_1786994798873 extends BaseMigration {

  static identifier = '1786994798873';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "UPDATE edge SET data = data - 'support' - 'type' WHERE data ?| array['support', 'type'];"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      const res = await pgExec(this.dbPool,
        "SELECT COUNT(*)::int AS remaining FROM edge WHERE data ?| array['support', 'type']");
      return res.rows[0].remaining === 0;
  }

  success_message(obj=null) {
      return `strip_support_and_type_from_edge_data`;
  }

}
