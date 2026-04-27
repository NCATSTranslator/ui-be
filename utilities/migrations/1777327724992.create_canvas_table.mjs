'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1777327724992 };

class Migration_1777327724992 extends BaseMigration {

  static identifier = '1777327724992';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS canvas (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, label TEXT NOT NULL, layout TEXT NOT NULL, data JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_canvas_table`;
  }

}
