'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1777329783596 };

class Migration_1777329783596 extends BaseMigration {

  static identifier = '1777329783596';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS canvas (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, label TEXT NOT NULL, layout TEXT NOT NULL, data JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_deleted TIMESTAMPTZ DEFAULT NULL);",
        "CREATE TABLE IF NOT EXISTS user_to_canvas (user_id UUID NOT NULL REFERENCES users(id), canvas_id BIGINT NOT NULL REFERENCES canvas(id), PRIMARY KEY (user_id, canvas_id));"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_canvas_tables`;
  }

}
