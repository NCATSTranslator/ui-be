'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1781026708566 };

class Migration_1781026708566 extends BaseMigration {

  static identifier = '1781026708566';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS node (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, ref TEXT NOT NULL UNIQUE, data JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, UNIQUE (id, ref));",
        "CREATE TABLE IF NOT EXISTS edge (id BIGINT GENERATED ALWAYS AS IDENTITY PRIMARY KEY, ref TEXT NOT NULL UNIQUE, data JSONB NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_updated TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, UNIQUE (id, ref));"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_canvas_entity_data_tables`;
  }

}
