'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1788268532281 };

class Migration_1788268532281 extends BaseMigration {

  static identifier = '1788268532281';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "CREATE TABLE IF NOT EXISTS api_keys (id UUID PRIMARY KEY, user_id UUID NOT NULL REFERENCES users(id), name TEXT NOT NULL, key_hash TEXT NOT NULL UNIQUE, key_display TEXT NOT NULL, time_created TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP, time_last_used TIMESTAMPTZ DEFAULT NULL, time_revoked TIMESTAMPTZ DEFAULT NULL);",
        "CREATE INDEX IF NOT EXISTS api_keys_user_id ON api_keys (user_id);"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `create_api_keys_table`;
  }
}
