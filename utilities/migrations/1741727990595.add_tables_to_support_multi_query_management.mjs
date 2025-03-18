'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_1741727990595 };

class Migration_1741727990595 extends BaseMigration {

  static identifier = '1741727990595';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        "create table queries (id serial primary key, pk uuid, status text not null default 'running', time_created timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL, time_updated timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL, deleted boolean not null default false, metadata jsonb);",
        "create table query_to_user (qid int not null, uid uuid not null, primary key (qid, uid));"
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return `Ran successfully`;
  }

}
