'use strict'

import { pg, pgExec, pgExecTrans } from "../../lib/postgres_preamble.mjs";
import { Migration } from "./Migration.mjs";


class MigrationStorePostgres {
    constructor(pool, config=null) {
      this.pool = pool ? pool : new pg.Pool(config);
    }


    async saveMigrationRecord(migration) {
        const res = await pgExec(this.pool, `
            INSERT INTO migrations (migration_id, time_begun, time_complete, message, run_id)
            VALUES ($1, $2, $3, $4, $5)
            RETURNING *
        `, [migration.migration_id, migration.time_begun, migration.time_complete, migration.message,
            migration.run_id]);
        return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }

    async getMostRecentMigration() {
        const res = await pgExec(this.pool, `SELECT * FROM migrations ORDER BY migration_id DESC LIMIT 1`);
        return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }

    async getMigrationByMigrationId(migration_id) {
        const res = await pgExec(this.pool, `SELECT * FROM migrations where migration_id = $1`,
            [migration_id]);
            return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }
}
