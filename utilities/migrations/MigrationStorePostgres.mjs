'use strict'

import { pg, pgExec, pgExecTrans } from "../../lib/postgres_preamble.mjs";
import { Migration } from "./Migration.mjs";
export { MigrationStorePostgres };


class MigrationStorePostgres {
    constructor(pool, config=null) {
      this.pool = pool ? pool : new pg.Pool(config);
    }

    async saveMigrationRecord(migration) {
        const res = await pgExec(this.pool, `
            INSERT INTO migrations (migration_id, time_begun, time_complete, run_id, message)
            VALUES ($1, $2, $3, $4, $5)
            RETURNING *;
        `, [migration.migration_id, migration.time_begun, migration.time_complete, migration.run_id, migration.message]);
        return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }

    async getMostRecentMigration() {
        const res = await pgExec(this.pool, `SELECT * FROM migrations ORDER BY migration_id DESC LIMIT 1;`);
        return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }

    async getMigrationByMigrationId(migration_id) {
        const res = await pgExec(this.pool, `SELECT * FROM migrations where migration_id = $1;`,
            [migration_id]);
        return res.rows.length > 0 ? new Migration(res.rows[0]) : null;
    }

    async getMigrationTableStatus() {
        /* This mysterious-seeming query is courtesy chatgpt: "to_regclass('public.migrations') returns
         * the table's OID (object identifier) if it exists or NULL if it doesn't. This is the most
         * efficient way to check table existence when you don’t need extra metadata."
         */
        const exists = await pgExec(this.pool, `SELECT to_regclass('migrations') IS NOT NULL as exists;`);
        if (!exists || !exists.rows[0].exists) {
            return {
                exists: false,
                n_rows: 0
            };
        }
        const n_rows = await pgExec(this.pool, `select count(id) as n_rows from migrations;`);
        return {
            exists: true,
            n_rows: n_rows.rows[0].n_rows
        };
    }
}
