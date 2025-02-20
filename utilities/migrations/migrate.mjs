'use strict';

import { readdir } from 'node:fs/promises';
import path from 'path';
import { bootstrapConfig } from '../../lib/config.mjs';
import { pg, pgExec } from '../../lib/postgres_preamble.mjs';
import { logger } from '../../lib/logger.mjs';
import { v4 as uuidv4 } from 'uuid';
import { MigrationStorePostgres } from './MigrationStorePostgres.mjs';
import { Migration } from './Migration.mjs';
import meow from 'meow';

export { getMigrationFiles };

const MIGRATION_FILES_RX = new RegExp(`^(\\d+)\\..*\\.mjs\$`); // Note doubled \\
const MIGRATIONS_DIR = 'utilities/migrations';
const RUN_ID = uuidv4();
const migration_logger = logger.child({run_id: RUN_ID});
migration_logger.level = 'debug';

const program_name = path.basename(process.argv[1]);
const cli = meow(`
    Usage:
    $ node ${program_name} <options>

    Options:
      --config-file, -c: required
      --local-overrides, -l: optional
      --one-big-tx, -b: boolean
      --force-dangerous-things: true
      --first: number, optional
      --last: number, optional
    `, {
        importMeta: import.meta,
        allowUnknownFlags: false,
        flags: {
            configFile: {
                type: 'string',
                shortFlag: 'c',
                isRequired: true
            },
            localOverrides: {
                type: 'string',
                shortFlag: 'l',
                isRequired: false
            },
            oneBigTx: {
                type: 'boolean',
                default: true,
                isRequired: false
            },
            forceDangerousThings: {
                type: 'boolean',
                default: false,
                isRequired: false
            },
            first: {
                type: 'number',
                isRequired: false,
                default: 0
            },
            last: {
                type: 'number',
                isRequired: false,
                default: Infinity // does not seem to work
            }
        }
    }
);

migration_logger.info({flags: cli.flags});



async function getMigrationFiles(dir, firstTimestamp=0, lastTimestamp=Infinity) {
    migration_logger.trace(MIGRATION_FILES_RX);
    let all_files = await readdir(dir);
    migration_logger.trace(all_files);
    let migration_files = all_files.filter(e => e.match(MIGRATION_FILES_RX));
    migration_logger.trace(migration_files);
    let target_files = migration_files.filter((e) => {
        let file_timestamp = parseInt(e.match(MIGRATION_FILES_RX)[1], 10);
        return file_timestamp >= firstTimestamp && file_timestamp <= lastTimestamp;
    });
    return target_files.sort((a, b) => {
        let a_timestamp = parseInt(a.match(MIGRATION_FILES_RX)[1], 10);
        let b_timestamp = parseInt(b.match(MIGRATION_FILES_RX)[1], 10);
        migration_logger.trace(`timestamps: ${a_timestamp}, ${b_timestamp}`);
        return a_timestamp - b_timestamp;
    });
}

async function instantiateMigration(file) {
    migration_logger.debug(`importing ${file}`);
    const imp = await import('./' + file);
    const cur_migration_class = Object.values(imp)[0];
    return {
        migration: new cur_migration_class(dbPool),
        migration_id: cur_migration_class.identifier
    }
}

async function retrieveLatestMigrationRecord(migrationStore) {
    return
 }

 async function runSanityChecks(migrationStore, first_file, arg_first_timestamp, force_dangerous_things) {

    // If the migrations table doesn't exist, bail.
    let migration_table_status = await migrationStore.getMigrationTableStatus();
    if (!migration_table_status || !migration_table_status.exists) {
        throw new Error(`Migrations table does not exist in DB. Aborting.`);
    }

    if (migration_table_status.n_rows > 0) {
        /* First, confirm that the first migration slated to run is strictly after the last
         * successful run recorded in the DB */
        let first_file_timestamp = parseInt(first_file.match(MIGRATION_FILES_RX)[1], 10);
        let db_most_recent = await migrationStore.getMostRecentMigration();
        if (!db_most_recent) {
            throw new Error('Failed to retrieve latest migration record. Aborting.');
        }
        db_most_recent = db_most_recent.migration_id;
        if (first_file_timestamp <= db_most_recent) {
            throw new Error(`The proposed starting point (${first_file_timestamp}) is earlier than `
                + `the most recent run recorded in the DB (${db_most_recent}). Aborting.`);
        }

        /* Next, more complicatedly, confirm that the first migration slated to run is in fact the immediate
         * next one after the last successful run. Don't skip migrations unless explicitly forced to. */
        let sanity_check_files = getMigrationFiles(MIGRATIONS_DIR, db_most_recent);
        if (sanity_check_files[0] !== first_file && !force_dangerous_things) {
            throw new Error(`The specified starting point ${arg_first_timestamp} skips some migrations, `
                + `starting with ${sanity_check_files[0]}. If you really want to do this, `
                + `rerun with --force-dangerous-things.`);
        }
    // If the migrations table is empty, don't accept a default starting arg.
    } else if (arg_first_timestamp === 0) {
        throw new Error(`Migrations table is empty. In this case, you must specify an explicit migration to start at. Aborting.`);
    }
 }

 async function migrationAlreadyRun(migrationStore, migrationId) {
     const res = await migrationStore.getMigrationByMigrationId(migrationId);
     return res ? true : false;
 }

async function biggy(targetFiles, dbPool, migrationStore, oneBigTx=true) {
    let start;
    let end;
    let migration_record;

    if (oneBigTx) {
        await pgExec(dbPool, 'BEGIN');
        migration_logger.info('Beginning big tx');
    }
    try {
        for (const f of targetFiles) {
            let {migration, migration_id} = await instantiateMigration(f);
            let alreadyRun = await migrationAlreadyRun(migrationStore, migration_id);
            if (alreadyRun) {
                throw new Error(`Migration already run: ${migration_id}`);
            }

            migration_logger.info(`Executing ${migration_id}`);
            start = new Date();
            if (!oneBigTx) {
                await pgExec(dbPool, 'BEGIN')
                migration_logger.info(`Beginning transactions for ${migration_id}`);
            }
            let res = await migration.execute();
            if (!res) {
                throw new Error(`Migration ${migration_id} execute() returned false. Aborting`);
            }
            res = await migration.verify();
            if (!res) {
                throw new Error(`Migration ${migration_id} verification failed. Aborting.`)
            }
            end = new Date();
            migration_record = new Migration(null, migration_id, start, end,
                migration.successMessage(), RUN_ID);
            res = await migrationStore.saveMigrationRecord(migration_record);
            if (!res) {
                throw new Error(`Could not save migration record for ${migration_id}: `
                    + `${JSON.stringify(migration_record)}. Aborting`);
            }

            if (!oneBigTx) {
                await pgExec(dbPool, 'COMMIT');
                migration_logger.info(`Committed transactions Tx for ${migration_id}`);
            }
        }
    } catch (err) {
        migration_logger.error("We got problems. Executing rollback");
        let rb = await pgExec(dbPool, 'ROLLBACK');
        console.log(rb);
        throw err;
    }
    if (oneBigTx) {
        await pgExec(dbPool, 'COMMIT');
        migration_logger.info("Completed big tx");
    }
    migration_logger.info("Completed run");

}


async function recordAppliedMigration(migration_id, success) {

}

const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
migration_logger.trace(CONFIG);

const dbPool = new pg.Pool({
    ...CONFIG.storage.pg,
    password: CONFIG.secrets.pg.password,
    ssl: CONFIG.db_conn.ssl
  });
migration_logger.trace(dbPool);

const migrationStore = new MigrationStorePostgres(dbPool);

let target = await getMigrationFiles(MIGRATIONS_DIR,
    cli.flags.first ?? 0, cli.flags.last ?? Infinity,
    '.mjs');


console.log(foo);
throw 42;
await biggy(target, dbPool, migrationStore, cli.flags.oneBigTx);

dbPool.end();






