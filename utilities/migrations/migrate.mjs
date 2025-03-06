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

const MIGRATION_FILES_RX = new RegExp(`^(\\d+)\\..*\\.mjs\$`); // Note doubled \\
const MIGRATIONS_DIR = 'utilities/migrations';
const DEFAULT_FIRST_MIGRATION_TIMESTAMP = 0;
const RUN_ID = uuidv4();
const MIGRATION_LOGGER = logger.child({run_id: RUN_ID});
MIGRATION_LOGGER.level = 'info';

// Parse the command line args
const program_name = path.basename(process.argv[1]);
const cli = meow(`
    Usage:
    $ node ${program_name} <options>

    Options:
      --config-file, -c: required
      --local-overrides, -l: optional
      --one-big-tx: boolean
      --force-dangerous-things: true
      --first: number, optional
      --last: number, optional
      --log-level: string, optional
    `, {
        importMeta: import.meta,
        allowUnknownFlags: false,
        flags: {
            // Base configuration file
            configFile: {
                type: 'string',
                shortFlag: 'c',
                isRequired: true
            },
            // Local overrides config file (to run in local env)
            localOverrides: {
                type: 'string',
                shortFlag: 'l',
                isRequired: false
            },
            // Run all the migrations indicated by the args as one single transaction as opposed to individual
            // per-migration transactions.
            // Safer, and highly recommended (is the default).
            oneBigTx: {
                type: 'boolean',
                default: true,
                isRequired: false
            },
            // A general purpose flag used to override safety recommendations.
            forceDangerousThings: {
                type: 'boolean',
                default: false,
                isRequired: false
            },
            // Timestamp of the first migration to run (inclusive)
            first: {
                type: 'number',
                isRequired: false,
                default: DEFAULT_FIRST_MIGRATION_TIMESTAMP
            },
            // Timestamp of the final migration to run (inclusive)
            last: {
                type: 'number',
                isRequired: false,
                default: Infinity // does not seem to work
            },
            // Logging level, should be 'info' in production
            logLevel: {
                type: 'string',
                choices: ['trace', 'debug', 'info'],
                default: 'info',
                isRequired: false
            }

        }
    }
);

MIGRATION_LOGGER.info({flags: cli.flags});
MIGRATION_LOGGER.level = cli.flags.logLevel;

/* Given cli args for the timestamps for range of migrations to run, return a
 * sorted (by timestamp, ascending) list of actual files
 */
async function get_migration_files(dir, first_timestamp=DEFAULT_FIRST_MIGRATION_TIMESTAMP, last_timestamp=Infinity) {
    MIGRATION_LOGGER.trace(MIGRATION_FILES_RX);
    const all_files = await readdir(dir);
    MIGRATION_LOGGER.trace(all_files);
    const migration_files = all_files.filter(e => e.match(MIGRATION_FILES_RX));
    MIGRATION_LOGGER.trace(migration_files);
    const target_files = migration_files.filter((e) => {
        const file_timestamp = parseInt(e.match(MIGRATION_FILES_RX)[1], 10);
        return file_timestamp >= first_timestamp && file_timestamp <= last_timestamp;
    });
    return target_files.sort((a, b) => {
        const a_timestamp = parseInt(a.match(MIGRATION_FILES_RX)[1], 10);
        const b_timestamp = parseInt(b.match(MIGRATION_FILES_RX)[1], 10);
        MIGRATION_LOGGER.trace(`timestamps: ${a_timestamp}, ${b_timestamp}`);
        return a_timestamp - b_timestamp;
    });
}

/* Load and instantiate the class defined in a migration file */
async function instantiate_migration(file) {
    MIGRATION_LOGGER.debug(`importing ${file}`);
    const imp = await import('./' + file);
    const cur_migration_class = Object.values(imp)[0];
    return {
        migration: new cur_migration_class(DB_POOL),
        migration_id: cur_migration_class.identifier
    }
}

/* Assuming the migrations DB table exists, retrieve the record for the most recently run migration */
async function retrieve_latest_migration_record(migration_store) {
    const retval = await migration_store.getMostRecentMigration();
    if (!retval) {
        throw new Error('Failed to retrieve latest migration record. Aborting.');
    }
    return retval;
 }

/* Given a migration id (timestamp), see if this migration has already been run, which is
 * determined by seeing if the DB has a record for it */
 async function migration_already_run(migration_store, migration_id) {
     const res = await migration_store.getMigrationByMigrationId(migration_id);
     return res ? true : false;
 }

/* The 'main' function that runs all the indicated migrations */
async function run_migrations(target_files, db_pool, migration_store, one_big_tx=true) {
    let start;
    let end;
    let migration_record;

    if (one_big_tx) {
        await pgExec(db_pool, 'BEGIN');
        MIGRATION_LOGGER.info('Beginning big tx');
    }
    try {
        for (const f of target_files) {
            const {migration, migration_id} = await instantiate_migration(f);
            // This check may be superfluous but you can't be too careful, right?
            const alreadyRun = await migration_already_run(migration_store, migration_id);
            if (alreadyRun) {
                throw new Error(`Migration already run: ${migration_id}. Aborting.`);
            }

            MIGRATION_LOGGER.info(`Executing ${migration_id}`);
            start = new Date();
            if (!one_big_tx) {
                await pgExec(db_pool, 'BEGIN')
                MIGRATION_LOGGER.info(`Beginning transaction for ${migration_id}`);
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
            migration_record = new Migration({id: null, migration_id: migration_id, time_begun: start, time_complete: end,
                run_id: RUN_ID, message: migration.successMessage()});
            res = await migration_store.saveMigrationRecord(migration_record);
            if (!res) {
                throw new Error(`Could not save migration record for ${migration_id}: `
                    + `${JSON.stringify(migration_record)}. Aborting`);
            }

            if (!one_big_tx) {
                await pgExec(db_pool, 'COMMIT');
                MIGRATION_LOGGER.info(`Committed transactions Tx for ${migration_id}`);
            }
        }
    } catch (err) {
        MIGRATION_LOGGER.error(err, "Unexpected exception. Rolling back.");
        const rb = await pgExec(db_pool, 'ROLLBACK');
        throw err;
    }
    if (one_big_tx) {
        await pgExec(db_pool, 'COMMIT');
        MIGRATION_LOGGER.info("Completed big tx");
    }
}

// Begin executable code

// Load config
const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
MIGRATION_LOGGER.trace(CONFIG);
const file_first = cli.flags.first;
const file_last = cli.flags.last;

if (file_first > file_last) {
    throw new Error(`Specified range doesn't make sense (first > last). Recheck your args. Aborting.`);
}

// Open DB connection
const DB_POOL = new pg.Pool({
    ...CONFIG.storage.pg,
    password: CONFIG.secrets.pg.password,
    ssl: CONFIG.db_conn.ssl
  });
MIGRATION_LOGGER.trace(DB_POOL);
const migrationStore = new MigrationStorePostgres(DB_POOL);

let target_files = [];

// Cue a ton of sanity checks
const migration_table_status = await migrationStore.getMigrationTableStatus();
if (!migration_table_status || !migration_table_status.exists) {
    throw new Error(`Migrations table does not exist in DB. Aborting.`);
}
if (migration_table_status.n_rows > 0) {
    const latest_migration = await retrieve_latest_migration_record(migrationStore);
    if (!latest_migration) {
        throw new Error('Error retrieving latest migration. Aborting.');
    }
    const latest_db_migration_id = latest_migration.migration_id;

    if (file_first === DEFAULT_FIRST_MIGRATION_TIMESTAMP) {
        // If no explicit start arg was given to the program, then by definition this means use the DB value as the starting point
        target_files = await get_migration_files(MIGRATIONS_DIR, latest_db_migration_id + 1, file_last);
    } else {
        // If an explicit first file was provided, then two checks are necessary:
        //  First, it must not predate the most recent run.
        if (file_first <= latest_db_migration_id) {
            throw new Error(`The proposed starting point (${file_first}) is not later than `
                + `the most recent run recorded in the DB (${latest_db_migration_id}). Aborting.`);
        }
        // Second, it must not skip ahead past any migrations that exist as files, unless this is explicitly requested.
        const db_relative_migrations = await get_migration_files(MIGRATIONS_DIR, latest_db_migration_id + 1, file_last);
        target_files = await get_migration_files(MIGRATIONS_DIR, file_first, file_last);
        if (db_relative_migrations[0] !== target_files[0] && !cli.flags.forceDangerousThings) {
            throw new Error(`The specified starting point ${target_files[0]} skips some migrations, `
                + `starting with ${db_relative_migrations[0]}. Recheck your args. If you really mean to do this, `
                + `rerun with --force-dangerous-things.`);
        }
    }
} else {
    // If the migration table contains no rows, then insist on an explicit starting point instead of the default arg
    if (file_first === DEFAULT_FIRST_MIGRATION_TIMESTAMP) {
        throw new Error(`Migrations table is empty. In this case, you must specify an explicit migration to start at. Aborting.`);
    } else {
        target_files = await get_migration_files(MIGRATIONS_DIR, file_first, file_last);
    }
}
if (target_files.length === 0) {
    throw new Error(`No migration files found that match input params. Aborting.`);
}

// If we got this far, we're ready to run migrations!
MIGRATION_LOGGER.info(`Sanity checks passed. Proceeding with running ${target_files.length} migration(s).`);
MIGRATION_LOGGER.debug(target_files);

await run_migrations(target_files, DB_POOL, migrationStore, cli.flags.oneBigTx);
MIGRATION_LOGGER.info('Completed migration run.');
DB_POOL.end();
