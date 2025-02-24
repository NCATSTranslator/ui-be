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
migration_logger.level = 'info';

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
            },
            logLevel: {
                type: 'string',
                choices: ['trace', 'debug', 'info'],
                default: 'info',
                isRequired: false
            }

        }
    }
);

migration_logger.info({flags: cli.flags});
migration_logger.level = cli.flags.logLevel;

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

// assume the table exists
async function retrieveLatestMigrationRecord(migrationStore) {
    let retval = await migrationStore.getMostRecentMigration();
    if (!retval) {
        throw new Error('Failed to retrieve latest migration record. Aborting.');
    }
    return retval;
 }


 async function migrationAlreadyRun(migrationStore, migrationId) {
     const res = await migrationStore.getMigrationByMigrationId(migrationId);
     return res ? true : false;
 }

async function runMigrations(targetFiles, dbPool, migrationStore, oneBigTx=true) {
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
            // This check may be superfluous but you can't be too careful, right?
            let alreadyRun = await migrationAlreadyRun(migrationStore, migration_id);
            if (alreadyRun) {
                throw new Error(`Migration already run: ${migration_id}. Aborting.`);
            }

            migration_logger.info(`Executing ${migration_id}`);
            start = new Date();
            if (!oneBigTx) {
                await pgExec(dbPool, 'BEGIN')
                migration_logger.info(`Beginning transaction for ${migration_id}`);
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
        migration_logger.error(err, "Unexpected exception. Rolling back.");
        let rb = await pgExec(dbPool, 'ROLLBACK');
        throw err;
    }
    if (oneBigTx) {
        await pgExec(dbPool, 'COMMIT');
        migration_logger.info("Completed big tx");
    }
}

const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
migration_logger.trace(CONFIG);
let file_first = cli.flags.first;
let file_last = cli.flags.last;

if (file_first > file_last) {
    throw new Error(`Specified range doesn't make sense (first > last). Recheck your args. Aborting.`);
}

const dbPool = new pg.Pool({
    ...CONFIG.storage.pg,
    password: CONFIG.secrets.pg.password,
    ssl: CONFIG.db_conn.ssl
  });
migration_logger.trace(dbPool);
const migrationStore = new MigrationStorePostgres(dbPool);

let target_files = [];

// Cue a ton of sanity checks
let migration_table_status = await migrationStore.getMigrationTableStatus();
if (!migration_table_status || !migration_table_status.exists) {
    throw new Error(`Migrations table does not exist in DB. Aborting.`);
}
if (migration_table_status.n_rows > 0) {
    let latest_migration = await retrieveLatestMigrationRecord(migrationStore);
    if (!latest_migration) {
        throw new Error('Error retrieving latest migration. Aborting.');
    }
    let latest_db_migration_id = latest_migration.migration_id;

    if (file_first === 0) {
        // If no explicit start arg was given to the program, then by definition this means use the DB value as the starting point
        target_files = await getMigrationFiles(MIGRATIONS_DIR, latest_db_migration_id + 1, file_last);
    } else {
        // If an explicit first file was provided, then two checks are necessary:
        //  First, it must not predate the most recent run.
        if (file_first <= latest_db_migration_id) {
            throw new Error(`The proposed starting point (${file_first}) is not later than `
                + `the most recent run recorded in the DB (${latest_db_migration_id}). Aborting.`);
        }
        // Second, it must not skip ahead past any migrations that exist as files, unless this is explicitly requested.
        let db_relative_migrations = await getMigrationFiles(MIGRATIONS_DIR, latest_db_migration_id + 1, file_last);
        target_files = await getMigrationFiles(MIGRATIONS_DIR, file_first, file_last);
        if (db_relative_migrations[0] !== target_files[0] && !cli.flags.forceDangerousThings) {
            throw new Error(`The specified starting point ${target_files[0]} skips some migrations, `
                + `starting with ${db_relative_migrations[0]}. Recheck your args. If you really mean to do this, `
                + `rerun with --force-dangerous-things.`);
        }
    }
} else {
    // If the migration table contains no rows, then insist on an explicit starting point instead of the default arg
    if (file_first === 0) {
        throw new Error(`Migrations table is empty. In this case, you must specify an explicit migration to start at. Aborting.`);
    } else {
        target_files = await getMigrationFiles(MIGRATIONS_DIR, file_first, file_last);
    }
}
if (target_files.length === 0) {
    throw new Error(`No migration files found that match input params. Aborting.`);
}

// If we got this far, we're ready to run migrations!
migration_logger.info(`Sanity checks passed. Proceeding with running ${target_files.length} migration(s).`);
migration_logger.debug(target_files);

await runMigrations(target_files, dbPool, migrationStore, cli.flags.oneBigTx);
migration_logger.info('Completed migration run.');
dbPool.end();
