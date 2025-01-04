'use strict';

import { readdir } from 'node:fs/promises';
import path from 'path';
import { bootstrapConfig } from '../../lib/config.mjs';
import { pg } from '../../lib/postgres_preamble.mjs';
import { logger } from '../../lib/logger.mjs';
import { v4 as uuidv4 } from 'uuid';
import meow from 'meow';

export { getMigrationFiles };

const run_id = uuidv4();
const migration_logger = logger.child({run_id: run_id});
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



async function getMigrationFiles(dir, first_timestamp=0, last_timestamp=Infinity, suffix='.mjs') {
    let match_rx = new RegExp(`^(\\d+)\\..*${suffix}\$`); // Note doubled \\
    migration_logger.info(match_rx);
    let all_files = await readdir(dir);
    migration_logger.debug(all_files);
    let migration_files = all_files.filter(e => e.match(match_rx));
    migration_logger.debug(migration_files);
    let target_files = migration_files.filter((e) => {
        let file_timestamp = parseInt(e.match(match_rx)[1], 10);
        return file_timestamp >= first_timestamp && file_timestamp <= last_timestamp;
    });
    return target_files.sort((a, b) => {
        let a_timestamp = parseInt(a.match(match_rx)[1], 10);
        let b_timestamp = parseInt(b.match(match_rx)[1], 10);
        migration_logger.trace(`timestamps: ${a_timestamp}, ${b_timestamp}`);
        return a_timestamp - b_timestamp;
    });
}

async function update_migrations_table(applied_migrations) {

}


const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
migration_logger.debug(CONFIG);

let dbconfs = {
    ...CONFIG.storage.pg,
    password: CONFIG.secrets.pg.password,
    ssl: CONFIG.db_conn.ssl
  };
migration_logger.debug(dbconfs);
const dbPool = new pg.Pool(dbconfs);


migration_logger.debug(dbPool);

let target = await getMigrationFiles('utilities/migrations',
    cli.flags.first ?? 0, cli.flags.last ?? Infinity,
    '.mjs');
migration_logger.info({target_migrations: target});

for (let migration of target) {
    let imp;
    // Load the migration file
    try {
        migration_logger.debug(`loading ${migration}`);
        imp = await import('./' + migration);
    } catch (err) {
        migration_logger.error({error_str: err.toString(), error_obj: err}, `Error importing ${migration}`);
        throw err;
    }
    // Instantiate the migration class
    let cur_migration_class, cur_migration, cur_migration_id;
    try {
        cur_migration_class = Object.values(imp)[0];
        migration_logger.debug({class: cur_migration_class});
        cur_migration = new cur_migration_class(dbPool);
        cur_migration_id = cur_migration_class.identifier;
    } catch (err) {
        migration_logger.error({error_str: err.toString(), error_obj: err}, `Error instantiating class for ${cur_migration_class}`);
        throw err;
    }
    // Run the migration
    let execute_result = false;
    migration_logger.info(`executing ${cur_migration_id}`);
    try {
        execute_result = await cur_migration.execute();
        console.log(`ok result: ${execute_result}`);
        // throw new Error('silly error');
    } catch (err) {
        migration_logger.error({error_str: err.toString(), error_obj: err}, `Error running execute() for ${cur_migration_id}`);
        throw err;
    }


    // If the migrations failed, simply abort

    // If the verification fails, try the undo procedure, then abort

    // If verification succeeds, record the success



}