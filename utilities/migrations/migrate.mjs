'use strict';

import { readdir } from 'node:fs/promises';
import path from 'path';
import { bootstrapConfig } from '../../lib/config.mjs';
import { pg, pgExec } from '../../lib/postgres_preamble.mjs';
import { logger } from '../../lib/logger.mjs';
import { v4 as uuidv4 } from 'uuid';
import meow from 'meow';

export { getMigrationFiles };

const MIGRATIONS_DIR = 'utilities/migrations';
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
    migration_logger.trace(match_rx);
    let all_files = await readdir(dir);
    migration_logger.trace(all_files);
    let migration_files = all_files.filter(e => e.match(match_rx));
    migration_logger.trace(migration_files);
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

async function instantiateMigration(file) {
    migration_logger.debug(`importing ${file}`);
    const imp = await import('./' + file);
    const cur_migration_class = Object.values(imp)[0];
    return {
        migration: new cur_migration_class(dbPool),
        migration_id: cur_migration_class.identifier
    }
}

async function biggy(targetFiles, dbPool, oneBigTx=true) {
    if (oneBigTx) {
        await pgExec(dbPool, 'BEGIN');
        migration_logger.info('Beginning big tx');
    }
    try {
        for (const f of targetFiles) {
            let {migration, migration_id} = await instantiateMigration(f);
            migration_logger.info(`Executing ${migration_id}`);
            if (!oneBigTx) {
                await pgExec(dbPool, 'BEGIN')
                migration_logger.info(`Beginning single Tx for ${migration_id}`);
            }
            let res = await migration.execute();
            if (!res) {
                throw new Error(`Migration ${migration_id} execute() returned false. Aborting`);
            }
            res = await migration.verify();
            if (!res) {
                throw new Error(`Migration ${migration_id} verification failed. Aborting.`)
            }
            if (!oneBigTx) {
                await pgExec(dbPool, 'COMMIT');
                migration_logger.info(`Committed single Tx for ${migration_id}`);
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
    migration_logger.info("yeah complete");

}

const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
migration_logger.trace(CONFIG);

let dbconfs = {
    ...CONFIG.storage.pg,
    password: CONFIG.secrets.pg.password,
    ssl: CONFIG.db_conn.ssl
  };
migration_logger.trace(dbconfs);
const dbPool = new pg.Pool(dbconfs);


migration_logger.trace(dbPool);

let target = await getMigrationFiles(MIGRATIONS_DIR,
    cli.flags.first ?? 0, cli.flags.last ?? Infinity,
    '.mjs');
await biggy(target, dbPool, cli.flags.oneBigTx);
throw 42;




async function update_migrations_table(applied_migrations) {

}

async function execMigrationMethod(migrationObj, methodName, args=null) {
    let retval = {
        success: false,
        err_data: null
    }

    let method = migrationObj[methodName];
    if (!method || typeof method !== 'function') {
        throw new Error(`Could not find method ${methodName} on provided object`);
    }

    try {
        retval.success = await method.apply(migrationObj, args);
    } catch (err) {
        retval = { success: false, err_data: err};
    }
    return retval;
}

function main() {
    if (ONE_BIG_TX) {
        // start transaction
    }
    for (f in migration_files) {
        try {
            m = instantiate_migration(m);
            if (!ONE_BIG_TX) {
                // begin trans
            }
            m.exec();
            if (!m.verify()) {
                throw ERROR;
            }
            RECORD();
            if (!ONE_BIG_TX) {
                // commit transaction
            }

        } catch (err) {
            // ROLLBACK TRANS
            // ATTEMPT TO RECORD ??
        }
    }
    if (ONE_BIG_TX) {
        // commit transaction
    }
}


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

    let res = await execMigrationMethod(cur_migration, 'execute');
    if (res.success) {
        migration_logger.info(`Total fucking success for ${cur_migration_id}`);
        // try the verification. if verification succeeds keep going.
        // if fails, try undo. Report on undo status, then abort
    } else {
        migration_logger.error({err_data: res}, `Total fucking disaster for ${cur_migration_id}: ${res.err_data.toString()}`);
    }
    if (!res.success) console.log(`aiiieeeee`);
    if (!res.success) console.log(res.err_data.toString());
    /**
    // Run the migration
    let execute_success = false;
    let execute_exception_data = null;
    migration_logger.info(`executing ${cur_migration_id}`);
    try {
        execute_success = await cur_migration.execute();
        // throw new Error('silly error');
    } catch (err) {
        migration_logger.error({error_str: err.toString(), error_obj: err}, `Error running execute() for ${cur_migration_class}`);                  execute_exception_data = err;
        execute_success = false;
        execute_exception_data = err;
    }

    if (!execute_success) {
        let remediate_info = { success: false, err_data: null};
        try {
            remediate_info.success = await cur_migration.undo();
        } catch (err) {
            migration_logger.error({error_str: err.toString(), error_obj: err}, `Error running undo() for ${cur_migration_class}`);
            remediate_info = { success: false, err_data: err};
        }

    } else {
        // run verification

    }


    // If the migrations failed, simply abort

    // If the verification fails, try the undo procedure, then abort

    // If verification succeeds, record the success
    * **/


}