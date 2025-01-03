'use strict';

import { readdir } from 'node:fs/promises';
import path from 'path';
import { bootstrapConfig } from '../../lib/config.mjs';
import { pg } from '../../lib/postgres_preamble.mjs';
import { logger } from '../../lib/logger.mjs';
import meow from 'meow';

export { getMigrationFiles };

logger.level = 'info';

const program_name = path.basename(process.argv[1]);
const cli = meow(`
    Usage:
    $ node ${program_name} <options>

    Options:
      --config-file, -c: required
      --local-overrides, -l: optional
      --one-big-tx, -b: boolean
      --force-dangerous-things: true
      --start-at: number, optional
      --stop-after: number, optional
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

logger.info(cli.flags);



async function getMigrationFiles(dir, first_timestamp=0, last_timestamp=Infinity, suffix='.mjs') {
    let match_rx = new RegExp(`^(\\d+)\\..*${suffix}\$`); // Note doubled \\
    logger.info(match_rx);
    let all_files = await readdir(dir);
    logger.debug(all_files);
    let migration_files = all_files.filter(e => e.match(match_rx));
    logger.debug(migration_files);
    let target_files = migration_files.filter((e) => {
        let file_timestamp = parseInt(e.match(match_rx)[1], 10);
        return file_timestamp >= first_timestamp && file_timestamp <= last_timestamp;
    });
    return target_files.sort((a, b) => {
        let a_timestamp = parseInt(a.match(match_rx)[1], 10);
        let b_timestamp = parseInt(b.match(match_rx)[1], 10);
        logger.trace(`timestamps: ${a_timestamp}, ${b_timestamp}`);
        return a_timestamp - b_timestamp;
    });
}

async function update_migrations_table(applied_migrations) {

}


const CONFIG = await bootstrapConfig(cli.flags.configFile, cli.flags.localOverrides ?? null);
logger.debug(CONFIG);

const dbPool = new pg.Pool({
  ...CONFIG.storage.pg,
  password: CONFIG.secrets.pg.password,
  ssl: CONFIG.db_conn.ssl
});



logger.trace(dbPool);

let target = await getMigrationFiles('utilities/migrations',
    cli.flags.first ?? 0, cli.flags.last ?? Infinity,
    '.mjs');
logger.info(target);

for (let migration of target) {
    console.log(`loading ${migration}`);
    let imp = await import('./' + migration);
    let cur_migration_class = Object.values(imp)[0];
    let cur_migration = new cur_migration_class(dbPool);
    console.log(`i ran: ${cur_migration_class.identifier}`);
}