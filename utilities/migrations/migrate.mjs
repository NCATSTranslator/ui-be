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
            startAt: {
                type: 'number',
                isRequired: false
            },
            stopAfter: {
                type: 'number',
                isRequired: false
            }
        }
    }
);

logger.info(cli.flags);



async function getMigrationFiles(dir, after_timestamp=0, suffix='.mjs') {
    let match_rx = new RegExp(`^(\\d+)\\..*${suffix}\$`); // Note doubled \\
    logger.info(match_rx);
    let all_files = await readdir(dir);
    logger.debug(all_files);
    let migration_files = all_files.filter(e => e.match(match_rx));
    logger.debug(migration_files);
    let target_files = migration_files.filter((e) => {
        let file_timestamp = e.match(match_rx)[1];
        return parseInt(file_timestamp, 10) > after_timestamp;
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

let target = await getMigrationFiles('utilities/migrations', cli.flags.startAt, '.mjs');
logger.info(target);

