'use strict';
// Import statement is kinda weird because of some commonJS stuff happening in 'pg'.
// This worked after a bit of trial and error.
import pg from 'pg';

import { logger } from './logger.mjs';
export { pg, pgExec, pgExecTrans };

/* Technically unsafe due to integer precision in node but probably fine in reality.
 * Allows count() results to be returned as numbers instead of strings.
 *https://github.com/brianc/node-pg-types#use
 */
pg.types.setTypeParser(pg.types.builtins.INT8, parseInt);

async function pgExec(pool, sql, sqlParams=[]) {
  let client = null;
  try {
    client = await pool.connect();
    const res = await client.query(sql, sqlParams);
    return res;
  } catch (err) {
    logger.error(err);
    throw err;
  } finally {
    if (client) {
      client.release();
    }
  }
}

async function pgExecTrans(pool, fun, ...args) {
  let client = null;
  try {
    client = await pool.connect();
    await client.query('BEGIN');
    const res = await fun(client, ...args);
    await client.query('COMMIT');
    return res;
  } catch (err) {
    logger.error(err);
    await client.query('ROLLBACK');
  } finally {
    if (client) {
      client.release();
    }
  }
}
