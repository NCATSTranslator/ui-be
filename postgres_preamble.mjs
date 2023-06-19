'use strict';
// Import statement is kinda weird because of some commonJS stuff happening in 'pg'.
// This worked after a bit of trial and error.
import pg from 'pg';

export { pg };

/* Technically unsafe due to integer precision in node but probably fine in reality.
 * Allows count() results to be returned as numbers instead of strings.
 *https://github.com/brianc/node-pg-types#use
 */
pg.types.setTypeParser(pg.types.builtins.INT8, parseInt);
