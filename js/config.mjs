'use strict'

import * as cmn from './common.mjs';

const argvs = process.argv;
let configPath = (argvs.length < 3) ? 'configurations/full-mock.json' : argvs[2];
export const SERVER_CONFIG = await cmn.readJson(configPath);

if (!SERVER_CONFIG['document-root'])
{
  SERVER_CONFIG['document-root'] = process.cwd();
}
