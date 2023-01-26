'use strict'

import * as cmn from './common.mjs';

const argvs = process.argv;
let configPath = (argvs.length < 3) ? 'configurations/mock.json' : argvs[2];
export const SERVER_CONFIG = await cmn.readJson(configPath);
SERVER_CONFIG['canonicalization_priority'] = await cmn.readJson(SERVER_CONFIG['canonicalization_priority']);

if (!SERVER_CONFIG.document_root)
{
  SERVER_CONFIG.document_root = process.cwd();
}
