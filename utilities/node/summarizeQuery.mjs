'use strict'

/**
 * Reads in a file from the command line and runs it through the TRAPI summarization
 * NOTE: This can only be run from the root directory because of the relative file structure assumption in our config files
 */

import * as cfg from '../../lib/config.mjs'
import { loadBiolink } from '../../lib/biolink-model.mjs';
import { loadChebi } from '../../lib/chebi.mjs';
import { TranslatorServicexFEAdapter } from '../../adapters/TranslatorServicexFEAdapter.mjs'
import { readJson } from '../../lib/common.mjs';
import { load_trapi } from '../../lib/trapi/core.mjs';

// TODO: config shit
const configPath = process.argv[2];
const dataPath = process.argv[3];
const maxHops = 3;
const translatorAdapter = new TranslatorServicexFEAdapter();
readJson(dataPath).then(async (data) => {
  const config = await cfg.bootstrapConfig(configPath);
  await loadBiolink(config.biolink);
  await loadChebi();
  load_trapi(config.trapi);
  const summaryMsg = await translatorAdapter.queryResultsToFE(data, maxHops);
  console.log(JSON.stringify(summaryMsg.data));
});
