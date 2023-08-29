'use strict'

import * as cmn from './common.mjs';

export { loadConfigFromFile, postProcessConfig };


async function loadConfigFromFile(filePath) {
  console.log(filePath);
  let config = await cmn.readJson(filePath);
  if (!config['document-root'])
  {
    config['document-root'] = process.cwd();
  }

  for (let k of Object.keys(config)) {
    if (k.match(/^_load_/)) {
      await loadAndReplace(config, k);
    }
  }
  return config;
}

// Hack city
function postProcessConfig(config) {
  // Put the host specified at top-level config into the pg-specific object
  config.storage.pg.host = config.pg_host;
}

async function loadAndReplace(config, prop)
{
  if (config[prop])
  {
    config[prop.replace(/^_load_/, '')] = await cmn.readJson(config[prop]);
  }
}

async function loadAndReplace2(config, prop)
{
  if (config[prop])
  {
    config[prop] = await cmn.readJson(config[prop]);
  }
}
