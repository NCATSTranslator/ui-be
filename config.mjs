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
  console.log(config);

  return config;
}

// Hack city
function postProcessConfig(config) {
  // Put the host specified at top-level config into the pg-specific object
  config.storage.pg.host = config.pg_host;
}

async function loadConfigFromFile2(filePath) {
  console.log(filePath);
  let config = await cmn.readJson(filePath);
  if (!config['document-root'])
  {
    config['document-root'] = process.cwd();
  }

  await loadAndReplace(config, 'canonicalization_priority');
  await loadAndReplace(config, 'frontend');
  await loadAndReplace(config, 'ara_to_infores_map');
  await loadAndReplace(config, 'auth');
  await loadAndReplace(config, 'sessions');
  await loadAndReplace(config, 'storage');
  await loadAndReplace(config, 'secrets');
  await loadAndReplace(config, 'demo_diseases');

  // post-processing
  // Put the host specified at top-level config into the pg-specific object
  config.storage.pg.host = config.pg_host;

  return config;
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
