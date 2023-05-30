'use strict'

import * as cmn from './common.mjs';

export { loadConfigFromFile };

async function loadConfigFromFile(filePath) {
  console.log(filePath);
  let config = await cmn.readJson(filePath);
  if (!config['document-root'])
  {
    config['document-root'] = process.cwd();
  }

  await loadAndReplace(config, 'canonicalization_priority');
  await loadAndReplace(config, 'frontend');
  await loadAndReplace(config, 'ara_to_infores_map');

  return config;
}

async function loadAndReplace(config, prop)
{
  if (config[prop])
  {
    config[prop] = await cmn.readJson(config[prop]);
  }
}
