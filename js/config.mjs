'use strict'

import * as cmn from './common.mjs';

export { loadConfigFromFile };

async function loadConfigFromFile(filePath) {
  console.log(filePath);
  let config = cmn.readJson(filePath);
  if (!config['document-root'])
  {
    config['document-root'] = process.cwd();
  }
  if (config['canonicalization_priorty'])
  {
    config['canonicalization_priorty'] = await cmn.readJson(config['canonicalization_priority']);
  }

  return config;
}
