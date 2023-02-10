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
  if (config['canonicalization_priority'])
  {
    config['canonicalization_priority'] = await cmn.readJson(config['canonicalization_priority']);
  }

  return config;
}
