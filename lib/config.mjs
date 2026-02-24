'use strict'

import { overwriteObj, readJson } from './common.mjs';

export { bootstrapConfig };

async function bootstrapConfig(baseFile, overrideFile=null) {
  let config = await loadConfigFromFile(baseFile);
  if (overrideFile) {
    let overrides = await loadConfigFromFile(overrideFile);
    config = overwriteObj(config, overrides);
  }
  postProcessConfig(config);
  return config;
}

async function loadConfigFromFile(filePath) {
  let config = await readJson(filePath);
  if (!config.document_root) {
    config.document_root = process.cwd();
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
  return config;
}

async function loadAndReplace(config, prop) {
  if (config[prop]) {
    const path = config[prop];
    config[prop.replace(/^_load_/, '')] = await readJson(_resolve_path(config, path));
  }
}

function _resolve_path(config, path) {
  if (config.document_root) {
    return `${config.document_root}/${path}`;
  }
  return path;
}
