'use strict'

import * as cmn from './common.mjs';

export { loadConfigFromFile };

async function loadConfigFromFile(filePath) {
  console.log(filePath);
  let retval = cmn.readJson(filePath);
  if (!retval['document-root']) {
    retval['document-root'] = process.cwd();
  }
  return retval;
}
