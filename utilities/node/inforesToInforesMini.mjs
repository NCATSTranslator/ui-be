'use strict'

import * as cmn from '../../common.mjs';

const filePath = process.argv[2];
const inforesCatalog = await cmn.readJson(filePath);
const inforesEntries = inforesCatalog.information_resources;
const inforesMini = {};
inforesEntries.forEach((inforesEntry) => {
  if (inforesEntry.id && inforesEntry.name && inforesEntry.xref && inforesEntry.xref.length > 0)
  {
    inforesMini[inforesEntry.id] = {
      name: inforesEntry.name,
      url: inforesEntry.xref[0]
    };
  }
});

console.log(JSON.stringify(inforesMini));