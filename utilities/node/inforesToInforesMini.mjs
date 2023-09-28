'use strict'

import * as cmn from '../../common.mjs';

function cleanupKnowledgeLevel(rawKL) {
  if (rawKL === 'curated') {
    return 'trusted';
  } else if (rawKL === 'correlation' ||
             rawKL === 'predicated' ||
             rawKL === 'prediction' ||
             rawKL === 'observation') {
    return 'inferred';
  } else if (rawKL === 'text_mined') {
    return 'ml';
  } else {
    return 'unknown';
  }
}

const filePath = process.argv[2];
const inforesCatalog = await cmn.readJson(filePath);
const inforesEntries = inforesCatalog.information_resources;
const inforesMini = {};
inforesEntries.forEach((inforesEntry) => {
  if (inforesEntry.id && inforesEntry.name && inforesEntry.xref && inforesEntry.xref.length > 0)
  {
    inforesMini[inforesEntry.id] = {
      name: inforesEntry.name,
      url: inforesEntry.xref[0],
      knowledge_level: cleanupKnowledgeLevel(inforesEntry['knowledge level'])
    };
  }
});

console.log(JSON.stringify(inforesMini));