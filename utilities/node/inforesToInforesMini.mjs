'use strict'

import * as cmn from '../../lib/common.mjs';

function cleanupKnowledgeLevel(rawKL) {
  if (rawKL === 'curated') {
    return 'trusted';
  } else if (rawKL === 'correlation' ||
             rawKL === 'predicted' ||
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
const patchPath = process.argv[3];
const inforesCatalog = await cmn.readJson(filePath);
const inforesEntries = inforesCatalog.information_resources;
const inforesMini = {};
inforesEntries.forEach((inforesEntry) => {
  if (inforesEntry.id) {
    inforesMini[inforesEntry.id] = {
      name: inforesEntry.name || inforesEntry.id,
      wiki: (inforesEntry.xref && inforesEntry.xref.length > 0) ? inforesEntry.xref[0] : null,
      knowledge_level: cleanupKnowledgeLevel(inforesEntry['knowledge level'])
    };
  }
});

if (patchPath) {
  const patchFile = await cmn.readJson(patchPath);
  for (const entryPatch of patchFile) {
    const entry = inforesMini[entryPatch.id];
    if (entry) {
      for (const key in entryPatch) {
        if (key === 'id') continue;
        entry[key] = entryPatch[key];
      }
    }
  }
}

console.log(JSON.stringify(inforesMini));
