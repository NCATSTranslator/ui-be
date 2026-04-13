'use strict'
import * as cmn from '../../lib/common.mjs';

const filePath = process.argv[2];
const patchPath = process.argv[3];
const inforesCatalog = await cmn.readJson(filePath);
const inforesEntries = inforesCatalog.information_resources;
const inforesMini = {};
inforesEntries.forEach((inforesEntry) => {
  if (inforesEntry.id) {
    let xref = inforesEntry.xref ?? null;
    let wiki = null;
    let url = null;
    if (xref && xref.length > 0) {
      xref = xref[0];
      if (xref.startsWith("https://github.com")) {
        wiki = xref;
      } else {
        url = xref;
      }
    }

    inforesMini[inforesEntry.id] = {
      name: inforesEntry.name || inforesEntry.id,
      wiki: wiki,
      url: url,
      knowledge_level: _cleanupKnowledgeLevel(inforesEntry['knowledge_level'])
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

function _cleanupKnowledgeLevel(rawKL) {
  if (rawKL === 'knowledge_assertion') {
    return 'trusted';
  } else if (rawKL === 'prediction' || rawKL === 'statistical_association') {
    return 'inferred';
  } else {
    return 'unknown';
  }
}

console.log(JSON.stringify(inforesMini));
