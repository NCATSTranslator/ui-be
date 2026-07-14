'use strict'
import * as cmn from '../../lib/common.mjs';

const filePath = process.argv[2];
const patchPath = process.argv[3];
const inforesCatalog = await cmn.read_json(filePath);
const inforesEntries = inforesCatalog.information_resources;
const inforesMini = {};
inforesEntries.forEach((inforesEntry) => {
  if (inforesEntry.id) {
    let xrefs = inforesEntry.xref ?? null;
    let wiki = null;
    let url = null;
    if (xrefs) {
      for (let i = 0; i < xrefs.length; i++) {
        if (wiki !== null && url !== null) break;
        const xref = xrefs[i];
        const isWiki = xref.startsWith("https://github.com");
        if (wiki === null && isWiki) {
          wiki = xref;
        } else if (url === null && !isWiki) {
          url = xref;
        }
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
  const patchFile = await cmn.read_json(patchPath);
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
