export {
  role_id_to_name
}

import * as cmn from './common.mjs';

let CHEBI_ROLES = null;
let CHEBI_ROOTS = null;
let CHEBI_EXCLUDES = null;

export async function loadChebi() {
  CHEBI_ROLES = await cmn.readJson('./assets/chebi/chebi_roles.json');
  CHEBI_ROOTS = await cmn.readJson('./assets/chebi/chebi_roots.json');
  CHEBI_EXCLUDES = await cmn.readJson('./assets/chebi/chebi_excludes.json');
}

function role_id_to_name(role_id) {
  return CHEBI_ROLES[role_id].name;
}

export function getParent(chebiId) {
  return CHEBI_ROLES[chebiId].is_a;
}

export function getHighLevelRole(chebiId) {
  if (isExcluded(chebiId)) return null;

  let parent = getParent(chebiId);
  if (parent === undefined) return null;

  while (!rootNode(parent)) {
    if (isExcluded(parent)) return null;

    chebiId = getParent(chebiId);
    parent = getParent(parent);
  }

  return chebiId;
}

function isExcluded(chebiId) {
  return CHEBI_EXCLUDES.includes(chebiId);
}

function rootNode(chebiId) {
  return CHEBI_ROOTS.includes(chebiId);
}
