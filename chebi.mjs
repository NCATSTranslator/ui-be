'use strict'

import * as cmn from './common.mjs';

let CHEBI_ROLES = null;
let CHEBI_ROOTS = null;

export async function loadChebi() {
  CHEBI_ROLES = await cmn.readJson('./assets/chebi/chebi_roles.json');
  CHEBI_ROOTS = await cmn.readJson('./assets/chebi/chebi_roots.json');
}

export function getName(chebiId) {
  return CHEBI_ROLES[chebiId].name;
}

export function getParent(chebiId) {
  return CHEBI_ROLES[chebiId].is_a;
}

export function getHighLevelRole(chebiId) {
  let parent = getParent(chebiId);
  if (parent === undefined) {
    return null;
  }

  while (!rootNode(parent)) {
    chebiId = getParent(chebiId);
    parent = getParent(parent);
  }

  return chebiId;
}

function rootNode(chebiId) {
  return CHEBI_ROOTS.includes(chebiId);
}