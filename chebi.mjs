'use strict'

import * as cmn from './common.mjs';

let CHEBI_ROLES = null;

export async function loadChebi() {
  CHEBI_ROLES = await cmn.readJson('./assets/chebi/chebi_roles.json');
}

export function getName(chebiId) {
  return CHEBI_ROLES[chebiId].name;
}

export function getParent(chebiId) {
  return CHEBI_ROLES[chebiId].is_a;
}

export function getHighLevelRole(chebiId) {
  let parentOfParent = getParent(getParent(chebiId));
  if (parentOfParent === undefined) {
    return null;
  }

  while (!rootNode(parentOfParent)) {
    chebiId = getParent(chebiId);
    parentOfParent = getParent(parentOfParent);
  }

  return chebiId;
}

function rootNode(chebiId) {
  return chebiId === 'CHEBI:50906';
}