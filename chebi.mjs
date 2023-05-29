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