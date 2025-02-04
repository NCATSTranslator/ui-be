'use strict'

import * as cmn from './common.mjs';
import * as chebi from './chebi.mjs';

export function getFdaApproval(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    getChemicalFdaApproval,
    genDefaultHandler());
}

export function getDescription(annotation) {
  return parseAnnotation(
    annotation,
    getDiseaseDescription,
    getChemicalDescription,
    getGeneDescription);
}

export function getChebiRoles(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    getChemicalChebiRoles,
    genDefaultHandler());
}

export function getDrugIndications(annotation) {
  const defaultHandler = genDefaultHandler([]);
  return parseAnnotation(
    annotation,
    defaultHandler,
    getChemicalDrugIndications,
    defaultHandler,
    defaultHandler);
}

export function getCuries(annotation) {
  const defaultHandler = genDefaultHandler([]);
  return parseAnnotation(
    annotation,
    getDiseaseMeshCuries,
    defaultHandler,
    defaultHandler,
    defaultHandler);
}

export function getNames(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    getChemicalNames,
    genDefaultHandler());
}

export function getOtc(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    getChemicalOtcStatus,
    genDefaultHandler());
}

export function getSpecies(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    genDefaultHandler(),
    getGeneSpecies);
}

export function getTdl(annotation) {
  return parseAnnotation(
    annotation,
    genDefaultHandler(),
    genDefaultHandler(),
    getGeneTdl);
}

function parseAnnotation(annotation, diseaseHandler, chemicalHandler, geneHandler, defaultHandler = genDefaultHandler()) {
  if (isDisease(annotation)) {
    return diseaseHandler(annotation);
  }

  if (isChemical(annotation)) {
    return chemicalHandler(annotation);
  }

  if (isGene(annotation)) {
    return geneHandler(annotation);
  }

  return defaultHandler();
}

function getDiseaseDescription(annotation) {
  let description = cmn.jsonGetFromKpath(annotation, ['disease_ontology', 'def'], null);
  if (description !== null) {
    description = description.split('[')[0];
  }

  return description;
}

function getDiseaseMeshCuries(annotation) {
  const paths = [
    ['mondo', 'xrefs', 'mesh'],
    ['disease_ontology', 'xrefs', 'mesh']
  ];
  const curies = [];
  for (const path of paths) {
    const curie = cmn.jsonGetFromKpath(annotation, path, null);
    if (curie) {
      curies.push(`MESH:${curie}`);
    }
  }
  return curies;
}

function isDisease(annotation) {
  return annotation['disease_ontology'] !== undefined;
}

function getChemicalNames(annotation) {
  const ndc = cmn.jsonGetFromKpath(annotation, ['ndc'], []);
  const commercialNames = new Set();
  const genericNames = new Set();
  for (const entry of ndc) {
    let commercialName = cmn.jsonGet(entry, 'proprietaryname', null);
    let genericName = cmn.jsonGet(entry, 'nonproprietaryname', null);
    if (commercialName !== null) {
      commercialNames.add(commercialName.toLowerCase());
    }

    if(genericName !== null) {
      genericNames.add(genericName.toLowerCase());
    }
  }

  return {
    commercial: [...commercialNames],
    generic: [...genericNames]
  };
}

function getChemicalDescription(annotation) {
  let description = cmn.jsonGetFromKpath(annotation, ['unii', 'ncit_description'], null);
  if (description === null) {
    description = cmn.jsonGetFromKpath(annotation, ['chebi', 'definition'], null);
  }
  return description;
}

function getChemicalChebiRoles(annotation) {
  let ids = cmn.jsonGetFromKpath(annotation, ['chebi', 'relationship', 'has_role'], null);
  if (ids === null) {
    return null;
  }

  if (!cmn.isArray(ids)) {
    ids = [ids];
  }

  const roles = [];
  ids.forEach((id) => {
    const highLevelId = chebi.getHighLevelRole(id);
    if (highLevelId !== null) {
      roles.push({ id: highLevelId, name: chebi.getName(highLevelId) });
    }
  });

  return roles;
}

function getChemicalFdaApproval(annotation) {
  const fdaApproved = cmn.jsonGetFromKpath(annotation, ['chembl', 'max_phase'], 0);
  return fdaApproved;
}

function getChemicalDrugIndications(annotation) {
  const indicationObjs = cmn.jsonGetFromKpath(annotation, ['chembl', 'drug_indications'], []);
  const indications = [];
  indicationObjs.forEach((obj) => {
    const id = cmn.jsonGet(obj, 'mesh_id', false);
    if (id) {
      indications.push(id);
    }
  });

  return indications;
}

function getChemicalOtcStatus(annotation) {
  const otcStatus = cmn.jsonGetFromKpath(annotation, ['chembl', 'availability_type'], -1);
  switch (otcStatus) {
    case 2:
      return {id: 't', name: 'Over the counter'};
    case 1:
      return {id: 'f', name: 'Prescription only'};
    case 0:
      return {id: 'd', name: 'Discontinued'};
    case -2:
      return {id: 'w', name: 'Withdrawn'};
    default:
      return {id: 'o', name: 'Other'};
  }
}

function isChemical(annotation) {
  return annotation.chebi !== undefined ||
         annotation.chembl !== undefined ||
         annotation.ndc !== undefined;
}

function getGeneDescription(annotation) {
  return cmn.jsonGet(annotation, 'summary', null);
}

function getGeneSpecies(annotation) {
  const speciesId = cmn.jsonGet(annotation, 'taxid', null);
  return speciesIdToSpecies(speciesId);
}

function getGeneTdl(annotation) {
  const tdl = cmn.jsonGetFromKpath(annotation, ['pharos', 'tdl'], null);
  return tdl;
}

function isGene(annotation) {
  return annotation.symbol !== undefined;
}

function genDefaultHandler(fallback = null) {
  return function() { return fallback };
}

function speciesIdToSpecies(id) {
  switch(id) {
    case 7955: return 'Zebrafish';
    case 10090: return 'Mouse';
    case 10116: return 'Rat';
    default: return null;
  }
}
