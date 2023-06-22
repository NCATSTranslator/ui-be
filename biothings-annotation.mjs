'use strict'

import * as cmn from './common.mjs';
import * as chebi from './chebi.mjs';

export function getFdaApproval(annotation)
{
  return parseAnnotation(
    annotation,
    noHandler,
    getChemicalFdaApproval,
    noHandler);
}

export function getChebiRoles(annotation)
{
  return parseAnnotation(
    annotation,
    noHandler,
    getChemicalChebiRoles,
    noHandler);
}

function parseAnnotation(annotation, diseaseHandler, chemicalHandler, geneHandler, fallback = null)  
{
  annotation = annotation[0];
  if (isDisease(annotation)) 
  {
    return diseaseHandler(annotation);
  }

  if (isChemical(annotation))
  {
    return chemicalHandler(annotation);
  }

  if (isGene(annotation))
  {
    return geneHandler(annotation);
  } 

  return fallback;
}

function isDisease(annotation)
{
  return annotation.disease_ontology !== undefined;
}

function getChemicalChebiRoles(annotation)
{
  let ids = cmn.jsonGetFromKpath(annotation, ['chebi', 'relationship', 'has_role'], null);
  if (ids === null)
  {
    return null;
  }

  if (!cmn.isArray(ids))
  {
    ids = [ids];
  }

  const roles = [];
  ids.forEach((id) => {
    const highLevelId = chebi.getHighLevelRole(id);
    if (highLevelId !== null)
    {
      roles.push({ id: highLevelId, name: chebi.getName(highLevelId) });
    }
  });

  return roles;
}

function getChemicalFdaApproval(annotation)
{
  const fdaApproved = cmn.jsonGetFromKpath(annotation, ['chembl', 'max_phase'], 0);
  return fdaApproved;
}

function isChemical(annotation)
{
  return annotation.chebi !== undefined || annotation.chembl !== undefined;
}

function isGene(annotation)
{
  return annotation.symbol !== undefined;
}

function noHandler(annotation)
{
  return null;
}