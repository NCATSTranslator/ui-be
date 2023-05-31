'use strict'

import * as cmn from './common.mjs';
import * as chebi from './chebi.mjs';

export function getName(annotation)
{
  return parseAnnotation(
    annotation,
    getDiseaseName,
    getChemicalName,
    getGeneName);
}

export function getDescription(annotation)
{
  return parseAnnotation(
    annotation,
    getDiseaseDescription,
    noHandler,
    getGeneDescription);
}

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

function getDiseaseName(annotation)
{
  return cmn.jsonGetFromKpath(annotation, ['disease_ontology', 'name'], null);
}

function getDiseaseDescription(annotation)
{
  return cmn.jsonGetFromKpath(annotation, ['disease_ontology', 'def'], null);
}

function isDisease(annotation)
{
  return annotation.disease_ontology !== undefined;
}

function getChemicalName(annotation)
{
  let name = cmn.jsonGetFromKpath(annotation, ['chebi', 'name'], null);

  if (name === null)
  {
    name = cmn.jsonGetFromKpath(annotation, ['chembl', 'pref_name'], null);
  }

  return name;
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
  return fdaApproved === 4;
}

function isChemical(annotation)
{
  return annotation.chebi !== undefined || annotation.chembl !== undefined;
}

function getGeneName(annotation)
{
  return cmn.jsonGet(annotation, 'symbol', null);
}

function getGeneDescription(annotation)
{
  return cmn.jsonGet(annotation, 'summary', null);
}

function isGene(annotation)
{
  return annotation.symbol !== undefined;
}

function noHandler(annotation)
{
  return null;
}