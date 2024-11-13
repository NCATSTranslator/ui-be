'use strict';

import { logger } from "./logger.mjs";

export function isValidId(id)
{
  const [url, type] = idToTypeAndUrl(id);
  const isValid = !!url && !!type;
  if (!isValid) {
    logger.error(`Invalid id: ${id}`);
  }

  return isValid;
}

export function sanitize(id)
{
  if (id.startsWith('PMC:'))
  {
    return id.replace(':', '');
  }

  // Sometimes we get a clinicaltrials link instead of an id
  if (id.startsWith('https://clinicaltrials.gov/'))
  {
    const nctId = id.match(/NCT\d{8}/)[0];
    return `clinicaltrials:${nctId}`;
  }

  if (id.startsWith('http:'))
  {
    return id.replace('http:', 'https:');
  }

  return id;
}

export function idToTypeAndUrl(id)
{
  function stripTag(id)
  {
    const strippedId = id.split(':');
    if (strippedId.length > 1)
    {
      return strippedId[1];
    }

    return false;
  }

  function taggedIdToUrl(id, url)
  {
    const strippedId = stripTag(id);
    if (!!strippedId)
    {
      return `${url}/${strippedId}`;
    }

    return false;
  }

  function pmidToUrl(id)
  {
    return taggedIdToUrl(id, 'http://www.ncbi.nlm.nih.gov/pubmed');
  }

  function pmcidToUrl(id)
  {
    return taggedIdToUrl(id, 'http://pmc.ncbi.nlm.nih.gov/articles/');
  }

  function pmcToUrl(id)
  {
    return `https://pmc.ncbi.nlm.nih.gov/articles/${id}`;
  }

  function nctidToUrl(id)
  {
    return taggedIdToUrl(id, 'https://clinicaltrials.gov/ct2/show');
  }

  function doiidToUrl(id)
  {
    return taggedIdToUrl(id, 'https://www.doi.org');
  }

  function isbnToUrl(id)
  {
    return taggedIdToUrl(id, 'https://www.isbn-international.org/identifier');
  }

  function hasTag(id, tag)
  {
    return id.startsWith(tag);
  }

  function isUrl(id)
  {
    return hasTag(id, 'http') || hasTag(id, 'https');
  }

  if (hasTag(id, 'PMID'))
  {
    return ['PMID', pmidToUrl(id)];
  }
  else if (hasTag(id, 'PMCID'))
  {
    return ['PMC', pmcidToUrl(id)];
  }
  else if (hasTag(id, 'PMC'))
  {
    return ['PMC', pmcToUrl(id)];
  }
  else if (hasTag(id, 'clinicaltrials'))
  {
    return ['NCT', nctidToUrl(id)];
  }
  else if (hasTag(id, 'doi'))
  {
    return ['doi', doiidToUrl(id)];
  }
  else if (hasTag(id, 'isbn'))
  {
    return ['other', isbnToUrl(id)];
  }
  else if (isUrl(id))
  {
    return ['other', id];
  }
  else
  {
    return [false, false];
  }
}
