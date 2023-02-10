export function isValidId(id)
{
  return id.startsWith('PMID') ||
         id.startsWith('NCT') ||
         id.startsWith('clinicaltrialsNCT') ||
         id.startsWith('DOI');
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

    return 'Unknown';
  }

  function pmidToUrl(id)
  {
    return taggedIdToUrl(id, 'https://pubmed.ncbi.nlm.nih.gov');
  }

  function nctidToUrl(id)
  {
    return `https://clinicaltrials.gov/ct2/show/${id}`;
  }

  function doiidToUrl(id)
  {
    return taggedIdToUrl(id, 'https://www.doi.org');
  }

  function hasTag(id, tag)
  {
    return id.startsWith(tag);
  }

  if (hasTag(id, 'PMID'))
  {
    return ['PMID', pmidToUrl(id)];
  }
  else if (hasTag(id, 'NCT'))
  {
    return ['NCT', nctidToUrl(id)];
  }
  else if (hasTag(id, 'clinicaltrialsNCT'))
  {
    return ['NCT', nctidToUrl(id.split('clinicaltrials')[1])];
  }
  else if (hasTag(id, 'DOI'))
  {
    return ['DOI', doiidToUrl(id)];
  }
  else
  {
    return ['Unknown', 'Unknown'];
  }
}
