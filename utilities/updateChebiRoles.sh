#!/bin/sh

chebi_roles=$1
if [ -z "${chebi_roles}" ]; then
  chebi_roles="chebi_roles.json"
fi

curl -X 'GET' https://ftp.ebi.ac.uk/pub/databases/chebi/ontology/chebi_lite.obo > tmp.obo
node ./node/oboToJson.mjs tmp.obo term > "${chebi_roles}"
rm tmp.obo
