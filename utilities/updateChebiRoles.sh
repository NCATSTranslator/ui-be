#!/bin/sh

chebi_roles=$1
curl -X 'GET' https://ftp.ebi.ac.uk/pub/databases/chebi/ontology/chebi_lite.obo > tmp.obo
node ./node/generateChebiRoles.mjs tmp.obo > "${chebi_roles}"
rm tmp.obo