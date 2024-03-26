#!/bin/sh

go_terms=$1
if [ -z $go_terms ]; then
  go_terms="go_terms.json"
fi

curl -X 'GET' https://current.geneontology.org/ontology/go-basic.obo > tmp.obo
node ./node/oboToJson.mjs 'tmp.obo' 'molecular_function' > "${go_terms}"
rm tmp.obo
