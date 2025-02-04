#!/bin/sh

version=$1
patch=$2
if [ -z "${version}" ]; then
  version='main'
fi

curl -X 'GET' "https://raw.githubusercontent.com/biolink/information-resource-registry/${version}/infores_catalog.yaml" > 'tmp.yaml'
yq -o=json 'tmp.yaml' > 'tmp.json'
node ./node/inforesToInforesMini.mjs 'tmp.json' $2 > "infores-catalog-${version}.json"
rm tmp.yaml tmp.json
