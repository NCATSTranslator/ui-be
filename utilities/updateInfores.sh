#!/bin/sh

infores_catalog=$1
version=$2
if [ -z "${version}" ]; then
  version='master'
fi

curl -X 'GET' "https://raw.githubusercontent.com/biolink/information-resource-registry/${version}/infores_catalog.yaml" > 'tmp.yaml'
yq -o=json 'tmp.yaml' > 'tmp.json'
node ./node/inforesToInforesMini.mjs 'tmp.json' > "${infores_catalog}"
rm tmp.yaml tmp.json
