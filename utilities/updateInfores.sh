#!/bin/sh

infores_catalog=$1

curl -X 'GET' "https://raw.githubusercontent.com/biolink/biolink-model/master/infores_catalog.yaml" > 'tmp.yaml'
yq -o=json 'tmp.yaml' > 'tmp.json'
node ./node/inforesToInforesMini.mjs 'tmp.json' > "${infores_catalog}"
rm tmp.yaml tmp.json
