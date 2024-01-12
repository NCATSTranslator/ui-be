#!/bin/sh
version=$1
if [ -z "${version}" ]; then
  version="master"
fi
echo 'Downloading biolink model'
wget -O 'tmp.yaml' "https://raw.githubusercontent.com/biolink/biolink-model/${version}/biolink-model.yaml"
echo 'Converting from YAML to JSON'
yq -o=json 'tmp.yaml' > 'biolink-model.json'
rm 'tmp.yaml'
echo 'Done'
