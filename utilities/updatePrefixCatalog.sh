#!/bin/sh

prefix_catalog=$1
version=$2
if [ -z "${version}" ]; then
  version='master'
fi

curl -X 'GET' "https://raw.githubusercontent.com/biolink/biolink-model/${version}/prefix-map/biolink-model-prefix-map.json" > "${prefix_catalog}"
