#!/bin/sh
echo 'Downloading biolink model'
wget -O 'tmp.yaml' 'https://raw.githubusercontent.com/biolink/biolink-model/master/biolink-model.yaml'
echo 'Converting from YAML to JSON'
yq -o=json 'tmp.yaml' > 'biolink-model.json'
rm 'tmp.yaml'
echo 'Done'