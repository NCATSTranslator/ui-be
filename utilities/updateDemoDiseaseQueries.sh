#!/bin/sh

cd "$(dirname $0)/node"
node ./autoGeneratePrerunQueries.mjs '../../assets/demo-diseases/queries.json' '../../assets/demo-diseases' 1> /dev/null
mv ../../assets/demo-diseases/production.json ../../configurations/demo-diseases.json
