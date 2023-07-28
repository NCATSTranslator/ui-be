#!/bin/sh

cd "$(dirname $0)/node"
node ./autoGeneratePrerunQueries.mjs '../../assets/gard/queries.json' '../../assets/gard' #1> /dev/null
mv ../../assets/gard/production.json ../../configurations/gard.json
