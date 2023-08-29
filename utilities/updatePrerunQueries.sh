#!/bin/sh

cd "$(dirname $0)/node"
node ./autoGeneratePrerunQueries.mjs '../../configurations/frontend/prerun-queries.json' '../../configurations/frontend'
