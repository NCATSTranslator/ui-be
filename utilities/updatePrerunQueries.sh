#!/bin/sh

cd "$(dirname $0)/node"
node ./autoGeneratePrerunQueries.mjs 1> /dev/null
