#!/bin/sh

cd "$(dirname $0)/node"
for env in "$@"; do
  node ./generatePrerunQueries.mjs "$env" && node ./retainPKs.mjs "$env"
done
