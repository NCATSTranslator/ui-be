#!/bin/sh

pk_file=$1
for pk in $(cat $pk_file); do
  echo $pk
  node ./utilities/node/downloadQuery.mjs $pk > "$pk.json"
done
