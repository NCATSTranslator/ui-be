#!/bin/sh

pk_file=$1
config_file=$2
if [ -z "$config_file" ]; then
  config_file="./configurations/production.json"
fi

data_in="/tmp"
data_out="./test/data/llm-stress"
if [ -n "$pk_file" ]; then
  for pk in $(cat "${pk_file}"); do
    echo $pk
    node ./utilities/node/downloadQuery.mjs $pk > "$data_in/tmp.json"
    node ./utilities/node/summarizeQuery.mjs "${config_file}" "$data_in/tmp.json" > "$data_in/tmp2.json"
    node ./utilities/node/genLLMResultContext.mjs "$data_in/tmp2.json" > "$data_out/${pk}.json"
  done
fi
