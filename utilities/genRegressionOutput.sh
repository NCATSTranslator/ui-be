#!/bin/sh

config_file=$1
if [ -z "$config_file" ]; then
  config_file="./configurations/production.json"
fi

test_dir="./test/data/regression"
data_in="$test_dir/in"
data_out="$test_dir/out"
cd ..
mkdir -p $data_in $data_out
rm -f $data_out/*
for f in $(ls $data_in); do
  node ./utilities/node/summarizeQuery.mjs "${config_file}" "$data_in/${f}" > "$data_out/${f}"
done
