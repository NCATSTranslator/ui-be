#!/bin/sh

pk_file=$1
config_file=$2
if [ -z "$config_file" ]; then
  config_file="./configurations/production.json"
fi

test_dir="./test/data/regression"
test_file="./test/regression.mjs"
data_in="$test_dir/in"
data_out="$test_dir/out"
cd ..
mkdir -p $data_in $data_out
echo "'use strict'
import * as assert from 'assert';
import * as cfg from '../lib/config.mjs';
import * as cmn from '../lib/common.mjs';
import * as tsmy from './lib/summarization.mjs';
import { loadBiolink } from '../lib/biolink-model.mjs';
import { loadChebi } from '../lib/chebi.mjs';
import { TranslatorServicexFEAdapter } from '../adapters/TranslatorServicexFEAdapter.mjs';

// We have to do this because the 'before' hook does not seem to work
async function loadConfig() {
  const config = await cfg.bootstrapConfig('test/data/regression/config.json')
  await loadBiolink(config.biolink.version,
                    config.biolink.support_deprecated_predicates,
                    config.biolink.infores_catalog,
                    config.biolink.prefix_catalog);
  await loadChebi();
}

async function regressionTest(testFile) {
  await loadConfig();
  const input = cmn.readJson("'`test/data/regression/in/${testFile}`'");
  const expected = cmn.readJson("'`test/data/regression/out/${testFile}`'");
  const maxHops = 3;
  const translatorAdapter = new TranslatorServicexFEAdapter();
  const actual = await translatorAdapter.queryResultsToFE(await input, maxHops);
  tsmy.testSummary(actual.data, await expected);
}

" > $test_file

if [ -n "$pk_file" ]; then
  rm -f $data_in/* $data_out/*
  for pk in $(cat "./utilities/${pk_file}"); do
    node ./utilities/node/downloadQuery.mjs ${pk} > "$data_in/${pk}.json"
    node ./utilities/node/summarizeQuery.mjs "${config_file}" "$data_in/${pk}.json" > "$data_out/${pk}.json"
  done
fi

for f in $(ls $data_in); do
  echo "await regressionTest('${f}');" >> $test_file
done
