'use strict';

import * as cmn from './common.mjs';
import { validate as isUuid } from 'uuid';
import * as trapi from './trapi.mjs';
import { loadConfigFromFile, postProcessConfig }  from './config.mjs';
import { loadBiolink } from './biolink-model.mjs';



async function queryResultsToFE(msg, maxHops)
{
  // Omit ARA results where the actual results array is empty
  // Need to account for the ARS returning both null and []
  let data = msg.completed.filter(e =>
    {
      return Array.isArray(e.data.results) && e.data.results.length > 0;
    }).map(e =>
      {
        return {
          agent: e.agent,
          message: e.data
        }
      });


  return {
    status: 'whatever',
    data: await trapi.creativeAnswersToSummary(msg.pk,
      data,
      maxHops,
      null) // pass annotation client as null
  };
}

async function measureOne(key, val) {
  const start = new Date();
  const res = await queryResultsToFE(val, 3);
  const time = new Date() - start;
  return time;
}

async function measureN(n, hash, keys) {
  const l = keys.length;
  let tot = 0;
  for (let i = 0; i < n; i++) {
    let key = keys[Math.floor(Math.random() * l)];
    let rtime = await measureOne(key, hash[key]);
    console.log(`Time for ${key}: ${rtime}`);
    tot += rtime;
  }
  console.log(`Ave time for ${n} runs: ${tot} / ${n}: ${tot / n}`);
}

const program_start = new Date();
const config_file = process.argv[2]
const dir = process.argv[3];
const prefix = process.argv[4];

const SERVER_CONFIG = await loadConfigFromFile(process.argv[2]);

await loadBiolink(SERVER_CONFIG.biolink.version,
  SERVER_CONFIG.biolink.support_deprecated_predicates,
  SERVER_CONFIG.biolink.infores_catalog,
  SERVER_CONFIG.biolink.prefix_catalog);

const h = await cmn.loadQueryData(dir, prefix);
console.log(Object.keys(h).length);

console.log(`Bootstrap complete: ${new Date() - program_start} ms`);

// sanity check:
let check = await queryResultsToFE(h['eca67dd8-ccb6-4824-8745-b33cb23a2602'], 3);
console.log(check);

// 2.4 - 818K
var small = [
  'f57761ef-2891-4826-998b-e36822108823','1adf9afb-0fe5-448f-b898-6eff6d180e31',
  '17d9baad-7f30-462f-b989-8d863b2a6dac','07ee73bb-9c30-4107-b7e8-9133c62cfe5a',
  '8530d9c4-7d20-417e-a462-d090d45ac6c4','8c56a6fb-aba4-4b0a-a884-4b9dd418cb49',
  '68e67c9e-b2d3-4077-8113-df49b3695ca2','723e676a-4269-403c-8614-56f5329cb700',
  'c5f9c262-b40e-4e87-9e7a-21a764eaa037'
];

// 1.3M - 3.2M
var medium = ['eca67dd8-ccb6-4824-8745-b33cb23a2602', 'a514fec0-b841-48d8-b997-0693378e5f30',
  '37eb6ae6-9aeb-4176-a663-9b4e08726ebf', '4fe06c37-6eda-4d0a-9970-514b5b56cb7f', 'd3e1a441-2b21-4e3f-8601-0979ba6b966c',
  'fe687e50-8528-4bb8-b353-da450fbbb6b5', '4b95e5ae-f96d-4574-88fa-3fbf8d5b3546', '06362dd2-b216-4f2e-a10a-22ec3440e27d',
  '6562e5f6-21a7-4179-a7fd-c10015baf76d', 'cd5cb3fb-a2f4-48cc-9545-cdc33d0d3b26', 'b0b5b5d3-7830-4b3a-9b7a-94a565529564',
  '827c85d4-6a2c-46a0-a6a1-ed897cf56318', '1b41cf85-b69c-427b-a02c-b620d217a741'];

// 3.2M - 12M
var bigger = ['1b41cf85-b69c-427b-a02c-b620d217a741','fce60ae0-12f7-4f9c-ac66-83692d07ab51',
  '277ba4d0-817c-411f-8f86-8d16e9422eb9','09ac9cc5-cf28-40b5-ada3-d3a061fe4e33',
  'bd7e4ab7-33b6-46db-99fb-452413f47886','aa9ffb9b-43b2-4335-a409-7a62763da50c',
  '70ebf6a8-7858-4a0f-8023-99427fc1e6bb','18146dab-56ce-4781-8b72-3b5e78ea1acc',
  'd87d08ac-47ac-402d-b541-cd4bc797a6c4','91b2e8ee-ac73-4a1d-896d-12fd79950569',
  '8ade18e6-872d-435a-9a3b-05a52e593ca5','ddcc3e3e-3ffe-4af1-b75c-6e57886ef8ed',
  '7c1e3ab6-405f-4bbd-84fe-11e7b00d75e2'];

// 11M - 44M
var large = ['ddcc3e3e-3ffe-4af1-b75c-6e57886ef8ed', '7c1e3ab6-405f-4bbd-84fe-11e7b00d75e2',
  '4ff9292b-018b-497c-ad0b-34fcf8f406ed', 'feb1a230-8c13-4c74-b038-92f31f969d60',
  'b069032a-d59e-411b-8ada-4720ab4fc82b', 'd6d1ab17-e779-437c-a6ce-423556724d76',
  '57ddf8a4-a77e-4abb-84c2-4b6f734516be'];

// 19M - 44M
var largest = [ '4ff9292b-018b-497c-ad0b-34fcf8f406ed', 'feb1a230-8c13-4c74-b038-92f31f969d60',
'b069032a-d59e-411b-8ada-4720ab4fc82b', 'd6d1ab17-e779-437c-a6ce-423556724d76',
'57ddf8a4-a77e-4abb-84c2-4b6f734516be'];

// Ave time for 10 runs: 89 / 10: 8.9
// Ave time for 100 runs: 794 / 100: 7.94
// Ave time for 500 runs: 3011 / 500: 6.022
// Ave time for 1000 runs: 6644 / 1000: 6.644
// await measureN(1000, h, small);

// Ave time for 10 runs: 677 / 10: 67.7
// Ave time for 100 runs: 4987 / 100: 49.87
// Ave time for 500 runs: 26659 / 500: 53.318
// Ave time for 1000 runs: 43131 / 1000: 43.131
// await measureN(10, h, medium);

// Ave time for 10 runs: 1052 / 10: 105.2
// Ave time for 100 runs: 12761 / 100: 127.61
// Ave time for 500 runs: 66727 / 500: 133.454
// Ave time for 1000 runs: 134590 / 1000: 134.59
// await measureN(1000, h, bigger);

// Ave time for 10 runs: 1951 / 10: 195.1
// Ave time for 100 runs: 20950 / 100: 209.5
// Ave time for 500 runs: 114447 / 500: 228.894
// Ave time for 1000 runs: 241046 / 1000: 241.046
// await measureN(1000, h, large);

// Ave time for 10 runs: 3867 / 10: 386.7
// Ave time for 100 runs: 34092 / 100: 340.92
// Ave time for 500 runs: 134299 / 500: 268.598
// Ave time for 1000 runs: 318620 / 1000: 318.62
// await measureN(1000, h, largest);
