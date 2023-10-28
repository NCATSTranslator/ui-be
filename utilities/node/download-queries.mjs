'use strict';

import { createReadStream, promises as fs } from 'fs';
import { createInterface } from 'readline';
import { ARSClient } from '../../ARSClient.mjs';

/**
 * Reads all lines from a file, trims them and returns an array of non-empty lines.
 * @param {string} filename - The name of the file to be read.
 * @returns {Promise<string[]>} - A promise that resolves to an array of trimmed, non-empty lines.
 */
async function readAndTrimLines(filename) {
  // Create a read stream from the file
  const fileStream = createReadStream(filename);

  // Create an interface to read lines from the file stream
  const rl = createInterface({
    input: fileStream,
    crlfDelay: Infinity
  });

  // Initialize an array to store the trimmed lines
  const lines = [];

  // Read lines from the file and process them
  for await (const line of rl) {
    // Trim the line and check if it's non-empty
    const trimmedLine = line.trim();
    if (trimmedLine) {
      // Add the non-empty trimmed line to the lines array
      lines.push(trimmedLine);
    }
  }

  // Close the readline interface
  rl.close();

  // Return the array of trimmed, non-empty lines
  return lines;
}


async function writeToFile(object, filename) {
  // Convert the object to a pretty-printed JSON string with an indent level of 2
  const jsonString = JSON.stringify(object, null, 2);
  // Write the JSON string to the specified file
  await fs.writeFile(filename, jsonString, 'utf-8');
}

let l = await readAndTrimLines(process.argv[2]);
const arsclient = new ARSClient(
  'https://ars-prod.transltr.io',
  '/ars/api/messages', '/ars/api/submit');
const filters = {whitelistRx: /^ara-/};

l.forEach(async (e) => {
  let res = await arsclient.collectAllResults(e, filters, true);
  writeToFile(res, `saved_res.out.${e}`);
  console.log(`Done ${e}`);
});
