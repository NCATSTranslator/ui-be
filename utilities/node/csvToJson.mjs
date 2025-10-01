import { open } from 'node:fs/promises';

const filePath = process.argv[2];
const file = await open(filePath);
const jsonData = [];
const lines = file.readLines();
let header = null;
for await (const line of lines) {
  const entry = line.split(',');
  if (header === null) {
    header = entry.map(entry => entry.toLowerCase());
  } else {
    const jsonEntry = {};
    for (let j = 0; j < entry.length; j++) {
      jsonEntry[header[j]] = entry[j] || null;
    }
    jsonData.push(jsonEntry);
  }
}
console.log(JSON.stringify(jsonData));
