// Generates a JSON file of the roles heirarchy from the ChEBI ontology in the .obo format
import * as fs from 'node:fs';

function isTermRole(term, terms) {
  let parent = term.is_a;
  while (parent)
  {
    term  = terms[parent];
    parent = term.is_a;
  }

  return term.name === 'role';
}

const filePath = process.argv[2];
const lines = fs.readFileSync(filePath, 'utf-8').split('\n');
const terms = {};
let lineNum = 0;
while (lineNum < lines.length) {
  if (lines[lineNum] === '[Term]') {
    const idLine = lineNum+1;
    lineNum+=2;
    const term = {};
    while (lines[lineNum] !== '') {
      const [key, value] = lines[lineNum].split(': ');
      if (key === 'name' || key === 'is_a') {
        term[key] = value;
      }

      lineNum++;
    }

    terms[lines[idLine].split(': ')[1]] = term;
  }

  lineNum++;
}

const roles = {};
for (const [id, term] of Object.entries(terms)) {
  if (isTermRole(term, terms)) {
    roles[id] = term;
  }
}

console.log(JSON.stringify(roles));