// Generates a JSON file of the roles heirarchy from the ChEBI ontology in the .obo format
import * as fs from 'node:fs';

function belongsToTree(term, terms, treeRoot) {
  let parent = term.is_a;
  while (parent)
  {
    term  = terms[parent];
    parent = term.is_a;
  }

  return term.name === treeRoot;
}

const filePath = process.argv[2];
const treeRoot = process.argv[3];
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
      if (key === 'name') {
        term[key] = value;
      } else if (key === 'is_a') {
        term[key] = value.split(' ')[0];
      }

      lineNum++;
    }

    terms[lines[idLine].split(': ')[1]] = term;
  }

  lineNum++;
}

const tree = {};
for (const [id, term] of Object.entries(terms)) {
  if (belongsToTree(term, terms, treeRoot)) {
    tree[id] = term;
  }
}

console.log(JSON.stringify(tree));
