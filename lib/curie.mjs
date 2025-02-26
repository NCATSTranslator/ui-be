export function getId(curie) {
  return curie.split(':')[1];
}

export function isMesh(curie) {
  return curie.startsWith('MESH:');
}

export function isNcbi(curie) {
  return curie.startsWith('NCBIGene:');
}

export function isChembl(curie) {
  return curie.startsWith('CHEMBL');
}

export function genUrlSafeCurie(curie) {
  return curie.replace(':', '%3A');
}

