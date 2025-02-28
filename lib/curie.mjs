export function get_id(curie) {
  return curie.split(':')[1];
}

export function is_mesh(curie) {
  return curie.startsWith('MESH:');
}

export function is_ncbi(curie) {
  return curie.startsWith('NCBIGene:');
}

export function is_chembl(curie) {
  return curie.startsWith('CHEMBL');
}

export function is_doid(curie) {
  return curie.startsWith('DOID:');
}

export function is_drug_central(curie) {
  return curie.startsWith('DrugCentral:');
}

export function gen_url_safe_curie(curie) {
  return curie.replace(':', '%3A');
}

