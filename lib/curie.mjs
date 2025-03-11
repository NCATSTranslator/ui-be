export function is_chebi(curie) {
  return curie.startsWith('CHEBI:');
}

export function is_chembl_compound(curie) {
  return curie.startsWith('CHEMBL.COMPOUND:');
}

export function is_doid(curie) {
  return curie.startsWith('DOID:');
}

export function is_drug_central(curie) {
  return curie.startsWith('DrugCentral:');
}

export function is_drugbank(curie) {
  return curie.startsWith('DRUGBANK:');
}

export function is_ensembl_ensg(curie) {
  return curie.startsWith('ENSEMBL:ENSG');
}

export function is_go(curie) {
  return curie.startsWith('GO:');
}

export function is_hgnc(curie) {
  return curie.startsWith('HGNC:');
}

export function is_hp(curie) {
  return curie.startsWith('HP:');
}

export function is_mesh(curie) {
  return curie.startsWith('MESH:');
}

export function is_mondo(curie) {
  return curie.startsWith('MONDO:');
}

export function is_ncbi(curie) {
  return curie.startsWith('NCBIGene:');
}

export function is_ncit(curie) {
  return curie.startsWith('NCIT:');
}

export function is_pubchem_compound(curie) {
  return curie.startsWith('PUBCHEM.COMPOUND:');
}

export function is_uberon(curie) {
  return curie.startsWith('UBERON:');
}

export function get_id(curie) {
  return curie.split(':')[1];
}

export function gen_url_safe_curie(curie) {
  return curie.replace(':', '%3A');
}

export function remove_compound(curie) {
  return curie.replace('.COMPOUND', '');
}

