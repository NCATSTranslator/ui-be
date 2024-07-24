import * as ct from './common.mjs';

export function testSummary(ac, ex) {
  const testKeys = ['nodes', 'edges', 'paths', 'results', 'publications', 'tags'];
  for (let k of testKeys) {
    ct.testDeep(ac[k], ex[k]);
  }
}
