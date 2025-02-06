export { TranslatorServicexGeneClusterAdapter }
import * as cmn from '../lib/common.mjs';
import * as trapi from '../lib/trapi.mjs';

class TranslatorServicexGeneClusterAdapter {
  getGenes(msg) {
    const answers = msg.completed.filter(e => {
      return !!e.data && cmn.isObject(e.data);
    });
    const genes = [];
    for (const answer of answers) {
      const trapiAnswer = answer.data;
      const curies = trapi.getCuries(trapi.getKgraph(trapiAnswer));
      const ncbiGenes = curies.filter(curie => curie.startsWith('NCBIGene:'));
      genes.push(...ncbiGenes);
    }
    return cmn.distinctArray(genes);
  }
}
