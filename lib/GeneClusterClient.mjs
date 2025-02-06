export { GeneClusterClient };
import { logger } from './logger.mjs';
import * as cmn from './common.mjs';

class GeneClusterClient {
  constructor(endpoint) {
    this.endpoint = endpoint;
  }

  async getGeneClusters(genes) {
    const body = { genes: genes };
    return cmn.sendRecvJSON2(this.endpoint, 'POST', {}, body);
  }
}
