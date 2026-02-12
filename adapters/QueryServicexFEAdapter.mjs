'use strict';
export { QueryServicexFEAdapter };

import * as cmn from '../lib/common.mjs';

class QueryServicexFEAdapter {

  querySubmitToFE(queryModel) {
    return {
      status: cmn.QUERY_STATUS.COMPLETE,
      data: queryModel.pk
    }
  }

  queryStatusToFE(queryModel) {
    return {
      status: queryModel.status,
      data: {
        qid: queryModel.pk,
        query: queryModel.metadata.query,
        aras: queryModel.metadata.aras,
        timestamp: queryModel.time_updated
      }
    };
  }
}
