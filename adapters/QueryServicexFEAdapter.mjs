'use strict';
export { QueryServicexFEAdapter };

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
        aras: queryModel.metadata.aras,
        timestamp: queryModel.time_updated
      }
    };
  }
}
