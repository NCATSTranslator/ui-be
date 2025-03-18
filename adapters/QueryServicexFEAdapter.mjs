'use strict';
export { QueryServicexFEAdapter };

class QueryServicexFEAdapter {

  querySubmitToFE(queryModel) {
    return {
      status: 'success',
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
