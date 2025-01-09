export { ClientxServiceAdapter };
import { logger } from '../lib/logger.mjs';
import * as cmn from '../lib/common.mjs';

class ClientxServiceAdapter {
  processQueryUpdate(queryUpdate) {
    const eventType = cmn.jsonGet(queryUpdate, 'event_type');
    const update = {
      pk: cmn.jsonGet(queryUpdate, 'pk'),
      timestamp: cmn.jsonGet(queryUpdate, 'timestamp'),
      status: null,
      aras: []
    };
    switch (eventType) {
      case _UPDATE_EVENT.MV_AVAILABLE:
        update.aras = cmn.jsonGet(queryUpdate, 'merged_versions_list').map(idAraPair => idAraPair[1]);
        update.aras.reverse();
        if (cmn.jsonGet(queryUpdate, 'complete')) {
          update.status = cmn.QUERY_STATUS.COMPLETE;
        } else {
          update.status = cmn.QUERY_STATUS.RUNNING;
        }
        break;
      case _UPDATE_EVENT.MV_BEGUN:
      case _UPDATE_EVENT.ARA_COMPLETE:
        break; // We can ignore these events for now
      case _UPDATE_EVENT.ERROR:
        logger.error(`Received ARS callback error for query with PK ${update.pk}: ${queryUpdate.message}`);
        update.status = cmn.QUERY_STATUS.ERROR;
        break;
      default:
        throw new Error(`Unknown ARS callback event type: ${eventType}`);
    }
    return update;
  }
}


const _UPDATE_EVENT = Object.freeze({
  MV_AVAILABLE: 'merged_version_available',
  MV_BEGUN: 'merged_version_begun',
  ARA_COMPLETE: 'ara_response_complete',
  ERROR: 'ars_error'
});
