export { main };

import { realpathSync } from "node:fs";
import { fileURLToPath } from "node:url";
import { pg } from "../../lib/postgres_preamble.mjs";
import { bootstrapConfig } from "../../lib/config.mjs";
import * as cmn from "../../lib/common.mjs";
import { ARSClient } from "../../lib/ARSClient.mjs";
import { TranslatorService } from "../../services/TranslatorService.mjs";
import { QueryStorePostgres } from "../../stores/QueryStorePostgres.mjs";
import { QueryService } from "../../services/QueryService.mjs";

async function main(translator_service, query_service) {
  const stale_queries = await query_service.get_stale_queries();
  const map_pk_queries = new Map();
  for (let i = 0; i < stale_queries.length; i++) {
    const query = stale_queries[i];
    map_pk_queries.set(query.pk, query);
  }
  const stale_pks = [...map_pk_queries.keys()];
  if (cmn.is_array_empty(stale_pks)) {
    return { update_count: 0, delete_count: 0 };
  }
  const [_, notifications] = await translator_service.get_notification_statuses(stale_pks);
  const updates = [];
  const lost_pks = [];
  for (let n of notifications) {
    const query = map_pk_queries.get(n.pk);
    if (query === undefined) {
      console.error(`ARS sent a record with an unrequested pk: ${n.pk}`);
      continue;
    }
    const status = _ars_status_to_query_status(n.status);
    if (status === null) {
      console.error(`ARS has no usable status for pk ${n.pk}: ${n.status}`);
      lost_pks.push(n.pk);
      continue;
    }
    const aras = n.merged_list === null
      ? query.metadata.aras
      : n.merged_list.map(merged_entry => merged_entry[1]);
    if (status !== query.status
        || aras.length !== query.metadata.aras.length) {
      query.status = status;
      query.metadata.aras = aras;
      updates.push(query);
    }
  }
  let update_count = 0;
  if (!cmn.is_array_empty(updates)) {
    update_count = await query_service.UNSAFE_batch_update(updates);
  }
  let delete_count = 0;
  if (!cmn.is_array_empty(lost_pks)) {
    delete_count = await query_service.delete_lost_queries(lost_pks);
  }
  return { update_count: update_count, delete_count: delete_count };
}

function _ars_status_to_query_status(ars_status) {
  switch(ars_status) {
    case "Done": return cmn.QUERY_STATUS.COMPLETE;
    case "Running": return cmn.QUERY_STATUS.RUNNING;
    case "Error": return cmn.QUERY_STATUS.ERROR;
    default: return null;
  }
}

if (realpathSync(process.argv[1]) === fileURLToPath(import.meta.url)) {
  const SERVER_CONFIG = await (async function() {
    let basefile, overrides = null;
    if (process.argv.length === 3) {
      basefile = process.argv[2];
    } else if (process.argv.length === 4) {
      basefile = process.argv[2];
      overrides = process.argv[3];
    } else {
      throw new Error(`Unsupported number of args (${process.argv.length}) at startup. Exiting.`);
    }
    return bootstrapConfig(basefile, overrides);
  })();

  const translator_service = await (async function (config) {
    return new TranslatorService(new ARSClient(config.ars_endpoint, config.secrets.hmac.key));
  })(SERVER_CONFIG);

  const query_service = (function (config) {
    const dbPool = new pg.Pool({
      ...config.storage.pg,
      password: config.secrets.pg.password,
      ssl: config.db_conn.ssl
    });
    return new QueryService(new QueryStorePostgres(dbPool), null);
  })(SERVER_CONFIG);

  try {
    const { update_count, delete_count } = await main(translator_service, query_service);
    console.log(`Updated ${update_count} rows`);
    console.log(`Deleted ${delete_count} lost queries`);
  } catch (err) {
    console.error(`Error occurred: ${err}`);
    process.exitCode = 1;
  } finally {
    await query_service.stop();
  }
}
