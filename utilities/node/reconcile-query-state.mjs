import { pg } from "../../lib/postgres_preamble.mjs";
import { bootstrapConfig } from "../../lib/config.mjs";
import * as cmn from "../../lib/common.mjs";
import { ARSClient } from "../../lib/ARSClient.mjs";
import { TranslatorService } from "../../services/TranslatorService.mjs";
import { QueryStorePostgres } from "../../stores/QueryStorePostgres.mjs";
import { QueryService } from "../../services/QueryService.mjs";

async function main() {
  const SERVER_CONFIG = await (async function() {
    let basefile, overrides = null;
    if (process.argv.length === 3) {
      basefile = process.argv[2];
    } else if (process.argv.length === 4) {
      basefile = process.argv[2];
      overrides = process.argv[3]
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

  const stale_queries = await query_service.get_stale_queries();
  const map_pk_queries = new Map();
  for (let i = 0; i < stale_queries.length; i++) {
    const query = stale_queries[i];
    map_pk_queries.set(query.pk, query);
  }
  const stale_pks = [...map_pk_queries.keys()];
  if (cmn.isArrayEmpty(stale_pks)) {
    return;
  }
  const [_, notifications] = await translator_service.get_notification_statuses(stale_pks);
  const updates = []
  for (let n of notifications) {
    if (n.status === null) {
      console.error(`ARS does has no record of pk: ${n.pk}`);
      continue;
    }
    const query = map_pk_queries.get(n.pk);
    if (n.status !== query.status
        || n.merged_list.length !== query.metadata.aras.length) {
      query.status = _ars_status_to_query_status(n.status);
      query.metadata.aras = n.merged_list.map(merged_entry => {
        return merged_entry[1];
      });
      updates.push(query);
    }
  }
  if (!cmn.isArrayEmpty(updates)) {
    const success = await query_service.UNSAFE_batch_update(updates);
  }
}

function _ars_status_to_query_status(ars_status) {
  switch(ars_status) {
    case "Done": return cmn.QUERY_STATUS.COMPLETE;
    case "Running": return cmn.QUERY_STATUS.RUNNING;
    case "Error": return cmn.QUERY_STATUS.ERROR;
  }
  throw RangeError(`Unexpected ARS status: ${ars_status}`);
}

try {
  await main();
} catch (err) {
  console.error(`Error occured: ${err}`);
  process.exitCode = 1;
}
