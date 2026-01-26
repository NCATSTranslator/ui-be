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
  const stale_pks = map_pk_queries.keys();
  if (cmn.isArrayEmpty(stale_pks)) {
    return;
  }
  const notifications = await translator_service.get_notification_statuses(stale_pks);
  const updates = []
  for (let notification of notifications) {
    const query = map_pk_queries.get(notification.pk);
    if (notification.status !== query.status || notification.aras.length !== query.metadata.aras.length) {
      query.status = notification.status;
      query.metadata.aras = notification.aras;
    }
    updates.push(query);
  }
  if (!cmn.isArrayEmpty(updates)) {
    const success = await query_service.UNSAFE_batch_write(updates);
  }
}

try {
  await main();
} catch (err) {
  console.warn(`Error occured: ${err}`);
}
