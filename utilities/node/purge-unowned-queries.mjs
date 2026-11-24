import { pg } from "../../lib/postgres_preamble.mjs";
import { bootstrapConfig } from "../../lib/config.mjs";
import { QueryStorePostgres } from "../../stores/QueryStorePostgres.mjs";
import { QueryService } from "../../services/QueryService.mjs";

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

const query_service = (function (config) {
  const db_pool = new pg.Pool({
    ...config.storage.pg,
    password: config.secrets.pg.password,
    ssl: config.db_conn.ssl
  });
  return new QueryService(new QueryStorePostgres(db_pool), null);
})(SERVER_CONFIG);

try {
  const delete_count = await query_service.delete_unowned_queries();
  console.log(`Deleted ${delete_count} unowned queries`);
} catch (err) {
  console.error(`Error occurred: ${err}`);
  process.exitCode = 1;
} finally {
  await query_service.stop();
}
