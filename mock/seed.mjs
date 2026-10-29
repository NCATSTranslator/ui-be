export { seed_mock_queries, mock_query_request };

import { logger } from '#lib/logger.mjs';
import * as cmn from '#lib/common.mjs';

/* Gives the bypass test user a saved query for every top-level mock ARS data set so they appear in
 * the query history and own a row in queries/query_to_user, which the per-query user routes (query
 * views, working view) require. Idempotent: data sets the user already has are skipped. */
async function seed_mock_queries(ars_client, query_service, user_service, user_id) {
  const user_queries = await user_service.get_user_queries_map(user_id, true, false);
  for (const { pk, query_graph } of ars_client.mock_queries()) {
    if (user_queries.has(pk)) continue;
    const query_request = mock_query_request(query_graph);
    if (query_request === null) {
      logger.warn(`[mock ARS] no query graph for pk ${pk}; not seeding a user query`);
      continue;
    }
    try {
      const query = (await query_service.getQueryByPk(pk))
        ?? (await query_service.createQuery(pk, query_request, cmn.QUERY_STATUS.COMPLETE));
      const user_query = await user_service.createUserQuery(user_id, pk, query_request);
      await query_service.addQueryUserRelationship(query, user_query);
      logger.info(`[mock ARS] seeded query ${pk} (${query_request.curie}) for user ${user_id}`);
    } catch (err) {
      logger.warn(err, `[mock ARS] failed to seed query ${pk} for user ${user_id}`);
    }
  }
}

function mock_query_request(query_graph) {
  const pinned = Object.values(query_graph?.nodes ?? {}).find((node) => cmn.is_array(node.ids) && node.ids.length > 0);
  if (pinned === undefined) return null;
  return { type: 'drug', curie: pinned.ids[0], direction: null };
}
