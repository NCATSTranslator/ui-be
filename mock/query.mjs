export { seedDemoUserQuery, DEMO_QUERY_PK, DEMO_QUERY_USER_ID, DEMO_QUERY_REQUEST };

import { logger } from '#lib/logger.mjs';
import { UserSavedData, UserQueryData, SAVE_TYPE } from '#model/UserSavedData.mjs';

/* Mock query 3 (chemicals that treat MONDO:0005148, type 2 diabetes mellitus), attached to
 * the demo user so the account has something in its query list. DEMO_QUERY_PK is the uuid
 * alias that mock/client.mjs maps onto fixture "3"; the ars_pkey column is a uuid, so the
 * fixture's short name cannot be stored directly. */
const DEMO_QUERY_PK = '00000000-0000-4000-8000-000000000003';
const DEMO_QUERY_USER_ID = '1cce12fa-6120-4c40-92f9-a2ef9937c917';
const DEMO_QUERY_REQUEST = Object.freeze({
  type: 'drug',
  curie: 'MONDO:0005148',
  direction: null
});
const DEMO_QUERY_TITLE = 'Type 2 Diabetes Mellitus (mock query 3)';

/* Unlike the API key, user queries live in Postgres and so outlive a restart: seed only when
 * the query is not already there, or every boot would add another copy. */
async function seedDemoUserQuery(userService) {
  const existing = await userService.getUserSavesBy(
    DEMO_QUERY_USER_ID, { ars_pkey: DEMO_QUERY_PK, save_type: SAVE_TYPE.QUERY }, true);
  if (existing && existing.length > 0) {
    logger.info(`seedDemoUserQuery: query ${DEMO_QUERY_PK} already saved for user `
      + `${DEMO_QUERY_USER_ID}. Leaving it as is.`);
    return existing[0];
  }

  const data = new UserQueryData(DEMO_QUERY_REQUEST);
  data.title = DEMO_QUERY_TITLE;
  const saved = await userService.saveUserData(new UserSavedData({
    user_id: DEMO_QUERY_USER_ID,
    save_type: SAVE_TYPE.QUERY,
    ars_pkey: DEMO_QUERY_PK,
    data: data
  }));
  if (!saved) {
    logger.warn(`seedDemoUserQuery: failed to save query ${DEMO_QUERY_PK} for user ${DEMO_QUERY_USER_ID}.`);
    return null;
  }

  logger.warn(`*** DEMO QUERY SEEDED *** mock query 3 as pk ${DEMO_QUERY_PK} -> user `
    + `${DEMO_QUERY_USER_ID} (save id ${saved.id}).`);
  return saved;
}
