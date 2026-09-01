export { seedDemoApiKey, DEMO_API_KEY, DEMO_API_KEY_USER_ID };

import { logger } from '#lib/logger.mjs';
import { ApiKey } from '#model/ApiKey.mjs';

/* A fixed API key seeded at startup so the demo has a credential that survives a restart --
 * the in-memory store loses everything else. Being fixed, it is public knowledge and grants
 * full access to the account below, so StartServer only seeds it under the mock
 * configuration, alongside the auth_check bypass and subject to the same rail. */
const DEMO_API_KEY = 'tkey_demo-key-for-local-testing-do-not-deploy---';
const DEMO_API_KEY_USER_ID = '1cce12fa-6120-4c40-92f9-a2ef9937c917';

/* Registers the demo key against DEMO_API_KEY_USER_ID. The key authenticates by resolving to
 * that user, so a missing or deleted account yields a key that exists but 401s on every
 * request; we say so at startup rather than leaving it to be discovered against the API. */
async function seedDemoApiKey(apiKeyStore, userStore) {
  const user = await userStore.retrieveUserById(DEMO_API_KEY_USER_ID);
  if (!user) {
    logger.warn(`seedDemoApiKey: no user ${DEMO_API_KEY_USER_ID} in the DB. Seeding the demo `
      + `key anyway, but it will fail authentication until that user exists.`);
  } else if (user.deleted) {
    logger.warn(`seedDemoApiKey: user ${DEMO_API_KEY_USER_ID} is marked deleted. The demo key `
      + `will fail authentication until that user is restored.`);
  }

  const apiKey = await apiKeyStore.createApiKey(
    ApiKey.fromRawKey(DEMO_API_KEY_USER_ID, 'demo key (seeded at startup)', DEMO_API_KEY));

  logger.warn(`*** DEMO API KEY SEEDED *** ${DEMO_API_KEY} -> user ${DEMO_API_KEY_USER_ID}`
    + `${user ? ` (${user.email})` : ''}. This key is public knowledge. Do NOT use this `
    + `configuration in production.`);
  return apiKey;
}
