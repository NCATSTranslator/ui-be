export { bypassSessionAuth, BYPASS_TEST_USER };

import { logger } from '#lib/logger.mjs';
import { User } from '#model/User.mjs';
import { SESSION_VALID } from '../services/AuthService.mjs';

// Fixed identity used when session auth checking is bypassed.
const BYPASS_TEST_USER = {
  id: '11111111-1111-4111-8111-111111111111',
  name: 'Test User',
  email: 'test-user@local.invalid'
};

/* Ensures a fixed test user exists in the DB, then patches the given AuthService so that session
 * authentication is bypassed entirely: every request resolves to that test user, with or without a
 * session cookie. This both skips session validation and gives the privileged routes (which are
 * keyed on the user id) a real user to operate against. For local development and API testing
 * only -- must never be enabled in production. Optional userOverrides let callers pin a specific
 * id/name/email. Returns the test user. */
async function bypassSessionAuth(authService, userOverrides = {}) {
  const spec = { ...BYPASS_TEST_USER, ...(userOverrides || {}) };
  let user = await authService.getUserById(spec.id);
  if (!user) {
    user = await authService.userStore.createNewUser(new User(spec));
    if (!user) {
      throw new Error('bypassSessionAuth: failed to create test user.');
    }
    logger.warn(`bypassSessionAuth: created test user ${user.id} (${user.email}).`);
  }

  const bypassSession = () => ({ token: 'auth-check-disabled' });
  authService.getSessionData = async () => ({ status: SESSION_VALID, user, session: bypassSession() });
  authService.updateSessionTime = async () => bypassSession();
  authService.refreshSessionToken = async () => bypassSession();
  authService.expireSessionByToken = async () => bypassSession();

  logger.warn(`*** SESSION AUTH CHECKING IS DISABLED *** All requests run as test user ${user.id} (${user.email}). Do NOT use this configuration in production.`);
  return user;
}
