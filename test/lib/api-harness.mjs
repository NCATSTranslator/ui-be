/* Shared harness for the standalone API tests in test/api/.
 *
 * These tests run against a live server (see each test's header for how to start it). This module
 * factors out the boilerplate every test repeats: pass/fail counting, the verbose response echo, and
 * the JSON request helpers.
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { BYPASS_TEST_USER } from '../../mock/auth.mjs';

export const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
export const TEST_USER_ID = BYPASS_TEST_USER.id;
export const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

// A per-file harness: ok() records a check, fail() records a thrown error, finish() prints the
// summary and exits with the conventional status code.
export function createHarness() {
  let failures = 0;
  function ok(cond, msg) {
    console[cond ? 'log' : 'error'](`  ${cond ? '✓' : '✗'} ${msg}`);
    if (!cond) failures += 1;
  }
  function fail(msg) {
    console.error(`  ✗ ${msg}`);
    failures += 1;
  }
  function finish() {
    console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
    process.exit(failures === 0 ? 0 : 1);
  }
  return { ok, fail, finish };
}

// When verbose, echo the server's raw response (status line + body) so it can be eyeballed.
export function showResponse(method, path, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- server response: ${method} ${path} -> ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

export async function postJson(path, body) {
  const res = await fetch(`${BASE_URL}${path}`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const raw = await res.text();
  showResponse('POST', path, res, raw);
  let json = null;
  if (raw) { try { json = JSON.parse(raw); } catch { /* non-JSON */ } }
  return { res, json, raw };
}

export async function putJson(path, body) {
  const res = await fetch(`${BASE_URL}${path}`, {
    method: 'PUT',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(body),
  });
  const raw = await res.text();
  showResponse('PUT', path, res, raw);
  let json = null;
  if (raw) { try { json = JSON.parse(raw); } catch { /* non-JSON */ } }
  return { res, json, raw };
}

export async function getJson(path) {
  const res = await fetch(`${BASE_URL}${path}`);
  const raw = await res.text();
  showResponse('GET', path, res, raw);
  let json = null;
  if (raw) { try { json = JSON.parse(raw); } catch { /* non-JSON */ } }
  return { res, json, raw };
}
