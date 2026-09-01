/* Standalone sanity test for happy paths of the projects API:
 *
 *   POST /api/v1/users/me/projects           create a project
 *   GET  /api/v1/users/me/projects           list the current user's projects
 *   PUT  /api/v1/users/me/projects           update projects (array of {id, title?, pks?})
 *   PUT  /api/v1/users/me/projects/trash     soft-delete projects (array of ids)
 *   PUT  /api/v1/users/me/projects/restore   restore projects (array of ids)
 *
 * Exercises the create -> list -> update -> trash -> restore lifecycle.
 *
 * The test is intended to run with the mock ars server. Run it as follows:
 *   npm run mock-ars               # Ensure your local-overrides sets auth_check=false
 *   node test/api/projects.mjs
 *
 * Pass --verbose (or -v, or set VERBOSE=1) to print the raw server response for each request.
 * Override the target host with API_BASE_URL=... if the server is elsewhere.
 */

import { BYPASS_TEST_USER } from '../../mock/auth.mjs';

const BASE_URL = process.env.API_BASE_URL || 'http://localhost:8386';
const TEST_USER_ID = BYPASS_TEST_USER.id;
const VERBOSE = process.env.VERBOSE === '1' || process.argv.includes('--verbose') || process.argv.includes('-v');

let failures = 0;
function ok(cond, msg) {
  console[cond ? 'log' : 'error'](`  ${cond ? '✓' : '✗'} ${msg}`);
  if (!cond) failures += 1;
}

function showResponse(method, path, res, rawBody) {
  if (!VERBOSE) return;
  console.log(`  --- server response: ${method} ${path} -> ${res.status} ${res.statusText}`);
  let body = rawBody;
  if ((res.headers.get('content-type') || '').includes('application/json') && rawBody) {
    try { body = JSON.stringify(JSON.parse(rawBody), null, 2); } catch { /* leave raw */ }
  }
  console.log(body ? body.replace(/^/gm, '      ') : '      (empty body)');
}

async function req(method, path, body) {
  const opts = { method };
  if (body !== undefined) {
    opts.headers = { 'Content-Type': 'application/json' };
    opts.body = JSON.stringify(body);
  }
  const res = await fetch(`${BASE_URL}${path}`, opts);
  const rawBody = await res.text();
  showResponse(method, path, res, rawBody);
  let json = null;
  if (rawBody) { try { json = JSON.parse(rawBody); } catch { /* non-JSON */ } }
  return { res, json };
}

const findById = (arr, id) => Array.isArray(arr) ? arr.find((p) => p.id === id) : undefined;
const sameIds = (a, b) => JSON.stringify(a) === JSON.stringify(b);

console.log(`# user projects lifecycle  (target: ${BASE_URL}, test user: ${TEST_USER_ID})`);
try {
  const title = `api-test project ${new Date().toISOString()}`;

  // Create
  const create = await req('POST', '/api/v1/users/me/projects', { title, pks: ['pk-1', 'pk-2'] });
  ok(create.res.status === 200, `create responds 200 (got ${create.res.status})`);
  const project = create.json;
  ok(project && project.id !== undefined && project.id !== null, 'created project has an id');
  ok(project && project.user_id === TEST_USER_ID, 'created project belongs to the test user');
  ok(project && project.save_type === 'project', 'created project has save_type "project"');
  ok(project && project.data && project.data.title === title, 'created project stores the title under data');
  ok(project && project.data && sameIds(project.data.canvas_ids, []),
    'created project defaults canvas_ids to an empty array');

  const projectId = project ? project.id : null;

  // List
  const list = await req('GET', '/api/v1/users/me/projects');
  ok(list.res.status === 200, `list responds 200 (got ${list.res.status})`);
  ok(findById(list.json, projectId), 'created project appears in the projects list');

  // Update the title
  const newTitle = `${title} (updated)`;
  const update = await req('PUT', '/api/v1/users/me/projects', [{ id: projectId, title: newTitle }]);
  ok(update.res.status === 200, `update responds 200 (got ${update.res.status})`);

  const listAfterUpdate = await req('GET', '/api/v1/users/me/projects');
  const updated = findById(listAfterUpdate.json, projectId);
  ok(updated && updated.data && updated.data.title === newTitle, 'updated title persisted');
  ok(updated && updated.data && sameIds(updated.data.canvas_ids, []),
    'update without canvas_ids leaves canvas_ids untouched');

  const canvasIds = [101, 202];
  const canvasUpdate = await req('PUT', '/api/v1/users/me/projects', [{ id: projectId, canvas_ids: canvasIds }]);
  ok(canvasUpdate.res.status === 200, `canvas_ids update responds 200 (got ${canvasUpdate.res.status})`);

  const listAfterCanvasUpdate = await req('GET', '/api/v1/users/me/projects');
  const withCanvases = findById(listAfterCanvasUpdate.json, projectId);
  ok(withCanvases && withCanvases.data && sameIds(withCanvases.data.canvas_ids, canvasIds),
    'updated canvas_ids persisted');
  ok(withCanvases && withCanvases.data && withCanvases.data.title === newTitle,
    'canvas_ids update leaves the title untouched');

  const clearCanvases = await req('PUT', '/api/v1/users/me/projects', [{ id: projectId, canvas_ids: [] }]);
  ok(clearCanvases.res.status === 200, `canvas_ids clear responds 200 (got ${clearCanvases.res.status})`);
  const listAfterClear = await req('GET', '/api/v1/users/me/projects');
  const cleared = findById(listAfterClear.json, projectId);
  ok(cleared && cleared.data && sameIds(cleared.data.canvas_ids, []), 'canvas_ids can be cleared to an empty array');

  const seeded = await req('POST', '/api/v1/users/me/projects',
    { title: `${title} (seeded)`, pks: [], canvas_ids: [303] });
  ok(seeded.res.status === 200, `create with canvas_ids responds 200 (got ${seeded.res.status})`);
  ok(seeded.json && seeded.json.data && sameIds(seeded.json.data.canvas_ids, [303]),
    'create stores the supplied canvas_ids');

  // Trash (soft-delete)
  const trash = await req('PUT', '/api/v1/users/me/projects/trash', [projectId]);
  ok(trash.res.status === 200, `trash responds 200 (got ${trash.res.status})`);

  const listAfterTrash = await req('GET', '/api/v1/users/me/projects');
  ok(!findById(listAfterTrash.json, projectId), 'trashed project no longer appears in the default list');

  // Restore
  const restore = await req('PUT', '/api/v1/users/me/projects/restore', [projectId]);
  ok(restore.res.status === 200, `restore responds 200 (got ${restore.res.status})`);

  const listAfterRestore = await req('GET', '/api/v1/users/me/projects');
  ok(findById(listAfterRestore.json, projectId), 'restored project reappears in the projects list');
} catch (err) {
  failures += 1;
  console.error(`  ✗ request failed: ${err.message} -- is the server running with auth_check=false?`);
}

console.log(failures === 0 ? '\nPASS\n' : `\nFAIL (${failures} check(s) failed)\n`);
process.exit(failures === 0 ? 0 : 1);
