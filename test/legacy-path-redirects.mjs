export { test_legacy_path_redirects }

import * as ast from 'node:assert';
import { default as express } from 'express';
import { _legacy_prefix_redirect } from '../http_server.mjs';

const LEGACY_ROUTES = ['/main{/*splat}', '/demo{/*splat}'];

function make_app() {
  const errors = [];
  const app = express();
  app.all(LEGACY_ROUTES, _legacy_prefix_redirect);
  app.all('/{*splat}', (_req, res) => res.status(200).send('spa-skeleton'));
  app.use((err, _req, res, _next) => {
    errors.push(err.code || err.message);
    if (res.headersSent) {
      res.destroy();
      return;
    }
    res.status(500).send('uncaught');
  });
  return { app: app, errors: errors };
}

async function with_server(app, run) {
  const server = app.listen(0, '127.0.0.1');
  await new Promise((resolve) => server.once('listening', resolve));
  try {
    return await run(`http://127.0.0.1:${server.address().port}`);
  } finally {
    await new Promise((resolve) => server.close(resolve));
  }
}

async function redirect_of(base, path) {
  const res = await fetch(`${base}${path}`, { redirect: 'manual' });
  const location = res.headers.get('location');
  return { status: res.status, location: location, res: res };
}

async function test_legacy_prefixes_redirect_to_the_stripped_path() {
  const { app, errors } = make_app();
  const cases = [
    ['/main', '/'],
    ['/demo', '/'],
    ['/main/foo', '/foo'],
    ['/demo/disease/MONDO:0005148', '/disease/MONDO:0005148'],
    ['/main/foo?a=1', '/foo?a=1']
  ];
  await with_server(app, async (base) => {
    for (const [path, expected] of cases) {
      const { status, location } = await redirect_of(base, path);
      ast.strictEqual(status, 308, `${path} must answer 308 (got ${status})`);
      ast.strictEqual(location, expected,
        `${path} must redirect to ${expected} (got ${location})`);
    }
  });
  ast.deepStrictEqual(errors, [],
    'redirecting a legacy path must not raise an error after the response is sent');
}

async function test_redirects_never_point_back_at_themselves() {
  const { app, errors } = make_app();
  const paths = ['/main', '/demo', '/main?a=1', '/demo?a=1', '/main/foo?a=1', '/demo/disease/x?b=2'];
  await with_server(app, async (base) => {
    for (const path of paths) {
      const { status, location } = await redirect_of(base, path);
      ast.strictEqual(status, 308, `${path} must answer 308 (got ${status})`);
      const resolved = new URL(location, `${base}${path}`);
      ast.notStrictEqual(`${resolved.pathname}${resolved.search}`, path,
        `${path} must not redirect to itself (Location: ${location})`);
      ast.ok(location.startsWith('/'),
        `${path} must redirect to an absolute path (got ${location})`);
    }
  });
  ast.deepStrictEqual(errors, [], 'a legacy redirect must not raise an error');
}

async function test_non_legacy_paths_are_left_alone() {
  await with_server(make_app().app, async (base) => {
    for (const path of ['/', '/mainframe', '/demography', '/api/v1/config']) {
      const { status } = await redirect_of(base, path);
      ast.strictEqual(status, 200, `${path} must not be redirected (got ${status})`);
    }
  });
}

async function test_legacy_path_redirects() {
  console.log('START MODULE TEST legacy /main and /demo redirects');
  await test_legacy_prefixes_redirect_to_the_stripped_path();
  await test_redirects_never_point_back_at_themselves();
  await test_non_legacy_paths_are_left_alone();
  console.log('END MODULE TEST legacy /main and /demo redirects');
}
