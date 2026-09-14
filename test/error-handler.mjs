export { test_error_handler }

import * as ast from 'node:assert';
import { default as express } from 'express';
import { _handle_uncaught_error } from '../http_server.mjs';

const SECRET = 'connection string postgres://user:hunter2@db';

function make_app(json_options = {}) {
  const delegated = [];
  const app = express();
  app.use((req, _res, next) => {
    req.log = { info: () => {}, warn: () => {}, error: () => {} };
    next();
  });
  app.use(express.json(json_options));
  app.post('/echo', (req, res) => res.json({ body: req.body }));
  app.get('/boom', () => {
    throw new Error(`Database failed: ${SECRET}`);
  });
  app.get('/unavailable', () => {
    const err = new Error(`Upstream failed: ${SECRET}`);
    err.status = 503;
    throw err;
  });
  app.get('/late', (_req, res) => {
    res.status(200).send('already sent');
    throw new Error('thrown after the response');
  });
  app.use(_handle_uncaught_error);
  app.use((err, _req, res, _next) => {
    delegated.push(err.message);
    if (!res.headersSent) res.status(500).send('delegated');
  });
  return { app: app, delegated: delegated };
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

async function post_raw(base, body, content_type = 'application/json') {
  const res = await fetch(`${base}/echo`, {
    method: 'POST',
    headers: { 'Content-Type': content_type },
    body: body
  });
  return { status: res.status, raw: await res.text() };
}

async function test_malformed_json_is_a_client_error() {
  await with_server(make_app().app, async (base) => {
    for (const body of ['{bad json', '"a string"', 'null', '[1,2']) {
      const { status, raw } = await post_raw(base, body);
      ast.strictEqual(status, 400,
        `a malformed body (${JSON.stringify(body)}) must answer 400 (got ${status}: ${raw.slice(0, 90)})`);
      ast.strictEqual(JSON.parse(raw).status, 'error',
        'a client error must use the API error envelope');
    }
  });
}

async function test_oversized_body_reports_payload_too_large() {
  await with_server(make_app({ limit: '100b' }).app, async (base) => {
    const { status, raw } = await post_raw(base, JSON.stringify({ pad: 'x'.repeat(500) }));
    ast.strictEqual(status, 413,
      `an oversized body must answer 413 (got ${status}: ${raw.slice(0, 90)})`);
  });
}

async function test_wrong_content_type_is_still_served() {
  await with_server(make_app().app, async (base) => {
    const { status } = await post_raw(base, 'plain text', 'text/plain');
    ast.strictEqual(status, 200,
      `an unparsed body must reach the handler, not the error handler (got ${status})`);
  });
}

async function test_server_faults_stay_opaque() {
  await with_server(make_app().app, async (base) => {
    for (const path of ['/boom', '/unavailable']) {
      const res = await fetch(`${base}${path}`);
      const raw = await res.text();
      ast.strictEqual(res.status, 500, `${path} must answer 500 (got ${res.status})`);
      ast.ok(!raw.includes(SECRET),
        `${path} must not leak the error detail to the client (got ${raw.slice(0, 90)})`);
    }
  });
}

async function test_errors_after_the_response_are_delegated() {
  const { app, delegated } = make_app();
  await with_server(app, async (base) => {
    const res = await fetch(`${base}/late`);
    ast.strictEqual(res.status, 200, 'a sent response must not be rewritten');
    ast.strictEqual(await res.text(), 'already sent', 'a sent body must not be rewritten');
  });
  ast.deepStrictEqual(delegated, ['thrown after the response'],
    'an error raised after the response must pass to the next error handler');
}

async function test_error_handler() {
  console.log('START MODULE TEST http_server.mjs error handler');
  await test_malformed_json_is_a_client_error();
  await test_oversized_body_reports_payload_too_large();
  await test_wrong_content_type_is_still_served();
  await test_server_faults_stay_opaque();
  await test_errors_after_the_response_are_delegated();
  console.log('END MODULE TEST http_server.mjs error handler');
}
