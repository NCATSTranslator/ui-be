import { createHarness } from '../lib/api-harness.mjs';
import { describe_error, parse_args, run_pool } from '../lib/link-check.mjs';
import {
  group_diagnostics,
  has_result_data,
  load_environment_config,
  load_pipeline,
  summarize
} from '../lib/summary-pipeline.mjs';
import { ARSClient } from '#lib/ARSClient.mjs';
import { QUERY_STATUS } from '#lib/common.mjs';

const ENVIRONMENTS = Object.freeze({
  production: 'configurations/production.json',
  test: 'configurations/test.json',
  ci: 'configurations/ci.json',
  dev: 'configurations/dev.json'
});
const ARA_FILTERS = Object.freeze({ whitelistRx: /^ara-/ });
const DEFAULT_CONCURRENCY = 2;
const MAX_SHOWN_PER_AGENT = 3;
const MAX_SHOWN_DIAGNOSTICS = 8;

const { ok, fail, finish } = createHarness();
const args = parse_args(process.argv.slice(2));
const concurrency = Number(args.concurrency ?? DEFAULT_CONCURRENCY);
const query_filter = typeof args.query === 'string' ? args.query : null;

const environments = select_environments(args.env);
for (const [env_name, config_path] of environments) {
  await test_environment(env_name, config_path);
}
finish();

function select_environments(requested) {
  if (typeof requested !== 'string') return Object.entries(ENVIRONMENTS);
  const selected = [];
  for (const name of requested.split(',').map(part => part.trim()).filter(part => part !== '')) {
    if (ENVIRONMENTS[name] === undefined) {
      fail(`unknown environment "${name}" (known: ${Object.keys(ENVIRONMENTS).join(', ')})`);
      continue;
    }
    selected.push([name, ENVIRONMENTS[name]]);
  }
  return selected;
}

async function test_environment(env_name, config_path) {
  console.log(`\n# ${env_name} example queries (${config_path})`);
  let config;
  try {
    config = await load_environment_config(config_path);
  } catch (err) {
    fail(`[${env_name}] cannot load configuration: ${describe_error(err)}`);
    return;
  }
  let adapter;
  try {
    adapter = await load_pipeline(config);
  } catch (err) {
    fail(`[${env_name}] cannot load the summarization pipeline: ${describe_error(err)}`);
    return;
  }
  const client = new ARSClient(config.ars_endpoint, null);
  const queries = config.frontend.cached_queries.filter(query => matches_filter(query, query_filter));
  console.log(`  ${queries.length} of ${config.frontend.cached_queries.length} example queries against ${config.ars_endpoint.protocol}://${config.ars_endpoint.host}`);
  if (queries.length === 0) {
    fail(`[${env_name}] no example queries selected`);
    return;
  }
  await run_pool(queries, concurrency, query => test_query(env_name, client, adapter, query));
}

function matches_filter(query, filter) {
  if (filter === null) return true;
  const needle = filter.toLowerCase();
  return query.uuid === filter
    || query.id === filter
    || query.name.toLowerCase().includes(needle);
}

async function test_query(env_name, client, adapter, query) {
  const label = `[${env_name}] ${query.name.trim()} <${query.type} ${query.id}> ${query.uuid}`;
  const started = Date.now();
  let message;
  try {
    message = await client.getQueryResults(query.uuid, ARA_FILTERS);
  } catch (err) {
    fail(`${label}: cannot retrieve results: ${describe_error(err)}`);
    return;
  }
  if (message.queuing) {
    fail(`${label}: the ARS reports the query as still queuing`);
    return;
  }
  const completed = message.completed.filter(has_result_data);
  if (completed.length === 0) {
    fail(`${label}: no completed results (running: ${agents(message.running)}, errored: ${agents(message.errored)})`);
    return;
  }
  let summary;
  let diagnostics;
  try {
    ({ value: summary, diagnostics } = await summarize(adapter, message));
  } catch (err) {
    fail(`${label}: processing threw: ${describe_error(err)}`);
    return;
  }
  const data = summary.data;
  const elapsed = ((Date.now() - started) / 1000).toFixed(1);
  const counts = `${data.results.length} results, ${Object.keys(data.nodes).length} nodes, ${Object.keys(data.edges).length} edges, ${Object.keys(data.provenance).length} sources in ${elapsed}s`;
  ok(data.results.length > 0, `${label}: ${counts}`);
  report_warnings(label, message, summary);
  report_diagnostics(label, diagnostics);
}

function report_warnings(label, message, summary) {
  if (summary.status !== QUERY_STATUS.COMPLETE) {
    warn(`${label}: query status is "${summary.status}"`);
  }
  if (message.running.length > 0) {
    warn(`${label}: ${message.running.length} agent(s) still running: ${agents(message.running)}`);
  }
  if (message.errored.length > 0) {
    warn(`${label}: ${message.errored.length} agent(s) errored: ${agents(message.errored)}`);
  }
  for (const [agent, errors] of Object.entries(summary.data.errors)) {
    const distinct = [...new Set(errors.map(error => typeof error === 'string' ? error : JSON.stringify(error)))];
    const shown = distinct.slice(0, MAX_SHOWN_PER_AGENT).map(error => `"${error}"`).join(', ');
    const more = distinct.length > MAX_SHOWN_PER_AGENT ? `, +${distinct.length - MAX_SHOWN_PER_AGENT} more` : '';
    warn(`${label}: ${errors.length} summarization error(s) from ${agent}: ${shown}${more}`);
  }
}

function report_diagnostics(label, diagnostics) {
  const groups = group_diagnostics(diagnostics);
  for (const group of groups.slice(0, MAX_SHOWN_DIAGNOSTICS)) {
    const times = group.count > 1 ? ` (x${group.count})` : '';
    warn(`${label}: logged ${group.level}${times}: ${group.example}`);
  }
  if (groups.length > MAX_SHOWN_DIAGNOSTICS) {
    warn(`${label}: +${groups.length - MAX_SHOWN_DIAGNOSTICS} more distinct logged warning/error group(s)`);
  }
}

function agents(entries) {
  if (entries.length === 0) return 'none';
  return entries.map(entry => entry.agent ?? JSON.stringify(entry)).join(', ');
}

function warn(message) {
  console.warn(`  ! ${message}`);
}
