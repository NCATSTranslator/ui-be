import { existsSync } from 'node:fs';
import { createHarness } from '../lib/api-harness.mjs';
import {
  check_links,
  describe_check,
  describe_error,
  describe_outcome,
  parse_args,
  write_csv_report,
  DEFAULT_CONCURRENCY,
  DEFAULT_REPORT_DIR,
  DEFAULT_TIMEOUT_MS
} from '../lib/link-check.mjs';
import { group_diagnostics, load_pipeline, summarize } from '../lib/summary-pipeline.mjs';
import { read_json } from '#lib/common.mjs';
import { SOURCES } from '#lib/biothings-annotation.mjs';
import { ARSClient } from '../../mock/client.mjs';

const MOCK_CONFIG = 'configurations/mock.json';
const DEFAULT_PER_GROUP = 3;
const SAMPLE_SOURCE_IDS = Object.freeze({
  chebi: 'CHEBI:15365',
  chembl: 'CHEMBL25',
  disease_ontology: 'DOID:1612',
  mondo: 'MONDO:0007254',
  ncbi_gene: '1017',
  ncbi_taxonomy: '10090',
  ncit: 'C287'
});

const { ok, fail, finish } = createHarness();
const args = parse_args(process.argv.slice(2));
const concurrency = Number(args.concurrency ?? DEFAULT_CONCURRENCY);
const timeout_ms = Number(args.timeout ?? DEFAULT_TIMEOUT_MS);
const per_group = args.all === true ? Infinity : Number(args.per_group ?? DEFAULT_PER_GROUP);
const mock_data_path = typeof args.mock_data === 'string' ? args.mock_data : null;
const report_dir = typeof args.report_dir === 'string' ? args.report_dir : DEFAULT_REPORT_DIR;

const links = new Map();
collect_source_table_links(links);
await collect_mock_result_links(links);
const selected = select_links(links);
console.log(`\n# checking ${selected.length} of ${links.size} distinct annotation link(s) with concurrency ${concurrency}, timeout ${timeout_ms}ms\n`);
const failed = [];
const results = await check_links(selected, {
  concurrency: concurrency,
  timeout_ms: timeout_ms,
  on_result: result => {
    const origin = links.get(result.url);
    ok(result.ok, `${describe_check(result)}  [${origin.group}]`);
    if (!result.ok) failed.push(result);
  }
});
if (failed.length > 0) {
  console.log(`\n  ${failed.length} failed link(s):`);
  for (const result of failed) {
    console.log(`    ${describe_check(result)}  [${links.get(result.url).group}]`);
  }
}
const report_rows = results.map(result => ({
  link: result.url,
  source: links.get(result.url).group,
  succeeded: result.ok,
  reason: describe_outcome(result)
}));
console.log(`\n  report: ${await write_csv_report(report_dir, 'node-annotations', report_rows)}`);
finish();

function collect_source_table_links(links) {
  console.log('# annotation source table (lib/biothings-annotation.mjs SOURCES)');
  for (const source of Object.values(SOURCES)) {
    register(links, source.url, `source:${source.id}`);
    if (source.link === undefined) continue;
    const sample_id = SAMPLE_SOURCE_IDS[source.id];
    if (sample_id === undefined) {
      fail(`source ${source.id} builds per-id links but has no sample id in this test`);
      continue;
    }
    register(links, source.link(sample_id), `source:${source.id}`);
  }
  console.log(`  ${links.size} link(s) from ${Object.keys(SOURCES).length} sources`);
}

async function collect_mock_result_links(links) {
  console.log('\n# annotation links in summarized mock results');
  const config = await read_json(MOCK_CONFIG);
  const data_path = mock_data_path ?? config.ars_endpoint.data_path;
  if (!existsSync(data_path)) {
    fail(`mock result data is missing at ${data_path}; pass --mock-data <dir> to use another location`);
    return;
  }
  let adapter;
  try {
    adapter = await load_pipeline(config);
  } catch (err) {
    fail(`cannot load the summarization pipeline: ${describe_error(err)}`);
    return;
  }
  const client = new ARSClient(data_path);
  const missing_prefixes = new Map();
  const before_mock = links.size;
  for (const [name, pk] of client.query_pks) {
    const before = links.size;
    let summary;
    let diagnostics;
    try {
      const message = await client.getQueryResults(pk, {});
      ({ value: summary, diagnostics } = await summarize(adapter, message));
    } catch (err) {
      console.warn(`  ! mock query ${name} could not be summarized and was skipped: ${describe_error(err)}`);
      continue;
    }
    for (const node of Object.values(summary.data.nodes)) {
      collect_node_links(node, links, missing_prefixes);
    }
    const errors = group_diagnostics(diagnostics).filter(group => group.level === 'error');
    const error_note = errors.length > 0 ? `, ${errors.length} logged error group(s) during summarization` : '';
    console.log(`  mock query ${name}: ${Object.keys(summary.data.nodes).length} nodes, ${links.size - before} new link(s)${error_note}`);
  }
  if (links.size === before_mock) {
    fail(`no annotation links were found in any mock result under ${data_path}`);
  }
  for (const [prefix, count] of missing_prefixes) {
    console.warn(`  ! prefix ${prefix}: ${count} curie(s) have no URL because the prefix catalog has no entry`);
  }
}

function collect_node_links(node, links, missing_prefixes) {
  for (const annotation of Object.values(node.annotations ?? {})) {
    for (const [section_name, section] of Object.entries(annotation ?? {})) {
      if (section === null || section === undefined) continue;
      for (const source of section.metadata?.sources ?? []) {
        register(links, source.url, `source:${source.id}`);
      }
      if (section_name === 'curies') {
        for (const entry of section.value) {
          const prefix = entry.curie.split(':')[0];
          if (entry.url === null) {
            missing_prefixes.set(prefix, (missing_prefixes.get(prefix) ?? 0) + 1);
          } else {
            register(links, entry.url, `prefix:${prefix}`);
          }
        }
      }
      if (section_name === 'indications') {
        for (const indication of section.value) {
          for (const url of indication.urls) {
            register(links, url, `indication:${host_of(url)}`);
          }
        }
      }
    }
  }
  for (const url of node.provenance) {
    register(links, url, `provenance:${host_of(url)}`);
  }
}

function register(links, url, group) {
  if (typeof url !== 'string' || url === '') return;
  if (links.has(url)) return;
  links.set(url, { group });
}

function host_of(url) {
  try {
    return new URL(url).host;
  } catch {
    return 'invalid-url';
  }
}

function select_links(links) {
  const taken = new Map();
  const selected = [];
  for (const [url, { group }] of links) {
    const count = taken.get(group) ?? 0;
    if (count >= per_group) continue;
    taken.set(group, count + 1);
    selected.push(url);
  }
  return selected;
}
