import { existsSync } from 'node:fs';
import { createHarness } from '../lib/api-harness.mjs';
import {
  check_links,
  describe_check,
  is_absolute_http_url,
  parse_args,
  DEFAULT_CONCURRENCY,
  DEFAULT_TIMEOUT_MS
} from '../lib/link-check.mjs';
import { read_json } from '#lib/common.mjs';

const CONFIGURATIONS = Object.freeze([
  'configurations/production.json',
  'configurations/test.json',
  'configurations/ci.json',
  'configurations/dev.json',
  'configurations/mock.json'
]);
const CATALOG_DIR = 'assets/biolink-model/common';
const LINK_FIELDS = Object.freeze(['url', 'wiki']);

const { ok, fail, finish } = createHarness();
const args = parse_args(process.argv.slice(2));
const concurrency = Number(args.concurrency ?? DEFAULT_CONCURRENCY);
const timeout_ms = Number(args.timeout ?? DEFAULT_TIMEOUT_MS);
const link_filter = typeof args.filter === 'string' ? args.filter.toLowerCase() : null;
const limit = typeof args.limit === 'string' ? Number(args.limit) : null;

console.log(`# infores catalog links (${CATALOG_DIR})`);
const catalog_files = typeof args.catalog === 'string' ? [args.catalog] : await configured_catalog_files();
const links = new Map();
for (const file of catalog_files) {
  await collect_catalog_links(file, links);
}
const selected = select_links(links);
console.log(`  checking ${selected.length} distinct link(s) with concurrency ${concurrency}, timeout ${timeout_ms}ms\n`);
const failed = [];
await check_links(selected, {
  concurrency: concurrency,
  timeout_ms: timeout_ms,
  on_result: result => {
    const origins = [...links.get(result.url)].join(', ');
    ok(result.ok, `${describe_check(result)}  [${origins}]`);
    if (!result.ok) failed.push({ result, origins });
  }
});
if (failed.length > 0) {
  console.log(`\n  ${failed.length} failed link(s):`);
  for (const { result, origins } of failed) {
    console.log(`    ${describe_check(result)}  [${origins}]`);
  }
}
finish();

async function configured_catalog_files() {
  const files = new Map();
  for (const config_path of CONFIGURATIONS) {
    const config = await read_json(config_path);
    const file = config.biolink?.infores_catalog;
    if (typeof file !== 'string') {
      fail(`${config_path}: no biolink.infores_catalog configured`);
      continue;
    }
    const users = files.get(file) ?? [];
    users.push(config_path);
    files.set(file, users);
  }
  for (const [file, users] of files) {
    console.log(`  ${file} <- ${users.join(', ')}`);
  }
  return [...files.keys()];
}

async function collect_catalog_links(file, links) {
  const path = `${CATALOG_DIR}/${file}`;
  if (!existsSync(path)) {
    fail(`${file}: catalog referenced by configuration is missing from ${CATALOG_DIR}`);
    return;
  }
  const catalog = await read_json(path);
  let unlinked = 0;
  let malformed = 0;
  for (const [infores, entry] of Object.entries(catalog)) {
    let linked = false;
    for (const field of LINK_FIELDS) {
      const value = entry[field];
      if (value === null || value === undefined) continue;
      if (!is_absolute_http_url(value)) {
        malformed += 1;
        fail(`${file} ${infores}.${field} is not an absolute http(s) URL: ${JSON.stringify(value)}`);
        continue;
      }
      linked = true;
      const origins = links.get(value) ?? new Set();
      origins.add(`${infores}.${field}`);
      links.set(value, origins);
    }
    if (!linked) unlinked += 1;
  }
  console.log(`  ${file}: ${Object.keys(catalog).length} entries, ${unlinked} without any link, ${malformed} malformed link(s)`);
}

function select_links(links) {
  let urls = [...links.keys()];
  if (link_filter !== null) {
    urls = urls.filter(url => {
      const origins = [...links.get(url)].join(' ').toLowerCase();
      return url.toLowerCase().includes(link_filter) || origins.includes(link_filter);
    });
  }
  if (limit !== null) urls = urls.slice(0, limit);
  return urls;
}
