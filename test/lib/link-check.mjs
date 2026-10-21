export {
  check_link,
  check_links,
  describe_check,
  describe_error,
  is_absolute_http_url,
  parse_args,
  run_pool,
  DEFAULT_CONCURRENCY,
  DEFAULT_TIMEOUT_MS
}

const DEFAULT_CONCURRENCY = 8;
const DEFAULT_TIMEOUT_MS = 20000;
const MAX_BODY_BYTES = 512 * 1024;
const RETRY_STATUSES = new Set([429, 500, 502, 503, 504]);
const USER_AGENT = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0 Safari/537.36 translator-ui-link-check';
const ACCEPT = 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8';
const ERROR_TITLE_PATTERN = /\b(not found|404|403|500|page (cannot|can't|could not) be (found|displayed)|error|access denied|forbidden|unavailable|does not exist|no longer available|invalid)\b/i;

async function check_link(url, { timeout_ms = DEFAULT_TIMEOUT_MS, retries = 1 } = {}) {
  let attempt = 0;
  while (true) {
    const result = await _fetch_once(url, timeout_ms);
    if (result.ok || attempt >= retries || !_is_retryable(result)) return result;
    attempt += 1;
    await new Promise(resolve => setTimeout(resolve, 1000 * attempt));
  }
}

async function check_links(urls, { concurrency = DEFAULT_CONCURRENCY, timeout_ms = DEFAULT_TIMEOUT_MS, on_result = null } = {}) {
  return run_pool(urls, concurrency, async (url, index) => {
    const result = await check_link(url, { timeout_ms });
    if (on_result) on_result(result, index);
    return result;
  });
}

async function run_pool(items, concurrency, fn) {
  const results = new Array(items.length);
  let next = 0;
  async function worker() {
    while (next < items.length) {
      const index = next;
      next += 1;
      results[index] = await fn(items[index], index);
    }
  }
  const workers = Array.from({ length: Math.max(1, Math.min(concurrency, items.length)) }, worker);
  await Promise.all(workers);
  return results;
}

function describe_check(result) {
  const status = result.status === null ? '---' : String(result.status);
  const redirect = result.final_url && result.final_url !== result.url ? ` -> ${result.final_url}` : '';
  const title = result.title ? ` ("${result.title}")` : '';
  const problems = result.problems.length > 0 ? ` : ${result.problems.join('; ')}` : '';
  return `${status} ${result.url}${redirect}${title}${problems}`;
}

function describe_error(err) {
  if (err === null || err === undefined) return 'unknown error';
  if (err.status !== undefined && err.url !== undefined) return `HTTP ${err.status} ${err.method} ${err.url}`;
  const cause = err.cause ? ` (${err.cause.code ?? err.cause.message ?? err.cause})` : '';
  const upstream = err.upstreamError ? ` <- ${describe_error(err.upstreamError)}` : '';
  return `${err.name ?? 'Error'}: ${err.message}${cause}${upstream}`;
}

function is_absolute_http_url(value) {
  if (typeof value !== 'string') return false;
  try {
    const parsed = new URL(value);
    return parsed.protocol === 'http:' || parsed.protocol === 'https:';
  } catch {
    return false;
  }
}

function parse_args(argv) {
  const args = { verbose: process.env.VERBOSE === '1' };
  for (let i = 0; i < argv.length; i++) {
    const token = argv[i];
    if (token === '-v' || token === '--verbose') {
      args.verbose = true;
    } else if (token.startsWith('--')) {
      const key = token.slice(2).replaceAll('-', '_');
      const next = argv[i + 1];
      if (next === undefined || next.startsWith('-')) {
        args[key] = true;
      } else {
        args[key] = next;
        i += 1;
      }
    }
  }
  return args;
}

async function _fetch_once(url, timeout_ms) {
  const result = {
    url: url,
    ok: false,
    status: null,
    final_url: null,
    content_type: null,
    title: null,
    problems: []
  };
  try {
    const response = await fetch(url, {
      method: 'GET',
      redirect: 'follow',
      signal: AbortSignal.timeout(timeout_ms),
      headers: { 'User-Agent': USER_AGENT, 'Accept': ACCEPT }
    });
    result.status = response.status;
    result.final_url = response.url;
    result.content_type = response.headers.get('content-type');
    const body = await _read_prefix(response, MAX_BODY_BYTES);
    if (response.status !== 200) {
      result.problems.push(`HTTP ${response.status}${response.statusText ? ` ${response.statusText}` : ''}`);
    }
    if (_looks_like_html(result.content_type, body)) {
      result.title = _extract_title(body);
      result.problems.push(..._html_problems(body, result.title));
    } else if (body.length === 0) {
      result.problems.push('empty response body');
    }
  } catch (err) {
    result.problems.push(_describe_fetch_error(err, timeout_ms));
  }
  result.ok = result.problems.length === 0;
  return result;
}

async function _read_prefix(response, max_bytes) {
  if (response.body === null) return '';
  const reader = response.body.getReader();
  const decoder = new TextDecoder('utf-8', { fatal: false });
  let text = '';
  let bytes = 0;
  try {
    while (bytes < max_bytes) {
      const { done, value } = await reader.read();
      if (done) break;
      bytes += value.byteLength;
      text += decoder.decode(value, { stream: true });
    }
  } finally {
    await reader.cancel().catch(() => undefined);
  }
  return text;
}

function _looks_like_html(content_type, body) {
  if (content_type && /text\/html|application\/xhtml/i.test(content_type)) return true;
  return /^\s*(<!doctype html|<html)/i.test(body);
}

function _extract_title(body) {
  const match = body.match(/<title[^>]*>([\s\S]*?)<\/title>/i);
  if (!match) return null;
  return match[1].replace(/\s+/g, ' ').trim();
}

function _html_problems(body, title) {
  const problems = [];
  if (body.trim().length < 100) {
    problems.push('HTML body is nearly empty');
  }
  if (title === null && !/<body[\s>]/i.test(body)) {
    problems.push('HTML has neither a <title> nor a <body>');
  }
  if (title !== null && ERROR_TITLE_PATTERN.test(title)) {
    problems.push(`error-like title "${title}"`);
  }
  return problems;
}

function _describe_fetch_error(err, timeout_ms) {
  if (err.name === 'TimeoutError' || err.name === 'AbortError') return `timed out after ${timeout_ms}ms`;
  return describe_error(err);
}

function _is_retryable(result) {
  if (result.status !== null) return RETRY_STATUSES.has(result.status);
  const reason = result.problems[0] ?? '';
  return reason.startsWith('timed out') || reason.includes('fetch failed');
}
