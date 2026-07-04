#!/usr/bin/env node
// otel-trace-view.mjs — tail an OpenTelemetry ConsoleSpanExporter log (optionally
// interleaved with pino-pretty application logs) and render each completed trace as a
// span tree + waterfall, with a visual separator between traces.
//
//   usage:  node otel-trace-view.mjs <logfile>       (^C to quit)
//
// The file may be static or actively appended to (a live log). The ConsoleSpanExporter
// prints every span as a util.inspect object whose opening `{` and closing `}` sit in
// column 0 — every other line (span innards, and the surrounding pino log lines) is
// indented. We use that to carve span blocks out of the stream, buffer spans by traceId,
// and emit a trace only once it is whole: its root span plus every referenced parent are
// present. Because a child span always ends (and is queued for export) before its parent,
// seeing the root with no dangling parent refs means no descendant can still be missing.
//
// State is kept across reads (byte offset, partial-line carry, in-progress block, and the
// per-trace span buffers) so a trace split across several appends is assembled correctly.

import fs from 'node:fs';

// ---- tunables --------------------------------------------------------------
const POLL_MS        = 200;    // how often we poll the file for new bytes
const SETTLE_MS      = 250;    // quiet period after a trace looks complete, before emit
const IDLE_FLUSH_MS  = 10000;  // rootless / oddball traces: emit after this much silence
const BAR_W          = 44;     // waterfall timeline width, in columns
const NAMEW          = 40;     // tree: width of the "prefix + name" column
const NOTEW          = 42;     // tree: width of the key-attribute column
const HEADER_W       = 100;    // width of the ═ separator rules

// ---- tiny ANSI helpers (no-op when not a TTY, honours NO_COLOR) -------------
const COLOR = process.stdout.isTTY && !process.env.NO_COLOR;
const sgr   = (code, s) => COLOR ? `\x1b[${code}m${s}\x1b[0m` : String(s);
const bold  = s => sgr(1, s);
const dim   = s => sgr(2, s);
const green = s => sgr(32, s);
const cyan  = s => sgr(36, s);

// ---- span kind (OTel SpanKind: 0 INTERNAL,1 SERVER,2 CLIENT,3 PRODUCER,4 CONSUMER)
const HTTP_METHOD = /^(GET|POST|PUT|DELETE|PATCH|HEAD|OPTIONS|TRACE|CONNECT)\b/;
function kindTag(s) {
  if (s.kind === 1) return ' [server]';
  if (s.kind === 2 && HTTP_METHOD.test(s.name)) return ' [client]';
  if (s.kind === 3) return ' [producer]';
  if (s.kind === 4) return ' [consumer]';
  return '';
}

// ---- string helpers --------------------------------------------------------
const UUID = /[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}/gi;
const shortUuids = s => String(s).replace(UUID, m => m.slice(0, 8) + '…');
const shortTid   = t => t.length > 12 ? t.slice(0, 8) + '…' + t.slice(-4) : t;
const shortHost  = h => h && h.length > 14 ? h.split('.')[0] + '…' : h;
const truncate   = (s, w) => s.length > w ? s.slice(0, w - 1) + '…' : s;
const padTrunc   = (s, w) => (s = s ?? '', s.length > w ? s.slice(0, w - 1) + '…' : s + ' '.repeat(w - s.length));
const fmtMs      = ms => ms.toFixed(1) + 'ms';

// ---- parse one inspected span block into just the fields we render ----------
function parseSpan(text) {
  let o;
  try { o = new Function('return (' + text + ')')(); } catch { return null; }
  if (!o || typeof o !== 'object') return null;
  if (typeof o.traceId !== 'string' || typeof o.id !== 'string') return null;
  if (typeof o.timestamp !== 'number' || typeof o.duration !== 'number') return null;
  return {
    traceId:  o.traceId,
    id:       o.id,
    parentId: o.parentSpanContext ? o.parentSpanContext.spanId : null,
    name:     String(o.name ?? '?'),
    kind:     o.kind | 0,
    ts:       o.timestamp,   // epoch microseconds
    dur:      o.duration,    // microseconds (may be fractional)
    attrs:    o.attributes || {},
    status:   o.status ? o.status.code : 0,
    service:  o.resource?.attributes?.['service.name'] || '',
  };
}

// ---- build the key-attribute "note" shown beside each span ------------------
// `peer` is an inferred {host,port} for the span (own attrs, else a descendant's) so
// that e.g. a tls.connect can name the host its child tcp.connect actually dialed.
function noteFor(s, ipHost, peer = { host: '', port: '' }) {
  const a = s.attrs;
  const code = a['http.status_code'] ?? a['http.response.status_code'];
  const status = code != null ? ' →' + code : '';
  const portOf = own => { const p = own != null ? own : peer.port; return p !== '' && p != null ? ':' + p : ''; };

  // database
  if (a['db.system']) {
    if (a['db.statement']) return shortUuids(a['db.statement']);
    const who  = a['db.user'] || a['db.name'] || '';
    const host = a['net.peer.name'] || peer.host || '';
    return `${who}@${host}${portOf(a['net.peer.port'])}`;
  }
  // outbound HTTP (undici): url.* attributes
  if (a['url.full'] || a['url.path']) {
    const host = shortHost(a['server.address'] || '');
    const path = a['url.path'] || '';
    const q    = a['url.query'] || '';
    return shortUuids(`${host}${path}${q}`) + status;
  }
  // inbound HTTP (node http): http.target / http.url
  if (a['http.target'] || a['http.url']) {
    const ua  = (a['http.user_agent'] || '').split('/')[0];
    const tgt = a['http.target'] || a['http.url'] || '';
    return (ua ? ua + ' → ' : '') + shortUuids(tgt) + status;
  }
  // tcp / tls / generic socket
  if (a['tls.protocol'] || a['net.peer.name'] || a['net.peer.ip'] || s.name === 'tcp.connect' || s.name === 'tls.connect') {
    const host  = a['net.peer.name'] || a['net.peer.ip'] || peer.host || '';
    const proto = a['tls.protocol'] ? ' ' + a['tls.protocol'] : '';
    return (host ? host + portOf(a['net.peer.port']) : '').concat(proto).trim();
  }
  // dns: enrich the resolved ip with a hostname harvested from a sibling span
  if (s.name === 'dns.lookup') {
    const ip = a['peer.ipv4'] || a['peer.ipv6'];
    if (ip) { const h = ipHost.get(ip); return h ? `${h} → ${ip}` : String(ip); }
  }
  // fallback: first attribute
  const k = Object.keys(a)[0];
  return k ? `${k}=${shortUuids(a[k])}` : '';
}

// per-span inferred peer {host,port}: use the span's own socket attrs, else borrow from
// the nearest descendant that has them (tls.connect → its child tcp.connect, etc.)
function inferPeers(spans, kids) {
  const peer = new Map();
  const compute = s => {
    const hit = peer.get(s.id);
    if (hit) return hit;
    const v = {
      host: s.attrs['net.peer.name'] || s.attrs['server.address'] || '',
      port: s.attrs['net.peer.port'] != null ? s.attrs['net.peer.port'] : '',
    };
    peer.set(s.id, v);                       // set before recursing to break cycles
    if (!v.host || v.port === '') {
      for (const c of (kids.get(s.id) || [])) {
        const p = compute(c);
        if (!v.host && p.host) v.host = p.host;
        if (v.port === '' && p.port !== '') v.port = p.port;
        if (v.host && v.port !== '') break;
      }
    }
    return v;
  };
  for (const s of spans) compute(s);
  return peer;
}

// map a resolved ip -> hostname, harvested from any span in the trace that has both,
// so bare dns.lookup spans can show which host they resolved.
function ipHostMap(spans) {
  const m = new Map();
  for (const s of spans) {
    const ip   = s.attrs['net.peer.ip'] || s.attrs['network.peer.address'];
    const host = s.attrs['net.peer.name'] || s.attrs['server.address'];
    if (ip && host) m.set(ip, host);
  }
  return m;
}

// ---- lay the trace out as an ordered depth-first forest ---------------------
function layout(t) {
  const byId = t.spans;
  const kids = new Map(), roots = [];
  for (const s of byId.values()) {
    if (s.parentId && byId.has(s.parentId)) (kids.get(s.parentId) || kids.set(s.parentId, []).get(s.parentId)).push(s);
    else roots.push(s);                       // true root, or a child whose parent we never saw
  }
  const byStart = (a, b) => a.ts - b.ts || (a.id < b.id ? -1 : 1);
  roots.sort(byStart);
  for (const arr of kids.values()) arr.sort(byStart);

  const rows = [];
  const soleRoot = roots.length === 1;
  const walk = (s, prefix, isLast, isRoot) => {
    rows.push({ s, prefix, isLast, isRoot });
    const cs = kids.get(s.id) || [];
    const childPrefix = prefix + (isRoot ? '' : (isLast ? '   ' : '│  '));
    cs.forEach((c, i) => walk(c, childPrefix, i === cs.length - 1, false));
  };
  roots.forEach((r, i) => walk(r, '', i === roots.length - 1, soleRoot));
  return rows;
}

// microsecond time window spanned by the trace
function timeScale(spans) {
  let t0 = Infinity, t1 = -Infinity;
  for (const s of spans) { if (s.ts < t0) t0 = s.ts; const e = s.ts + s.dur; if (e > t1) t1 = e; }
  return { t0, T: Math.max(1, t1 - t0) };
}

// one waterfall bar: leading '.' padding to the start offset, then '#' for the duration
function bar(offMs, durMs, msPerCol) {
  let s = Math.round(offMs / msPerCol);
  let w = Math.max(1, Math.round(durMs / msPerCol));
  if (s > BAR_W - 1) s = BAR_W - 1;
  if (s + w > BAR_W) w = BAR_W - s;
  const cells = '.'.repeat(s) + '#'.repeat(Math.max(1, w));
  const fixed = cells.length < BAR_W ? cells + ' '.repeat(BAR_W - cells.length) : cells.slice(0, BAR_W);
  return COLOR ? fixed.replace(/#+/g, m => green(m)).replace(/\.+/g, m => dim(m)) : fixed;
}

// ---- render one whole trace ------------------------------------------------
function renderTrace(id, t) {
  const spans   = [...t.spans.values()];
  const rows    = layout(t);
  const ipHost  = ipHostMap(spans);
  const kids    = new Map();
  for (const s of spans) if (s.parentId && t.spans.has(s.parentId)) (kids.get(s.parentId) || kids.set(s.parentId, []).get(s.parentId)).push(s);
  const peers   = inferPeers(spans, kids);
  const note    = s => noteFor(s, ipHost, peers.get(s.id));
  const { t0, T } = timeScale(spans);
  const totalMs = T / 1000;
  const rootRow = rows.find(r => r.isRoot);
  const svc     = spans.find(s => s.service)?.service || 'unknown';
  const out     = [];
  const rule    = dim('═'.repeat(HEADER_W));

  // separator + trace header
  out.push('', rule);
  out.push(' ' + [
    'trace ' + bold(cyan(shortTid(id))),
    svc,
    spans.length + ' spans',
    fmtMs(totalMs),
  ].join(dim('   ·   ')));
  if (rootRow) out.push(' ' + dim('root: ') + rootRow.s.name + kindTag(rootRow.s) + '  ' + note(rootRow.s));
  out.push(rule, '');

  // span tree
  out.push('  ' + bold('span tree'));
  for (const r of rows) {
    const branch = r.isRoot ? '' : (r.isLast ? '└─ ' : '├─ ');
    const label  = r.prefix + branch + r.s.name + kindTag(r.s);
    const offMs  = Math.round((r.s.ts - t0) / 1000);
    out.push('  ' +
      padTrunc(label, NAMEW) + ' ' +
      padTrunc(note(r.s), NOTEW) + ' ' +
      fmtMs(r.s.dur / 1000).padStart(9) + '  ' +
      dim('+' + offMs + 'ms') +
      (r.isRoot ? dim('  ← root') : ''));
  }
  out.push('');

  // waterfall
  const msPerCol = totalMs / BAR_W;
  const total    = fmtMs(totalMs);
  out.push('  ' + bold('waterfall') + dim('   (# active · . waiting)'));
  out.push('  ' + 'time →'.padEnd(9) + '0ms' + ' '.repeat(Math.max(1, BAR_W - 3 - total.length)) + total);
  out.push('  ' + ' '.repeat(9) + '|' + dim('─'.repeat(BAR_W)) + '|');
  for (const r of rows) {
    const offMs = (r.s.ts - t0) / 1000;
    const durMs = r.s.dur / 1000;
    const label = r.s.name + kindTag(r.s) + '  ' + note(r.s);
    out.push('  ' + fmtMs(durMs).padStart(8) + ' |' + bar(offMs, durMs, msPerCol) + '| ' + dim(truncate(label, 52)));
  }
  process.stdout.write(out.join('\n') + '\n');
}

// ---- streaming state -------------------------------------------------------
const traces  = new Map();   // traceId -> { spans:Map, firstSeen, lastAt }
const flushed = new Set();   // traceIds already emitted (drop late stragglers)
let seq = 0;                 // first-seen ordering for deterministic emit order

let pos = 0;                 // byte offset consumed so far
let carry = '';              // partial trailing line from the last read
let inBlock = false;         // are we inside a column-0 { ... } span block?
let block = [];              // accumulated lines of the current block

function addSpan(s) {
  if (flushed.has(s.traceId)) return;
  let t = traces.get(s.traceId);
  if (!t) traces.set(s.traceId, t = { spans: new Map(), firstSeen: seq++, lastAt: Date.now() });
  if (!t.spans.has(s.id)) t.spans.set(s.id, s);
  t.lastAt = Date.now();
}

function onLine(line) {
  if (inBlock) {
    if (line === '{') { block = ['{']; return; }        // prior block was junk; restart
    block.push(line);
    if (line === '}') {                                  // column-0 close = end of span
      const span = parseSpan(block.join('\n'));
      inBlock = false; block = [];
      if (span) addSpan(span);
    } else if (block.length > 800) { inBlock = false; block = []; }   // safety valve
    return;
  }
  if (line === '{') { inBlock = true; block = ['{']; }   // otherwise: a log line, ignore
}

function readNewData() {
  let st;
  try { st = fs.statSync(FILE); } catch { return; }
  if (st.size < pos) { pos = 0; carry = ''; inBlock = false; block = []; }   // truncated / rotated
  if (st.size === pos) return;
  const fd = fs.openSync(FILE, 'r');
  try {
    const buf = Buffer.allocUnsafe(st.size - pos);
    const n = fs.readSync(fd, buf, 0, buf.length, pos);
    pos += n;
    carry += buf.toString('utf8', 0, n);
  } finally { fs.closeSync(fd); }

  let nl;
  while ((nl = carry.indexOf('\n')) !== -1) {
    let line = carry.slice(0, nl);
    if (line.endsWith('\r')) line = line.slice(0, -1);
    carry = carry.slice(nl + 1);
    onLine(line);
  }
}

// a trace is complete once a root span exists and no child references a missing parent
function isComplete(t) {
  let hasRoot = false;
  for (const s of t.spans.values()) {
    if (s.parentId === null) hasRoot = true;
    else if (!t.spans.has(s.parentId)) return false;
  }
  return hasRoot;
}

function tick() {
  readNewData();
  const now = Date.now();
  const ready = [];
  for (const [id, t] of traces) {
    const idle = now - t.lastAt;
    if (isComplete(t) ? idle >= SETTLE_MS : idle >= IDLE_FLUSH_MS) ready.push([id, t]);
  }
  ready.sort((a, b) => a[1].firstSeen - b[1].firstSeen);
  for (const [id, t] of ready) { renderTrace(id, t); traces.delete(id); flushed.add(id); }
}

// ---- main ------------------------------------------------------------------
const FILE = process.argv[2];
if (!FILE) { console.error('usage: node otel-trace-view.mjs <logfile>'); process.exit(1); }
if (!fs.existsSync(FILE)) { console.error('otel-trace-view: no such file: ' + FILE); process.exit(1); }

process.stderr.write(dim(`otel-trace-view · watching ${FILE} · ^C to quit\n`));
const timer = setInterval(tick, POLL_MS);
tick();

process.on('SIGINT', () => {
  clearInterval(timer);
  const rest = [...traces.entries()].filter(([, t]) => isComplete(t)).sort((a, b) => a[1].firstSeen - b[1].firstSeen);
  for (const [id, t] of rest) renderTrace(id, t);
  process.stdout.write('\n');
  process.exit(0);
});
