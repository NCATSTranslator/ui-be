'use strict'
import * as fs from 'fs';
import * as zlib from 'zlib';
import * as zstd from 'zstd-napi';
import * as urlLib from 'node:url';
import { validate as isUuid } from 'uuid';
import cloneDeep from 'lodash/cloneDeep.js';
import { randomInt, randomBytes, createHash, createHmac, timingSafeEqual } from 'crypto';
import { join } from 'path';

export const HEADERS = Object.freeze({
  CUSTOM_COMPRESSION: 'x-content-compression',
  CONTENT_LENGTH: "content-length"
});

export const CONTENT_TYPE = Object.freeze({
  JSON: 'application/json'
});

export const CONTENT_ENCODING = Object.freeze({
  ZSTD: 'zstd'
});

export const QUERY_STATUS = Object.freeze({
  RUNNING:  'running',
  COMPLETE: 'complete',
  ERROR:    'error'
});

export const HTTP_CODE = Object.freeze({
  SUCCESS:         200,
  NO_CONTENT:      204,
  BAD_REQUEST:     400,
  UNAUTHORIZED:    401,
  FORBIDDEN:       403,
  NOT_FOUND:       404,
  GONE:            410,
  INTERNAL_ERROR:  500,
  NOT_IMPLEMENTED: 501
});

export async function read_json(path)
{
  const content = await fs.promises.readFile(path);
  return JSON.parse(content);
}

export function deep_copy(o)
{
  return cloneDeep(o);
}

export function is_missing(x) {
  return x === undefined || x === null;
}

export function is_any_missing(...x) {
  for (let i = 0; i < x.length; i++) {
    if (is_missing(x[i])) return true;
  }
  return false;
}

export function require_defined(obj, ErrorClass, ...properties) {
  for (const property of properties) {
    if (obj[property] === undefined) {
      throw new ErrorClass(`Required field '${property}' is undefined`);
    }
  }
}

export function capitalize(s)
{
  return `${s[0].toUpperCase()}${s.slice(1).toLowerCase()}`;
}

/* Different from above function in that for biolinkifying category names, we want to allow
 * internal case preservation for things like `RNA product'
 */
export function capitalize_first_letter(s)
{
  return `${s[0].toUpperCase()}${s.slice(1)}`;
}

export function titleize(s)
{
  s = s.toLowerCase();
  const exclusions = ['and', 'or', 'for'];
  const words = s.split(/\s+/).map((word) =>
    {
      if (!exclusions.includes(word))
      {
        return capitalize(word);
      }

      return word;
    });

  return words.join(' ');
}

export function identity(x)
{
  return x;
}

export function make_pair(x_key, x, y_key, y) {
  const pair = new Map();
  pair[x_key] = x;
  pair[y_key] = y;
  return pair;
}

export function is_string(v) {
  return typeof v === 'string' || v instanceof String;
}

export function is_boolean(v) {
  return typeof v === 'boolean' || v instanceof Boolean;
}

export function is_array(v)
{
  return Array.isArray(v);
}

export function is_array_empty(a)
{
  if (is_array(a))
  {
    return a.length === 0;
  }

  throw new TypeError(`Expected array got ${a}`);
}

export function distinct_array(a)
{
  if (is_array(a))
  {
    return [...new Set(a)];
  }

  throw new TypeError(`Expected array got ${a}`);
}

export function set_union(sets)
{
  return sets.reduce((unioned_set, set) =>
    {
      [...set.keys()].forEach((key) =>
        {
          unioned_set.add(key);
        });

      return unioned_set;
    },
    new Set());
}

export function set_to_object(set)
{
  const obj = {};
  for (const k of set)
  {
    if (k !== undefined)
    {
      obj[k] = null;
    }
  }

  return obj;
}

export function is_object(o) {
  return typeof o === 'object'
    && o !== null
    && !is_array(o);
}

export function is_function(f) {
  return typeof f === 'function';
}

export function is_class(c) {
  if (typeof c !== 'function') return false;
  try {
    c();
    return false;
  } catch (err) {
    return (err instanceof TypeError);
  }
}

export function is_object_empty(o)
{
  if (is_object(o))
  {
    return Object.keys(o).length === 0;
  }

  throw new TypeError(`Expected object got ${o}`);
}

export function obj_remove_duplicates(obj) {
  Object.keys(obj).forEach((k) => {
      let v = json_get(obj, k);
      if (is_array(v)) {
        obj[k] = [...new Set(v)];
      }
    });

  return obj;
}

export function json_has_key(obj, key)
{
  return obj[key] !== undefined
}

export function json_get(obj, key, fallback = undefined)
{
  const v = obj[key];
  if (v !== undefined)
  {
    return v
  }

  if (fallback !== undefined)
  {
    return fallback
  }

  throw new ReferenceError(`Key: '${key}' not found in object '${JSON.stringify(obj)}' and no default provided`, 'common.mjs');
}

export function json_set(obj, key, v)
{
  obj[key] = v;
  return obj;
}

export function json_delete(obj, key)
{
  delete obj[key];
  return obj;
}

export function json_multi_set(obj, kvps)
{
  kvps.forEach((kvp) =>
    {
      const [key, v] = kvp;
      json_set(obj, key, v);
    });

  return obj;
}

export function json_set_default_and_get(obj, key, fallback)
{
  if (fallback === undefined)
  {
    throw new TypeError('fallback must be provided and not undefined');
  }

  let v = obj[key];
  if (v !== undefined)
  {
    return v;
  }

  obj[key] = fallback;
  return fallback;
}

export function json_get_from_kpath(obj, kpath, fallback = undefined)
{
  let current_obj = obj;
  for (const k of kpath)
  {
    if (current_obj)
    {
      current_obj = json_get(current_obj, k, fallback);
    }
    else
    {
      return fallback;
    }
  }

  return current_obj;
}

export function get_kpath_match(obj, matches, fallback = undefined) {
  for (const match of matches) {
    const val = json_get_from_kpath(obj, match, fallback);
    if (val !== fallback) return val;
  }
  return fallback;
}

export function json_search_kpath(obj, kpaths, fallback = undefined)
{
  for (const kpath of kpaths)
  {
    const v = json_get_from_kpath(obj, kpath, fallback);
    if (v !== fallback)
    {
      return v;
    }
  }

  return fallback;
}

export function json_set_from_kpath(obj, kpath, v)
{
  if (is_array_empty(kpath))
  {
    return obj;
  }

  let final_key = kpath[kpath.length-1];
  let partial_kpath = kpath.slice(0, -1);
  let current_obj = obj;
  partial_kpath.forEach(k =>
    {
      let next_obj = json_get(current_obj, k, false);
      if (!next_obj)
      {
        next_obj = {};
        json_set(current_obj, k, next_obj);
      }

      current_obj = next_obj;
    });

  json_set(current_obj, final_key, v);
  return obj;
}

export function json_update(obj, key, update)
{
  return json_set(obj, key, update(json_get(obj, key)));
}

export function coerce_array(v) {
  return is_array(v) ? v : [v];
}

export class ServerError extends Error
{
  constructor(message, http_code) {
    super(message);
    this.name = this.constructor.name;
    this.msg = message;
    this.http_code = http_code;
  }
}

export class HTTPError extends Error {
  constructor(message, http_data) {
    super(message);
    this.status = http_data.status;
    this.method = http_data.method;
    this.url = http_data.url;
    this.headers = http_data.headers;
    this.body = http_data.body;
  }
}

export class DeveloperError extends Error {
  constructor(filename, scope, message) {
    super(`Developer Error in ${filename} within ${scope}\n  ${message}`);
    this.name = this.constructor.name;
  }
}

export async function send_recv_json2(url, method='GET', headers={}, body=null, compressed=false) {
  const codec = {
    encode: JSON.stringify,
    decode: async (resp) => {
      const data = await resp.arrayBuffer();
      let encoding = null;
      if (resp.headers.has(HEADERS.CUSTOM_COMPRESSION)) {
        encoding = resp.headers.get(HEADERS.CUSTOM_COMPRESSION);
      }
      if (encoding === CONTENT_ENCODING.ZSTD) return JSON.parse(zstd.decompress(new Uint8Array(data)))
      // Legacy ARS does not send us any encoding information and may use gzip or text/plain
      try {
        const enc = new TextDecoder("utf-8");
        return JSON.parse(enc.decode(data));
      } catch (e) {
        return JSON.parse(zlib.gunzipSync(new Uint8Array(data)));
      }
    }
  };

  return send_recv_http2(url, method, headers, body, 'application/json', codec)
}


export async function send_recv_http2(url, method='GET', headers={}, body=null,
  content_type, codec) {
    let options = {
      method: method,
      headers: {...headers}
  };
  options.headers['Content-type'] = content_type;

  if (body) {
    options.body = codec.encode(body);
  }

  let start_time = new Date();
  let resp = await fetch(url, options);
  const fetch_ms = new Date() - start_time;
  let parse_ms = 0;
  if (resp.ok) {
    start_time = new Date();
    body = await codec.decode(resp);
    parse_ms = new Date() - start_time;
    return [{
        fetch_ms: fetch_ms,
        parse_ms: parse_ms,
        status: resp.status,
        headers: resp.headers
      }, body];
  } else {
    throw new HTTPError(
      `HTTP Error ${resp.status}: ${method} ${url}`, {
        method: method,
        headers: resp.headers,
        url: url,
        status: resp.status,
        body: await resp.text()
      });
  }
}

async function send_recv_http(url, method='GET', headers={}, body=null, content_type, body_serializer) {
  let options = {
      method: method,
      headers: {...headers}
  };
  options.headers['Content-type'] = content_type;

  if (body && typeof body === 'object') {
      options.body = body_serializer(body);
  }

  let resp = await fetch(url, options);
  if (resp.ok) {
      return resp.json();
  } else {
      throw new ServerError(resp.statusText, resp.status);
  }
}

export function send_recv_json(url, method='GET', headers={}, body=null) {
  return send_recv_http(url, method, headers, body, 'application/json', JSON.stringify);
}

export function send_recv_form_encoded(url, method='GET', headers={}, body=null) {
  return send_recv_http(url, method, headers, body, 'application/x-www-form-urlencoded',
    (data) => new URLSearchParams(data).toString());
}

// Usage: await sleep(250);
export async function sleep(ms) {
  return new Promise((resolve) =>
    setTimeout(() => resolve(), ms)
  );
}

// Usage: await with_timeout(async () => send_recv_json(...), 1000);
// fun must be an asynch function.
export async function with_timeout(fun, ms)
{
  let timer;
  return Promise.race([
    fun(),
    new Promise((_r, rej) =>
      {
        return timer = setTimeout(rej, ms, new Error(`with_timeout exceeded timeout ${ms} ms.`));
      })]).finally(() => clearTimeout(timer));
}

export function overwrite_obj(orig, overwrite) {
  for (let key of Object.keys(overwrite)) {
    if (orig.hasOwnProperty(key)) {
      if (overwrite[key] instanceof Object
          && !(overwrite[key] instanceof Array)) {
        overwrite_obj(orig[key], overwrite[key])
      } else {
        orig[key] = overwrite[key];
      }
    } else {
      orig[key] = overwrite[key];
    }
  }
  return orig;
}

export async function load_query_data(directory, prefix) {
  const files = await fs.promises.readdir(directory);
  const hash = {};

  // Filter files by prefix and process each file
  const file_read_promises = files.filter(file => file.startsWith(prefix)).map(async file => {
    const path = join(directory, file);
    const content = await read_json(path);

    // Extract the UUID from the filename and validate it
    const uuid_match = file.match(/\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b/);
    if (uuid_match && isUuid(uuid_match[0])) {
      hash[uuid_match[0]] = content;
    }
  });

  // Wait for all files to be read
  await Promise.all(file_read_promises);
  return hash;
}

export function make_partial_update_sql(table, idcol, fields, returnstar=true) {
  let sql = `UPDATE ${table} set `;
  let count = 0;
  let updates = [];
  for (let f in fields) {
    if (fields.hasOwnProperty(f)) {
      updates.push(`${f} = $${count}`);
      count += 1;
    }
  }
  sql = `${sql} ${updates.join(', ')} WHERE ${idcol} = $${count} ${returnstar ? ' RETURNING *' : ''};`;
  return sql;
}


export function generate_random_alpha_num_string(n=4, m=4) {
  const letters = 'abcdefghijkmnopqrstuvwxyz'; // Excluding 'l'
  const digits = '0123456789';

  let result = '';

  // Generate n random letters
  for (let i = 0; i < n; i++) {
    const random_index = randomInt(0, letters.length);
    result += letters[random_index];
  }

  // Generate m random digits
  for (let i = 0; i < m; i++) {
    const random_index = randomInt(0, digits.length);
    result += digits[random_index];
  }

  return result;
}

// See https://datatracker.ietf.org/doc/html/rfc7636#appendix-A
export function base64_url_encode(bytes) {
  const retval = bytes.toString('base64')
    .replace(/\+/g, '-') // Replace '+' with '-'
    .replace(/\//g, '_') // Replace '/' with '_'
    .replace(/=+$/, ''); // Remove '=' padding

  return retval;
}

export function generate_pkce_code_verifier(len=64) {
  return base64_url_encode(randomBytes(len));
}

export function generate_pkce_code_challenge(code_verifier) {
  let retval = createHash('sha256').update(code_verifier).digest();
  retval = base64_url_encode(retval);
  return retval;
}

export function generate_hmac_signature(msg, secret) {
  if (!msg) {
    msg = '';
  }
  const signature = createHmac('sha256', secret)
    .update(msg)
    .digest('hex');
  return signature;
}

// Domain separation: this string is prepended to the signed message so a signature minted here can
// never collide with another use of the same HMAC secret.
const _ENTITY_SIG_DOMAIN = 'translator:canvas:entity:v1';

// Deterministic JSON serialization
export function canonical_json(value) {
  return JSON.stringify(_canonicalize(value));
}

function _canonicalize(value) {
  if (Array.isArray(value)) {
    return value.map(_canonicalize);
  }
  if (value !== null && typeof value === 'object') {
    const sorted = {};
    for (const key of Object.keys(value).sort()) {
      sorted[key] = _canonicalize(value[key]);
    }
    return sorted;
  }
  return value;
}

export function sign_entity_data(raw_data, secret) {
  return generate_hmac_signature(`${_ENTITY_SIG_DOMAIN}:${canonical_json(raw_data)}`, secret);
}

export function verify_entity_data(raw_data, signature, secret) {
  if (!is_string(signature)) return false;
  const expected = sign_entity_data(raw_data, secret);
  if (expected.length !== signature.length) return false;
  return timingSafeEqual(Buffer.from(expected), Buffer.from(signature));
}

export function canonicalize_url(url_str, delimiter='|') {
  const url = new urlLib.URL(url_str);
  const sorted_query = [...url.searchParams.entries()]
    .map(([k, v]) => [decodeURIComponent(k), decodeURIComponent(v)])
    .sort(([a], [b]) => a.localeCompare(b))
    .flat()
    .join(delimiter);
  return [
    url.protocol.slice(0, -1), // remove colon
    url.host,
    url.pathname,
    sorted_query
  ].join(delimiter);
}

export function verify_hmac_signature(signature, msg, secret) {
  const is_valid = signature === generate_hmac_signature(msg, secret);
  return is_valid;
}
