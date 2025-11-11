'use strict'
import * as fs from 'fs';
import * as zlib from 'zlib';
import * as zstd from 'zstd-napi';
import * as urlLib from 'node:url';
import { validate as isUuid } from 'uuid';
import cloneDeep from 'lodash/cloneDeep.js';
import { randomInt, randomBytes, createHash, createHmac } from 'crypto';
import { join } from 'path';

export const HEADERS = Object.freeze({
  CUSTOM_COMPRESSION: 'x-content-compression'
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

export async function readJson(path)
{
  const content = await fs.promises.readFile(path);
  return JSON.parse(content);
}

export function deepCopy(o)
{
  return cloneDeep(o);
}

export function is_missing(i) {
  return i === undefined || i === null;
}

export function capitalize(s)
{
  return `${s[0].toUpperCase()}${s.slice(1).toLowerCase()}`;
}

/* Different from above function in that for biolinkifying category names, we want to allow
 * internal case preservation for things like `RNA product'
 */
export function capitalizeFirstLetter(s)
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

export function distinctArray(a)
{
  if (is_array(a))
  {
    return [...new Set(a)];
  }

  throw new TypeError(`Expected array got ${a}`);
}

export function setUnion(sets)
{
  return sets.reduce((unionedSet, set) =>
    {
      [...set.keys()].forEach((key) =>
        {
          unionedSet.add(key);
        });

      return unionedSet;
    },
    new Set());
}

export function setToObject(set)
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
  return typeof c === 'function'
    && c.constructor.name !== 'Function';
}

export function isObjectEmpty(o)
{
  if (is_object(o))
  {
    return Object.keys(o).length === 0;
  }

  throw new TypeError(`Expected object got ${o}`);
}

export function objRemoveDuplicates(obj) {
  Object.keys(obj).forEach((k) => {
      let v = jsonGet(obj, k);
      if (is_array(v)) {
        obj[k] = [...new Set(v)];
      }
    });

  return obj;
}

export function jsonHasKey(obj, key)
{
  return obj[key] !== undefined
}

export function jsonGet(obj, key, fallback = undefined)
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

export function jsonSet(obj, key, v)
{
  obj[key] = v;
  return obj;
}

export function jsonDelete(obj, key)
{
  delete obj[key];
  return obj;
}

export function jsonMultiSet(obj, kvps)
{
  kvps.forEach((kvp) =>
    {
      const [key, v] = kvp;
      jsonSet(obj, key, v);
    });

  return obj;
}

export function jsonSetDefaultAndGet(obj, key, fallback)
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

export function jsonGetFromKpath(obj, kpath, fallback = undefined)
{
  let currentObj = obj;
  for (const k of kpath)
  {
    if (currentObj)
    {
      currentObj = jsonGet(currentObj, k, fallback);
    }
    else
    {
      return fallback;
    }
  }

  return currentObj;
}

export function jsonSearchKpath(obj, kpaths, fallback = undefined)
{
  for (const kpath of kpaths)
  {
    const v = jsonGetFromKpath(obj, kpath, fallback);
    if (v !== fallback)
    {
      return v;
    }
  }

  return fallback;
}

export function jsonSetFromKpath(obj, kpath, v)
{
  if (is_array_empty(kpath))
  {
    return obj;
  }

  let finalKey = kpath[kpath.length-1];
  let partialKpath = kpath.slice(0, -1);
  let currentObj = obj;
  partialKpath.forEach(k =>
    {
      let nextObj = jsonGet(currentObj, k, false);
      if (!nextObj)
      {
        nextObj = {};
        jsonSet(currentObj, k, nextObj);
      }

      currentObj = nextObj;
    });

  jsonSet(currentObj, finalKey, v);
  return obj;
}

export function jsonUpdate(obj, key, update)
{
  return jsonSet(obj, key, update(jsonGet(obj, key)));
}

export function coerceArray(v) {
  return is_array(v) ? v : [v];
}

export class ServerError extends Error
{
  constructor(message, httpCode) {
    super(message);
    this.name = this.constructor.name;
    this.msg = message;
    this.httpCode = httpCode;
  }
}

export class HTTPError extends Error {
  constructor(message, httpData) {
    super(message);
    this.status = httpData.status;
    this.method = httpData.method;
    this.url = httpData.url;
    this.headers = httpData.headers;
    this.body = httpData.body;
  }
}

export class DeveloperError extends Error {
  constructor(filename, scope, message) {
    super(`Developer Error in ${filename} within ${scope}\n  ${message}`);
    this.name = this.constructor.name;
  }
}

export async function sendRecvJSON2(url, method='GET', headers={}, body=null, compressed=false) {
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

  return sendRecvHTTP2(url, method, headers, body, 'application/json', codec)
}


export async function sendRecvHTTP2(url, method='GET', headers={}, body=null,
  contentType, codec) {
    let options = {
      method: method,
      headers: {...headers}
  };
  options.headers['Content-type'] = contentType;

  if (body) {
    options.body = codec.encode(body);
  }

  let startTime = new Date();
  let resp = await fetch(url, options);
  const fetchMs = new Date() - startTime;
  let parseMs = 0;
  if (resp.ok) {
    startTime = new Date();
    body = await codec.decode(resp);
    parseMs = new Date() - startTime;
    return [{
        fetchMs: fetchMs,
        parseMs: parseMs,
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

async function sendRecvHTTP(url, method='GET', headers={}, body=null, contentType, bodySerializer) {
  let options = {
      method: method,
      headers: {...headers}
  };
  options.headers['Content-type'] = contentType;

  if (body && typeof body === 'object') {
      options.body = bodySerializer(body);
  }

  let resp = await fetch(url, options);
  if (resp.ok) {
      return resp.json();
  } else {
      throw new ServerError(resp.statusText, resp.status);
  }
}

export function SendRecvJSON(url, method='GET', headers={}, body=null) {
  return sendRecvHTTP(url, method, headers, body, 'application/json', JSON.stringify);
}

export function SendRecvFormEncoded(url, method='GET', headers={}, body=null) {
  return sendRecvHTTP(url, method, headers, body, 'application/x-www-form-urlencoded',
    (data) => new URLSearchParams(data).toString());
}

// Usage: await sleep(250);
export async function sleep(ms) {
  return new Promise((resolve) =>
    setTimeout(() => resolve(), ms)
  );
}

// Usage: await withTimeout(async () => sendRecvJSON(...), 1000);
// fun must be an asynch function.
export async function withTimeout(fun, ms)
{
  let timer;
  return Promise.race([
    fun(),
    new Promise((_r, rej) =>
      {
        return timer = setTimeout(rej, ms, new Error(`withTimeout exceeded timeout ${ms} ms.`));
      })]).finally(() => clearTimeout(timer));
}

export function overwriteObj(orig, overwrite) {
  for (let key of Object.keys(overwrite)) {
    if (orig.hasOwnProperty(key)) {
      if (overwrite[key] instanceof Object
          && !(overwrite[key] instanceof Array)) {
        overwriteObj(orig[key], overwrite[key])
      } else {
        orig[key] = overwrite[key];
      }
    } else {
      orig[key] = overwrite[key];
    }
  }
  return orig;
}

export async function loadQueryData(directory, prefix) {
  const files = await fs.promises.readdir(directory);
  const hash = {};

  // Filter files by prefix and process each file
  const fileReadPromises = files.filter(file => file.startsWith(prefix)).map(async file => {
    const path = join(directory, file);
    const content = await readJson(path);

    // Extract the UUID from the filename and validate it
    const uuidMatch = file.match(/\b[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\b/);
    if (uuidMatch && isUuid(uuidMatch[0])) {
      hash[uuidMatch[0]] = content;
    }
  });

  // Wait for all files to be read
  await Promise.all(fileReadPromises);
  return hash;
}

export function makePartialUpdateSQL(table, idcol, fields, returnstar=true) {
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


export function generateRandomAlphaNumString(n=4, m=4) {
  const letters = 'abcdefghijkmnopqrstuvwxyz'; // Excluding 'l'
  const digits = '0123456789';

  let result = '';

  // Generate n random letters
  for (let i = 0; i < n; i++) {
    const randomIndex = randomInt(0, letters.length);
    result += letters[randomIndex];
  }

  // Generate m random digits
  for (let i = 0; i < m; i++) {
    const randomIndex = randomInt(0, digits.length);
    result += digits[randomIndex];
  }

  return result;
}

// See https://datatracker.ietf.org/doc/html/rfc7636#appendix-A
export function base64URLEncode(bytes) {
  const retval = bytes.toString('base64')
    .replace(/\+/g, '-') // Replace '+' with '-'
    .replace(/\//g, '_') // Replace '/' with '_'
    .replace(/=+$/, ''); // Remove '=' padding

  return retval;
}

export function generatePKCECodeVerifier(len=64) {
  return base64URLEncode(randomBytes(len));
}

export function generatePKCECodeChallenge(codeVerifier) {
  let retval = createHash('sha256').update(codeVerifier).digest();
  retval = base64URLEncode(retval);
  return retval;
}

export function generateHMACSignature(msg, secret) {
  if (!msg) {
    msg = '';
  }
  const signature = createHmac('sha256', secret)
    .update(msg)
    .digest('hex');
  return signature;
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

export function verifyHMACSignature(signature, msg, secret) {
  const isValid = signature === generateHMACSignature(msg, secret);
  return isValid;
}
