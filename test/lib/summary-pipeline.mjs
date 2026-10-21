export {
  capture_diagnostics,
  group_diagnostics,
  has_result_data,
  load_environment_config,
  load_pipeline,
  summarize
}

import { logger } from '#lib/logger.mjs';
import { read_json } from '#lib/common.mjs';
import { load_biolink } from '#lib/biolink-model.mjs';
import { load_trapi } from '#lib/trapi/core.mjs';
import { loadChebi } from '#lib/chebi.mjs';
import { TranslatorServicexFEAdapter } from '../../adapters/TranslatorServicexFEAdapter.mjs';

const SIGNING_SECRET = 'links-test-signing-secret';
const MAX_LOGGED_OBJECT_CHARS = 200;
const DIAGNOSTIC_GROUP_CHARS = 48;

async function load_environment_config(config_path) {
  const config = await read_json(config_path);
  config.frontend = await read_json(config._load_frontend);
  return config;
}

async function load_pipeline(config) {
  logger.level = 'silent';
  await load_biolink(config.biolink);
  load_trapi(config.trapi);
  await loadChebi();
  return new TranslatorServicexFEAdapter(config.feature_config, SIGNING_SECRET);
}

async function summarize(adapter, message) {
  return capture_diagnostics(() => adapter.queryResultsToFE(message));
}

function has_result_data(entry) {
  return !!entry.data && !(Array.isArray(entry.data) && entry.data.length === 0);
}

async function capture_diagnostics(fn) {
  const diagnostics = [];
  const previous = { warn: logger.warn, error: logger.error, console_error: console.error };
  logger.warn = (...args) => diagnostics.push({ level: 'warn', message: _format_log_args(args) });
  logger.error = (...args) => diagnostics.push({ level: 'error', message: _format_log_args(args) });
  console.error = (...args) => diagnostics.push({ level: 'error', message: _format_log_args(args) });
  try {
    const value = await fn();
    return { value, diagnostics };
  } finally {
    logger.warn = previous.warn;
    logger.error = previous.error;
    console.error = previous.console_error;
  }
}

function group_diagnostics(diagnostics) {
  const groups = new Map();
  for (const diagnostic of diagnostics) {
    const key = `${diagnostic.level}:${_diagnostic_key(diagnostic.message)}`;
    const group = groups.get(key) ?? { level: diagnostic.level, example: diagnostic.message, count: 0 };
    group.count += 1;
    groups.set(key, group);
  }
  return [...groups.values()];
}

function _diagnostic_key(message) {
  return message
    .replace(/[A-Za-z][\w.-]*:[\w.-]+/g, '<id>')
    .replace(/"[^"]*"/g, '""')
    .replace(/\d+/g, '#')
    .slice(0, DIAGNOSTIC_GROUP_CHARS);
}

function _format_log_args(args) {
  return args.map(arg => {
    if (arg instanceof Error) return `${arg.name}: ${arg.message}`;
    if (typeof arg === 'string') return arg;
    const serialized = JSON.stringify(arg) ?? String(arg);
    return serialized.length > MAX_LOGGED_OBJECT_CHARS ? `${serialized.slice(0, MAX_LOGGED_OBJECT_CHARS)}…` : serialized;
  }).join(' ');
}
