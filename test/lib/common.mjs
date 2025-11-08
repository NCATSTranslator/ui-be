export {
  functional_test,
  run_case,
  class_test,
  test_deep,
  gen_function_loader,
  apply_rule,
  module_test,
  Environment
}

import * as ast from 'node:assert';
import * as cmn from '../../lib/common.mjs';

async function module_test({suite, against, env}) {
  for (const key of Object.keys(suite)) {
    const test_entity = against[key];
    if (test_entity === undefined) {
      throw new cmn.DeveloperError('test/lib/common.mjs', 'module_test', `Error when importing ${key} from module ${JSON.stringify(against)}.`);
    }
    if (cmn.is_function(test_entity)) {
      await functional_test({
        test_func: test_entity,
        test_cases: suite[key],
        env: env
      });
    } else {
      throw new cmn.DeveloperError('test/lib/common.mjs', 'module_test', `Unknown test entity type: ${typeof test_entity}`);
    }
  }
}

async function functional_test(kwargs) {
  const {
    test_func,
    test_cases,
    config_loader,
    env,
    post_func = (actual) => actual.actual} = kwargs;
  const test_name = test_func.name;
  console.log(`Running tests for ${test_name}`);
  for (let case_name of Object.keys(test_cases)) {
    const tc = test_cases[case_name];
    console.log(`--- Running test case ${case_name}`);
    try {
      const actual = await post_func({
        actual: await run_case({...kwargs, case_name: case_name}),
        case_context: tc.context,
      })
      test_deep(actual, _load_symbols(tc.expected, env));
    } catch (err) {
      const err_object = _load_symbols(tc.expected, env);
      if (!cmn.is_function(err_object) || !(err instanceof err_object)) {
        throw err;
      }
    }
    console.log(`--- Test case ${case_name} passed`);
  }
  console.log(`${test_name} passed`);
}

async function run_case({
    test_func,
    test_cases,
    case_name,
    config_loader,
    env}) {
  const tc = test_cases[case_name];
  if (tc.config) {
    await config_loader(await _expand(tc.config));
  }
  return await test_func(..._load_symbols(tc.args, env));
}

async function class_test({test_class, test_cases, config_loader}) {
  const class_name = test_class.name;
  console.log(`Running tests for ${class_name}`);
  for (let case_name of Object.keys(test_cases)) {
    console.log(`--- Running test case ${case_name}`);
    const tc = test_cases[case_name];
    if (tc.config) {
      await config_loader(await _expand(tc.config));
    }
    let obj = test_class;
    if (tc.constructor !== undefined) {
      obj = new test_class(...tc.constructor.args);
      console.log(`------ constructor passed`);
    }
    for (let step of tc.steps) {
      const actual = obj[step.method](...step.args);
      test_deep(actual, step.expected);
      console.log(`------ ${step.method} passed`);
    }
    console.log(`--- ${case_name} passed`);
  }
  console.log(`${class_name} passed`);
}

function test_deep(ac, ex) {
  try {
    _testDeep(ac, ex);
  } catch(err) {
    console.log(JSON.stringify(err.trace,null,2));
    err = removeTrace(err);
    throw err;
  }
}

function gen_function_loader(env) {
  return (args) => {
    args = args[0];
    if (typeof args.transform === 'string') {
      args.transform = env[args.transform];
    }
    return [args];
  }
}

async function apply_rule({actual, case_context}) {
  let {source, target} = case_context;
  const rule = actual;
  if (!cmn.is_array(source)) {
    source = [source];
  }
  for (const src of source) {
    const transform = rule(src, src['__context__']);
    target = transform(target);
  }
  return target;
}

class Environment {
  static UNDEFINED = Symbol('UNDEFINED');
  static Entry(kwargs) {
    return new _EnvironmentEntry(kwargs);
  }

  constructor(env) {
    for (const entry of env) {
      this.#validate_entry(entry);
      this.#env[entry.key] = entry;
    }
    Object.freeze(this);
  }

  get(key) {
    const entry = this.#env[key];
    if (entry === undefined) return Environment.UNDEFINED;
    return entry.eval(this);
  }

  #env = {};
  #validate_entry(entry) {
    if (!(entry instanceof _EnvironmentEntry)) {
      throw cmn.DeveloperError('test/lib/common.mjs', 'Environment.validate_entry', `Expected Entry to by of type _Environment_Entry.\n  Got: ${typeof entry}`);
    }
  }
}

class _EnvironmentEntry {
  constructor({key, construct, args, value}) {
    if (cmn.is_missing(construct) && cmn.is_missing(value)) {
      throw new cmn.DeveloperError('test/lib/common.mjs', '_EnvironmentEntry.constructor', 'construct and value key word arguments cannot both be missing');
    }
    this.key = key;
    this.#construct = construct;
    this.#args = args;
    if (cmn.is_missing(construct)) {
      this.#construct = () => value;
    }
  }

  eval(env) {
    const args = cmn.deepCopy(this.#args);
    try {
      return this.#construct(..._load_symbols(args, env));
    } catch (err) {
      return new this.#construct(..._load_symbols(args, env));
    }
  }

  #construct = null;
  #args = null;
}

function _load_symbols(obj, env) {
  if (cmn.is_object(obj)) {
    for (const [k, v] of Object.entries(obj)) {
      obj[k] = _load_symbols(v, env);
    }
    return obj;
  }
  if (cmn.is_array(obj)) {
    return obj.map(v => _load_symbols(v, env));
  }
  if (cmn.is_string(obj)) {
    const env_obj = env.get(obj);
    return (env_obj !== Environment.UNDEFINED) ? env_obj : obj;
  }
  return obj;
}

function _testDeep(ac, ex, permissive=false) {
  try {
    if (cmn.is_array(ac) && cmn.is_array(ex)) {
      testArray(ac, ex);
    } else if (cmn.is_object(ac) && cmn.is_object(ex)) {
      let aco = ac;
      if (Symbol.iterator in aco) {
        aco = Object.fromEntries(aco);
      }
      testObject(aco, ex);
    } else {
      if (typeof(ex) === 'string' && ex.startsWith('*')) return true;
      ast.strictEqual(ac, ex);
    }
    return true;
  } catch(err) {
    if (permissive) return false;
    err = addTrace(err, ac, ex);
    throw err;
  }
}

function testObject(ac, ex) {
  const acks = Object.keys(ac);
  const exks = Object.keys(ex);
  ast.strictEqual(acks.length, exks.length);
  for (let exk of exks) {
    if (!exk.startsWith('*') &&
        !acks.includes(exk)) {
      ast.fail(); // A non-wildcard key does not appear in the actual keys
    }
  }

  const acksLeft = new Set(acks);
  const wildcards = [];
  for (let i = 0; i < acks.length; i++) {
    const ack = acks[i];
    const exk = exks[i];
    if (exk.startsWith('*')) {
      wildcards.push(exk);
    }

    const acv = ac[ack];
    const exv = ex[ack];
    if (exv !== undefined) {
      _testDeep(acv, exv);
      acksLeft.delete(ack);
    }
  }

  for (let exk of wildcards) {
    for (let ack of acksLeft) {
      if (_testDeep(ac[ack], ex[exk], true)) {
        acksLeft.delete(ack);
        break;
      }
    }
  }

  if (acksLeft.length > 0) {
    ast.fail(); // There was an unmatched wildcard
  }
}

function testArray(ac, ex) {
  try {
    ast.strictEqual(ac.length, ex.length);
  } catch(err) {
    err = addTrace(err, ac, ex);
    throw err;
  }
  for (let i = 0; i < ac.length; i++) {
    _testDeep(ac[i], ex[i]);
  }
}

function addTrace(err, ac, ex) {
  if (err.depth === undefined) {
    err.depth = 0;
    err.trace = {};
  }

  err.trace[err.depth] = {
    actual: ac,
    expected: ex
  }

  err.depth++;
  return err;
}

function removeTrace(err) {
  err.depth = undefined;
  err.trace = undefined;
  return err;
}

async function _expand(x) {
  if (typeof(x) === 'string' && x.length > 0 && x[0] === '$') {
    return cmn.readJson(x.slice(1,));
  }
  return x;
}
