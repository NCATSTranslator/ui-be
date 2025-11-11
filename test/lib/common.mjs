export {
  module_test,
  functional_test,
  run_case,
  class_test,
  test_deep,
  apply_rule
}

import * as ast from 'node:assert';
import * as cmn from '#lib/common.mjs';

async function module_test({module_path, suite_path}) {
  const module = await import(module_path);
  const suite = (await import(suite_path)).suite;
  for (const [k, v] of Object.entries(module)) {
    const test_cases = suite[k];
    if (test_cases === undefined) { console.log(`WARNING: Untested export ${k} from module ${module_path}`);
      continue;
    }
    if (cmn.is_class(v)) {
      await class_test({
        test_class: v,
        test_cases: test_cases
      });
    } else if (cmn.is_function(v)) {
      await functional_test({
        test_func: v,
        test_cases: test_cases
      });
    } else {
      throw new cmn.DeveloperError('test/lib/common.mjs', 'module_test', `Unexpected test type for test case.\n  Test type: ${typeof v}\n  Test case: ${k}`);
    }
  }
}

async function functional_test({test_func, test_cases}) {
  const test_name = test_func.name;
  console.log(`Running tests for ${test_name}`);
  for (let case_name of Object.keys(test_cases)) {
    const tc = test_cases[case_name];
    console.log(`--- Running test case ${case_name}`);
    try {
      let actual = await run_case({
        test_func: test_func,
        test_case: tc,
        case_name: case_name
      });
      if (tc.post) {
        actual = await tc.post({
          actual: actual,
          case_context: tc.context
        });
      }
      test_deep(actual, tc.expected);
    } catch (err) {
      const err_object = tc.expected;
      if (!cmn.is_function(err_object) || !(err instanceof err_object)) {
        throw err;
      }
    }
    console.log(`--- Test case ${case_name} passed`);
  }
  console.log(`${test_name} passed`);
}

async function run_case({test_func, test_case, case_name}) {
  if (test_case.config_loader) {
    await test_case.config_loader();
  }
  return await test_func(...test_case.args);
}

async function class_test({test_class, test_cases, config_loader}) {
  const class_name = test_class.name;
  console.log(`Running tests for ${class_name}`);
  for (let case_name of Object.keys(test_cases)) {
    console.log(`--- Running test case ${case_name}`);
    const tc = test_cases[case_name];
    if (tc.config) {
      await config_loader(tc.config);
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
