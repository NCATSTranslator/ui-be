import * as ast from 'node:assert';
import * as cmn from '../../lib/common.mjs';

export async function functional_test(kwargs) {
  const {
    test_func,
    test_cases,
    config_loader,
    args_loader,
    post_func = (actual) => actual.actual} = kwargs;
  const test_name = test_func.name;
  console.log(`Running tests for ${test_name}`);
  for (let case_name of Object.keys(test_cases)) {
    const tc = test_cases[case_name];
    console.log(`--- Running test case ${case_name}`);
    const actual = await post_func({
        actual: await _run_case({...kwargs, case_name: case_name}),
        case_context: tc.context,
    })
    testDeep(actual, tc.expected);
    console.log(`--- Test case ${case_name} passed`);
  }
  console.log(`${test_name} passed`);
}

export async function _run_case({
    test_func,
    test_cases,
    case_name,
    config_loader,
    args_loader = cmn.identity}) {
  const tc = test_cases[case_name];
  if (tc.config) {
    await config_loader(await _expand(tc.config));
  }
  return await test_func(...await args_loader(tc.args));
}

export async function class_test({test_class, test_cases, config_loader}) {
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
      testDeep(actual, step.expected);
      console.log(`------ ${step.method} passed`);
    }
    console.log(`--- ${case_name} passed`);
  }
  console.log(`${class_name} passed`);
}

export function testDeep(ac, ex) {
  try {
    _testDeep(ac, ex);
  } catch(err) {
    console.log(JSON.stringify(err.trace,null,2));
    err = removeTrace(err);
    throw err;
  }
}

export function classLoader(func, args) {

}

function _testDeep(ac, ex, permissive=false) {
  try {
    if (cmn.isArray(ac) && cmn.isArray(ex)) {
      testArray(ac, ex);
    } else if (cmn.isObject(ac) && cmn.isObject(ex)) {
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
