import * as ast from 'node:assert';
import * as cmn from '../../lib/common.mjs';

export async function functionalTest(testFunc, testCases, configLoader) {
  const testName = testFunc.name;
  console.log(`Running tests for ${testName}`);
  for (let caseName of Object.keys(testCases)) {
    console.log(`--- Running test case ${caseName}`);
    const tc = testCases[caseName];
    if (tc.config) {
      await configLoader(await _expand(tc.config));
    }
    const actual = await testFunc(...tc.args);
    testDeep(actual, tc.expected);
    console.log(`--- Test case ${caseName} passed`);
  }
  console.log(`${testName} passed`);
}

export async function classTest(klass, testCases, configLoader) {
  const className = klass.name;
  console.log(`Running tests for ${className}`);
  for (let caseName of Object.keys(testCases)) {
    console.log(`--- Running test case ${caseName}`);
    const tc = testCases[caseName];
    if (tc.config) {
      await configLoader(await _expand(tc.config));
    }
    let obj = klass;
    if (tc.constructor !== undefined) {
      obj = new klass(...tc.constructor.args);
      console.log(`------ constructor passed`);
    }
    for (let step of tc.steps) {
      const actual = obj[step.method](...step.args);
      testDeep(actual, step.expected);
      console.log(`------ ${step.method} passed`);
    }
    console.log(`--- ${caseName} passed`);
  }
  console.log(`${className} passed`);
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
