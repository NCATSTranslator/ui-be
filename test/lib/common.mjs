import * as ast from 'node:assert';
import * as cmn from '../../lib/common.mjs';

export function testDeep(ac, ex) {
  try {
    _testDeep(ac, ex);
  } catch(err) {
    console.log(JSON.stringify(err.trace,null,2));
    err = removeTrace(err);
    throw err;
  }
}

function _testDeep(ac, ex) {
  try {
    if (cmn.isArray(ac) && cmn.isArray(ex)) {
      testArray(ac, ex);
    } else if (cmn.isObject(ac) && cmn.isObject(ex)) {
      testObject(ac, ex);
    } else {
      ast.strictEqual(ac, ex);
    }
  } catch(err) {
    err = addTrace(err, ac, ex);
    throw err;
  }
}

function testObject(ac, ex) {
  const ack = Object.keys(ac).sort();
  const exk = Object.keys(ex).sort();
  testArray(ack, exk);
  for (let k of ack) {
    _testDeep(ac[k], ex[k]);
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
