'use strict';

import * as fs from 'fs';
import * as cmn from './common.mjs';
import * as trapi from './lib/trapi.mjs';

var fnames=["msg-1.final","msg-2.final","msg-3.final","msg-4.final","msg-5.final","msg-6.final","msg-7.final","msg-8.final","msg-9.final","msg-10.final","msg-11.final","msg-12.final","msg-13.final","msg-14.final","msg-15.final","msg-16.final","msg-17.final","msg-18.final","msg-19.final","msg-20.final","msg-21.final","msg-22.final","msg-23.final","msg-24.final","msg-25.final","msg-26.final","msg-27.final","msg-28.final","msg-29.final","msg-30.final","msg-31.final","msg-32.final","msg-33.final","msg-34.final","msg-35.final","msg-36.final","msg-37.final","msg-38.final","msg-39.final","msg-40.final","msg-41.final","msg-42.final","msg-43.final","msg-44.final"];

async function loadAll(fileNames)
{
  let messages = [];
  for (const f of fileNames)
  {
    let yy = await cmn.readJson('/Users/nishadprakash/translator/nnn/gus-stuff/actual-data/' + f);
    messages.push(yy);
  }
  return messages;
}

console.time("loadtime");
var msgs = await loadAll(fnames);
console.timeEnd("loadtime");
var small = msgs.filter(e => e.message.results.length < 100);
var med = msgs.filter(e => e.message.results.length >= 100 && e.message.results.length < 500);
var large = msgs.filter(e => e.message.results.length >= 500 && e.message.results.length < 1000);
var xlarge = msgs.filter(e => e.message.results.length >= 1000);

//var target = small;
//var target = med;
//var target = large;
var target = xlarge;
//target = target.slice(0, 5);

console.time("summarybuild");
var _meh = trapi.creativeAnswersToSummary("ignoreme", target);
console.timeEnd("summarybuild");

var targetData = {
  num: target.length,
  lengths: target.map(e => e.message.results.length)
}

console.log(targetData);
