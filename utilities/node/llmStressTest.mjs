import * as cmn from '../../lib/common.mjs';
import * as fs from 'node:fs';

async function loadTestCases(path) {
  const testPool = [];
  const files = fs.readdirSync(path);
  for (let file of files) {
    const [name, ext] = file.split('.');
    if (ext === undefined) continue;
    const testCases = await cmn.readJson(`${path}/${file}`);
    testPool.push(...testCases);
  }
  return testPool;
}

async function makeRequest(data) {
  const url = 'https://ncats-llm-summarization.onrender.com/summary';
  const fetchOptions = {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Connection': 'keep-alive'
    },
    body: JSON.stringify(data)
  };
  try {
    const response = await fetch(url, fetchOptions);
    if (!response.ok) {
      console.log(`Error with request. Got code: ${response.status}`);
      return;
    }
    const reader = response.body.getReader();
    const decoder = new TextDecoder('utf-8');
    let summary = '';
    while (true) {
      const {done, value} = await reader.read();
      if (done) {
        return summary;
        break;
      }
      summary += `${decoder.decode(value, {stream: true})}`;
    }
    return response;
  } catch (err) {
    console.error(err);
    return false;
  }
}

function sleep(sec) {
  return new Promise(resolve => setTimeout(resolve, (sec * 1000)));
}

async function runSimulation(testCases, config) {
  const testingTime = 60 * 60 * config.hoursOfTesting;
  const requestRounds = config.requestRounds;
  const spikeMaxReq = config.spikeMaxReq;
  const spikeMinReq = config.spikeMinReq;
  const spikeProb = config.spikeProb;
  const timeBetweenRounds = testingTime / requestRounds;

  console.log(`Testing for ${testingTime} seconds`);
  console.log(`Time between request rounds: ${timeBetweenRounds} seconds`);
  let timeElapsed = 0;
  const responseStatus = []
  for (let curRound = 1; curRound <= requestRounds; curRound++) {
    console.log(`Total time elapsed: ${timeElapsed} seconds`);
    console.log(`Current status: ${curRound}/${requestRounds} requests made`);
    let requestsToMake = 1;
    let isSpike = (spikeProb >= Math.random());
    if (isSpike) {
      requestsToMake = Math.floor(Math.random() * (spikeMaxReq - spikeMinReq)) + spikeMinReq;
      console.log(`Simulating spike with ${requestsToMake} requests`);
    }
    const testPromises = [];
    const testRequests = [];
    let startTime = Date.now();
    for (let i = 0; i < requestsToMake; i++) {
      let testCase = testCases[Math.floor(Math.random() * testCases.length)];
      testRequests.push(testCase);
      testPromises.push(makeRequest(testCase));
    }
    const testResults = await Promise.all(testPromises);
    let endTime = Date.now();
    let requestTime = (endTime - startTime)/1000;
    timeElapsed += requestTime;
    for (let i = 0; i < testResults.length; i++) {
      responseStatus.push({
        testCase: testRequests[i],
        success: testResults[i] ? true : false,
        testResult: testResults[i] ? testResults[i] : null,
        time: requestTime,
        pathCount: testRequests[i].results.length,
        roundId: curRound
      });
    }
    let sleepTime = Math.max(0, timeBetweenRounds - requestTime);
    console.log(`Finished request ${curRound}. Sleeping for ${sleepTime} seconds`);
    await sleep(sleepTime);
  }
  return responseStatus;
}

const simulationConfig = {
  hoursOfTesting: 8,
  requestRounds: 480,
  spikeMaxReq: 6,
  spikeMinReq: 2,
  spikeProb: 0.2
};
const testCases = await loadTestCases(process.argv[2]);
const result = await runSimulation(testCases, simulationConfig);
console.log(JSON.stringify(result));

