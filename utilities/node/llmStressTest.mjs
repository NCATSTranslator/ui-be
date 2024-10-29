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

async function runSimulation(hoursOfTesting, users, requestsPerUser, testCases) {
  const testingTime = 60 * 60 * hoursOfTesting;
  const totalRequests = users * requestsPerUser;
  const timeBetweenRequests = testingTime / totalRequests;

  console.log(`Testing for ${testingTime} seconds`);
  console.log(`Time between requests: ${timeBetweenRequests} seconds`);
  let timeElapsed = 0;
  const responseStatus = []
  for (let currentRequest = 1; currentRequest <= totalRequests; currentRequest++) {
    console.log(`Total time elapsed: ${timeElapsed} seconds`);
    console.log(`Current status: ${currentRequest}/${totalRequests} requests made`);
    let startTime = Date.now();
    let testCase = testCases[Math.floor(Math.random() * testCases.length)];
    let response = await makeRequest(testCase);
    let endTime = Date.now();
    let requestTime = (endTime - startTime)/1000;
    timeElapsed += requestTime;
    responseStatus.push({
      request: testCase,
      success: response ? true : false,
      response: response ? response : null,
      time: requestTime
    });
    let sleepTime = Math.max(0, timeBetweenRequests - requestTime);
    console.log(`Finished request ${currentRequest}. Sleeping for ${sleepTime} seconds`);
    await sleep(sleepTime);
  }
  return responseStatus;
}

const hoursOfTesting = 4;
const users = 1;
const requestsPerUser = 240;
const testCases = await loadTestCases(process.argv[2]);
const result = await runSimulation(hoursOfTesting, users, requestsPerUser, testCases);
console.log(JSON.stringify(result));

