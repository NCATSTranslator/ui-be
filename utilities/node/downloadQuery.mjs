'use strict'

/**
 * Downloads an ARS results given a pk
 */

import { ARSClient } from '../../lib/ARSClient.mjs';

const arsClient = new ARSClient(
  'https://ars-prod.transltr.io',
  '/ars/api/messages',
  '/ars/api/submit',
  '/ars/api/retain',
  'https',
  true
);

const pk = process.argv[2];
arsClient._collectMergedResults(pk).then(data => console.log(JSON.stringify(data)));
