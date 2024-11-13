'use strict';

import { default as pino } from 'pino';

export const logger = pino({
  level: 'info',
  redact: ['secrets'] // For the config object
});
