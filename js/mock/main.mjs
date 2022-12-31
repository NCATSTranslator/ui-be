'use strict'

import { SERVER_CONFIG } from '../config.mjs';
import * as ars from './ars.mjs';
import * as trapi from '../trapi.mjs';
import * as server from '../server.mjs';

server.start(ars, trapi, SERVER_CONFIG);
