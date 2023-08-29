'use strict';

import { default as express } from 'express';
import { User } from '../models/User.mjs';
import * as wutil from '../webutils.mjs';

import { createQueryController } from './QueryAPIController.mjs'

export { createAPIRouter };

function createAPIRouter(config, services, isDemo) {
  var router = express.Router();
  router.all('/', function(req, res, next) {
    return res.status(403).send("Forbidden");
  })

  router.get('/config', function(req, res, next) {
    return res.status(200).json({
      cached_queries: config.frontend,
      social_providers: config.auth.social_providers
    });
  });

  router.use('/query', createQueryController(config, services, isDemo));

  return router;
}
