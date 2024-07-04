'use strict';

import { default as express } from 'express';

import { createQueryController } from './QueryAPIController.mjs'

export { createAPIRouter };

function createAPIRouter(config, services, isDemo) {
  var router = express.Router();
  router.all('/', function(req, res, next) {
    return res.status(403).send("Forbidden");
  })

  router.get('/config', function(req, res, next) {
    return res.status(200).json({
      gaID: config.google_analytics_id,
      gtmID: config.google_tag_manager_id,
      cached_queries: config.frontend.cached_queries.filter(e => e.allow_outbound),
      name_resolver: config.frontend.name_resolver.endpoint,
      social_providers: config.auth.social_providers
    });
  });

  router.use('/query', createQueryController(config, services, isDemo));

  return router;
}
