'use strict';

export { ConfigAPIController };

class ConfigAPIController {
  constructor(config) {
    this.config = config;
  }

  getConfig(req, res, next) {
    return res.status(200).json({
      gaID: this.config.google_analytics_id,
      cached_queries: this.config.frontend.cached_queries.filter(e => e.allow_outbound),
      name_resolver: this.config.frontend.name_resolver.endpoint,
      social_providers: this.config.auth.social_providers
    });
  }
}
/*
function configAPIController(config) {
  const router = express.Router();
  router.get('/', function(req, res, next) {
    return res.status(200).json({
      gaID: config.google_analytics_id,
      cached_queries: config.frontend.cached_queries.filter(e => e.allow_outbound),
      name_resolver: config.frontend.name_resolver.endpoint,
      social_providers: config.auth.social_providers
    });
  });
  return router;
}*/
