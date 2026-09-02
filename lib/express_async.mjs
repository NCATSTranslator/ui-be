'use strict';

export { wrap_router_async };

const ROUTING_METHODS = ['use', 'all', 'get', 'post', 'put', 'patch', 'delete', 'head', 'options'];

function is_router(handler) {
  return typeof handler.handle === 'function' || typeof handler.stack === 'object';
}

function wrap_handler(handler) {
  if (typeof handler !== 'function' || is_router(handler)) {
    return handler;
  }
  if (handler.length === 4) {
    return function (err, req, res, next) {
      return Promise.resolve(handler.call(this, err, req, res, next)).catch(next);
    };
  }
  return function (req, res, next) {
    return Promise.resolve(handler.call(this, req, res, next)).catch(next);
  };
}

function wrap_argument(argument) {
  if (Array.isArray(argument)) {
    return argument.map(wrap_argument);
  }
  return wrap_handler(argument);
}

function wrap_router_async(router) {
  for (const method of ROUTING_METHODS) {
    const original = router[method];
    if (typeof original !== 'function') {
      continue;
    }
    router[method] = function (...args) {
      return original.apply(this, args.map(wrap_argument));
    };
  }
  return router;
}
