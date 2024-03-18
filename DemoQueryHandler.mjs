'use strict'

import * as wutil from './lib/webutils.mjs';

export { validateDemoQueryRequest, handleDemoQueryRequest };

function findAllowed(curie, allowList, validationKey) {
  const retval = allowList.filter((elem) => elem[validationKey] === curie);
  return retval.length > 0 ? retval[0] : null;
}

function validateDemoQueryRequest(isDemo, allowList, validationKey, reqToCurie) {
  return function(req, res, next) {
    if (isDemo) {
      const curie = reqToCurie(req);
      const query = findAllowed(curie, allowList, validationKey);
      if (!query) {
        return wutil.sendError(res, 403, `Request for ${curie} is not supported`);
      } else {
        req.demoQuery = query;
        next();
      }
    } else {
      next();
    }
  }
}

function handleDemoQueryRequest(basePath) {
  return function(req, res, next) {
    const query = req.demoQuery;
    // Note: kinda tightly coupled to knowledge of FE. TODO to clean this up some day
    res.redirect(302, `${basePath}/results?l=${query.name}&i=${query.id}&t=0&q=${query.uuid}`);
  }
}
