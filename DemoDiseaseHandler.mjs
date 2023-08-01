'use strict'

import * as wutil from './webutils.mjs';

export { validateDemoDiseaseRequest, handleDemoDiseaseRequest };

function findAllowedDisease(disease, diseaseList, byField) {
  const retval = diseaseList.filter((elem) => elem[byField] === disease);
  return retval.length > 0 ? retval[0] : null;
}

function validateDemoDiseaseRequest(isDemo, diseaseList, byField, fromReq) {
  return function(req, res, next) {
    if (isDemo) {
      let disease_id = fromReq(req);
      let disease = findAllowedDisease(disease_id, diseaseList, byField);
      if (!disease) {
        return wutil.sendError(res, 403, `Request for ${disease_id} is not supported`);
      } else {
        req.demoDisease = disease;
        next();
      }
    } else {
      next();
    }
  };
}

function handleDemoDiseaseRequest(basePath) {
  return function(req, res, next) {
    const diseaseData = req.demoDisease;
    const name = diseaseData.name;
    const id = diseaseData.id;
    const uuid = diseaseData.uuid;
    // Note: kinda tightly coupled to knowledge of FE. TODO to clean this up some day
    res.redirect(302, `${basePath}/results?l=${name}&i=${id}&t=0&q=${uuid}`);
  }
}
