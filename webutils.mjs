'use strict';

export { sendError, sendInternalServerError, logInternalServerError};

function sendError(res, errorCode, message) {
  const response = {
    'status': 'error',
    'data': message
  }
  res.status(errorCode).json(response);
}

function sendInternalServerError(res) {
  sendError(res, 500, 'Internal Server Error');
}

function logInternalServerError(req, err) {
  req.log.error(`Internal Server Error: ${err}`);
}
