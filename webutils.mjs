'use strict';

export { sendError, sendInternalServerError, logInternalServerError, setSessionCookie };

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

function setSessionCookie(res, cookieName, cookieVal, cookiePath, maxAgeSec) {
  console.log(`_+_+_+_+_ set session cookie: [${cookieName}/${maxAgeSec}]: ${cookieVal}`);
  res.cookie(cookieName, cookieVal, {
    maxAge: maxAgeSec * 1000,
    path: cookiePath,
    httpOnly: true,
    secure: true,
    sameSite: 'Lax'
  });
}
