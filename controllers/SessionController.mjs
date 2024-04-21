'use strict';

export { SessionController };

class SessionController {
  constructor(config, authService) {
    this.config = config;
    this.authService = authService;
  }

  async getStatus(req, res, next) {
    let cookieToken = req.cookies[this.config.session_cookie.name];
    let sessionStatus = await this.authService.getSessionStatus(cookieToken);
    delete sessionStatus.session.data; // remove raw oauth data
    return res.status(200).json(sessionStatus);
  }

  async updateStatus(req, res, next) {
    return res.status(200).json({assume: "something happened"});
  }

}
