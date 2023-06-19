'use strict';

export { iSessionStore };

class iSessionStore {
  constructor(config) {
    if (new.target === iSessionStore) {
      throw new Error("This is an abstract class and should not be instantiated directly");
    }
  }

  async retrieveSessionByToken(token) {
    throw new Error("Not implemented");
  }

  async updateSession(sessionData) {
    throw new Error("Not implemented");
  }

  async createNewSession(sessionData) {
    throw new Error("Not implemented");
  }
}
