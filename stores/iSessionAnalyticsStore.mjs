'use strict';

export { iSessionAnalyticsStore };

class iSessionAnalyticsStore {
  constructor(config) {
    if (new.target === iSessionAnalyticsStore) {
      throw new Error("This is an abstract class and should not be instantiated directly");
    }
  }

  async retrieveAnalyticsById(id) {
    throw new Error("Not implemented");
  }

  async updateAnalytics(data) {
    throw new Error("Not implemented");
  }

  async createAnalyticsEntry(data) {
    throw new Error("Not implemented");
  }
}
