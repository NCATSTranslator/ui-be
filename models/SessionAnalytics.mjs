'use strict';

export { SessionAnalytics };

class SessionAnalytics {
  constructor({
    id = null,
    ip_address = null,
    user_agent= null,
    data = null,
    hits = 0
  } = {}) {
    this.id = id;
    this.ip_address = ip_address;
    this.user_agent = user_agent;
    this.data = data;
    this.hits = hits;
  }

  updateAnalyticsData(updatedFields = {}) {
    for (let key in updatedFields) {
      if (this.hasOwnProperty(key)) {
        this[key] = updatedFields[key];
      }
    }
    return this;
  }

  incrementHits() {
    this.hits += 1;
  }
}
