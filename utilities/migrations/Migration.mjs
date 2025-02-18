'use strict';

export { Migration };

class Migration {
    constructor({
      id = '',
      migration_id,
      time_begun,
      time_complete,
      message = '',
      run_id = ''
    }) {
      this.id = id;
      this.migration_id = migration_id;
      this.time_begun = time_begun;
      this.time_complete = time_complete;
      this.message = message;
      this.run_id = run_id;
    }
  }
