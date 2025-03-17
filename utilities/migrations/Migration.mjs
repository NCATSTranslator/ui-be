'use strict';

export { Migration };

class Migration {
    constructor({
      id=null,
      migration_id,
      time_begun,
      time_complete,
      run_id = '',
      message = ''
    }) {
      this.id = id;
      this.migration_id = migration_id;
      this.time_begun = time_begun;
      this.time_complete = time_complete;
      this.run_id = run_id;
      this.message = message;
    }
  }
