'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

export { BaseMigration };

class BaseMigration {
    constructor(dbPool) {
        this.dbPool = dbPool;
        this.sql = [];
    }

    async execute(obj=null) {
        for (let stmt of this.sql) {
            let res = await pgExec(this.dbPool, stmt);
        };
        return true;
    }

    async verify(obj=null) {
        // Do not implement--derived classes should override
        throw new Error('Not implemented');
    }

    successMessage(obj=null) {
        // Do not implement--derived classes should override
        throw new Error('Not implemented');
    }
}