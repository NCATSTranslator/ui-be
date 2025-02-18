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
            console.log(`stmt mata: ${stmt}`);
            let res = await pgExec(this.dbPool, stmt);
            console.log(`res`);
            console.log(res.rows);
        };
        return true;
    }

    async verify(obj=null) {
        throw new Error('Not implemented');
    }

    successMessage(obj=null) {
        throw new Error('Not implemented');
    }
}