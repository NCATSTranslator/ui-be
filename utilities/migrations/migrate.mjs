'use strict';

import { readdir } from 'node:fs/promises';

async function getMigrationFiles(dir, after_timestamp=0, suffix='.sql') {
    let match_rx = new RegExp(`^(\\d+)${suffix}$`); // Note doubled \\
    let all_files = await readdir(dir);
    console.log(all_files);
    let migration_files = all_files.filter(e => e.match(match_rx));
    console.log(migration_files);
    let target_files = migration_files.filter((e) => {
        let file_timestamp = e.match(match_rx)[1];
        return parseInt(file_timestamp, 10) > after_timestamp;
    });
    return target_files.sort((a, b) => {
        let a_timestamp = parseInt(a.match(match_rx)[1], 10);
        let b_timestamp = parseInt(b.match(match_rx)[1], 10);
        return a - b;
    });
}

export { getMigrationFiles };


