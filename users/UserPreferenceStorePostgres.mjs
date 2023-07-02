'use strict';

import { pg, pgExec, pgExecTrans } from '../postgres_preamble.mjs';
import { UserPreference } from '../models/UserPreference.mjs'; // path to the UserPreference class

export { UserPreferenceStorePostgres };

class UserPreferenceStorePostgres {
  constructor(pool, config=null) {
    this.pool = pool ? pool : new pg.Pool(config);
  }

  async retrieveUserPreferencesById(userId) {
    const sql = `
      SELECT UP.user_id, P.pref_name, UP.pref_value, P.pref_data_type, UP.time_created, UP.time_updated
      FROM user_preferences AS UP
      INNER JOIN preferences AS P
      ON UP.pref_id = P.id
      WHERE UP.user_id = $1
    `;
    const res = await pgExec(this.pool, sql, [userId]);
    const userPreferences = res.rows.map(row => new UserPreference(row));
    return userPreferences.length > 0 ? userPreferences : null;
  }

  async updateUserPreferences(userId, userPreferences) {
    return pgExecTrans(this.pool, async (args, client) => {
      const userId = args.userId;
      const userPreferences = args.userPreferences
      const updatedPrefNames = [];
      for (let userPreference of userPreferences) {
        const checkSql = `
          SELECT id
          FROM preferences
          WHERE pref_name = $1
        `;
        const checkRes = await client.query(checkSql, [userPreference.pref_name]);
        if (checkRes.rows.length === 0) {
          console.error(`Preference "${userPreference.pref_name}" does not exist in preferences table`);
          continue;
        }
        const prefId = checkRes.rows[0].id;
        const upsertSql = `
          INSERT INTO user_preferences (user_id, pref_id, pref_value, time_updated)
          VALUES ($1, $2, $3, $4)
          ON CONFLICT (user_id, pref_id)
          DO UPDATE SET pref_value = EXCLUDED.pref_value, time_updated = EXCLUDED.time_updated
        `;
        await client.query(upsertSql, [userId, prefId, userPreference.pref_value, new Date()]);
        // Add the updated pref_name to the array
        updatedPrefNames.push(userPreference.pref_name);
      }
      return updatedPrefNames;
    }, {userId: userId, userPreferences: userPreferences});
  }

  async retrieveSpecificUserPreference(userId, prefName) {
    const sql = `
      SELECT UP.user_id, P.pref_name, UP.pref_value, P.pref_data_type, UP.time_created, UP.time_updated
      FROM user_preferences AS UP
      INNER JOIN preferences AS P
      ON UP.pref_id = P.id
      WHERE UP.user_id = $1 AND P.pref_name = $2
    `;
    const res = await pgExec(this.pool, sql, [userId, prefName]);
    return res.rows.length > 0 ? new UserPreference(res.rows[0]) : null;
  }
}
