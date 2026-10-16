export {
  canvas_table,
  user_data_table,
  translator_data_table,
  rows_from_json
}

import { ENTITY_KIND } from "#model/common.mjs";

const _TIMESTAMP_FIELDS = Object.freeze(["time_created", "time_updated", "time_deleted"]);

function canvas_table(kind) {
  return `canvas_${_assert_entity_kind(kind)}`;
}

function user_data_table(kind) {
  return `user_${_assert_entity_kind(kind)}`;
}

function translator_data_table(kind) {
  return _assert_entity_kind(kind);
}

function _assert_entity_kind(kind) {
  if (!Object.values(ENTITY_KIND).includes(kind)) {
    throw new Error(`Unknown entity kind: ${JSON.stringify(kind)}`);
  }
  return kind;
}

function rows_from_json(rows) {
  return (rows ?? []).map((row) => {
    const parsed = { ...row };
    for (const field of _TIMESTAMP_FIELDS) {
      if (parsed[field] !== undefined && parsed[field] !== null) parsed[field] = new Date(parsed[field]);
    }
    return parsed;
  });
}
