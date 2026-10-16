export {
  SQL_TYPES,
  ENTITY_KIND,
  SERVER_FIELDS,
  models_to_params_and_args,
  client_fields,
}

const ENTITY_KIND = Object.freeze({
  NODE: "node",
  EDGE: "edge"
});

const SERVER_FIELDS = Object.freeze(["id", "user_id", "time_created", "time_updated", "time_deleted"]);

function client_fields(entity_class) {
  return Object.keys(new entity_class()).filter((field) => !SERVER_FIELDS.includes(field));
}

function models_to_params_and_args(models, columns, types, offset = 0) {
  const cc = columns.length;
  const params = [];
  const args = [];
  let param_list = [];
  for (let mp = 0; mp < models.length; mp++) {
    const model = models[mp];
    for (let c = 0; c < cc; c++) {
      const column = columns[c];
      const type = types[c];
      param_list.push(`$${offset+cc*mp+c+1}::${type}`);
      args.push(model[column]);
    }
    params.push(`(${param_list.join(",")})`);
    param_list = [];
  }
  return [params.join(","), args];
}

const SQL_TYPES = Object.freeze({
  INT:         "integer",
  BIGINT:      "bigint",
  DECIMAL:     "decimal",
  DOUBLE:      "double precision",
  SERIAL:      "serial",
  TEXT:        "text",
  VARCHAR:     "varchar",
  CHAR:        "char",
  BOOL:        "boolean",
  UUID:        "uuid",
  DATE:        "date",
  TIME:        "time",
  TIMESTAMP:   "timestamp",
  TIMESTAMPTZ: "timestamptz",
  JSON:        "json",
  JSONB:       "jsonb"
});
