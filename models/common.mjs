export {
  SQL_TYPES,
  models_to_params_and_args,
}

function models_to_params_and_args(models, columns, types) {
  const cc = columns.length;
  const params = [];
  const args = [];
  let param_list = [];
  for (let mp = 0; mp < models.length; mp++) {
    const model = models[mp];
    for (let c = 0; c < cc; c++) {
      const column = columns[c];
      const type = types[c];
      param_list.push(`$${cc*mp+c+1}::${type}`);
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
