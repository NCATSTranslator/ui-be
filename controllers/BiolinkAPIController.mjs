'use strict';

export { BiolinkAPIController };

import * as wutil from "#lib/webutils.mjs";
import { HEADERS, HTTP_CODE, is_array } from "#lib/common.mjs";
import { get_node_type_description, infores_to_provenance } from "#lib/biolink-model.mjs";

class BiolinkAPIController {
  get_node_descriptions(req, res, next) {
    const bytes = req.headers[HEADERS.CONTENT_LENGTH];
    const max_bytes = 1028;
    if (bytes > max_bytes) {
      return wutil.send_error(res, HTTP_CODE.BAD_REQUEST, "Size of request is too large");
    }
    const node_types = req.body;
    if (!is_array(node_types)) {
      return wutil.send_error(res, HTTP_CODE.BAD_REQUEST,
        `Expected body to be JSON array. Got: ${JSON.stringify(node_types)}`);
    }
    const mapping = {};
    for (const type of node_types) {
      mapping[type] = get_node_type_description(type);
    }
    return res.status(HTTP_CODE.SUCCESS).json(mapping);
  }

  get_infores_catalog_entry(req, res, next) {
    const infores = req.params.infores_id;
    const catalog_entry = infores_to_provenance(infores);
    if (catalog_entry === null) {
      return wutil.send_error(res, HTTP_CODE.NOT_FOUND, `No infores catalog entry found for ${infores}`);
    }
    return res.status(HTTP_CODE.SUCCESS).json(catalog_entry);
  }
}
