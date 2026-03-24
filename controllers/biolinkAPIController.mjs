'use strict';

export { BiolinkAPIController };

import * as wutil from "#lib/webutils.mjs";
import { HEADERS, HTTP_CODE } from "#lib/common.mjs";
import { get_node_type_description } from "#lib/biolink-model.mjs";

class BiolinkAPIController {
  get_node_descriptions(req, res, next) {
    const bytes = req.headers[HEADERS.CONTENT_LENGTH];
    const max_bytes = 1028;
    if (bytes > max_bytes) {
      return wutil.sendError(res, HTTP_CODE.BAD_REQUEST, "Size of request is too large");
    }
    const node_types = req.body;
    const mapping = {};
    for (const type of node_types) {
      mapping[type] = get_node_type_description(type);
    }
    return res.status(HTTP_CODE.SUCCESS).json(mapping);
  }
}
