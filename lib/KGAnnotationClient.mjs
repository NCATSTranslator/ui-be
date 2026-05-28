'use strict';

import { send_recv_json, with_timeout } from "./common.mjs";

export { KGAnnotationClient };

class KGAnnotationClient
{
  constructor(origin, queryPath, searchFields, defaultTimeout)
  {
    this.origin = origin;
    this.queryPath = queryPath;
    this.queryURL = `${origin}${queryPath}/?fields=${searchFields.join(',')}`;
    this.defaultTimeout = defaultTimeout
  }

  async annotateGraph(kg, timeout=this.defaultTimeout)
  {
    const res = with_timeout(async () =>
      {
        return send_recv_json(this.queryURL, 'POST', {}, kg);
      }, timeout);

    return res;
  }
}
