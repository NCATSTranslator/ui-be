#!/bin/sh

cookie=$1

curl -X 'GET' 'http://localhost:8386/api/v1/users/me/queries' -H 'Content-Type: application/json' \
  -H 'User-Agent: Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0' \
  -H 'Accept: */*' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate, br, zstd' \
  -H 'Connection: keep-alive' \
  -H 'Referer: http://localhost:8386/' \
  -H "Cookie: ${cookie}" \
  -H 'Sec-Fetch-Dest: script' \
  -H 'Sec-Fetch-Mode: cors' \
  -H 'Sec-Fetch-Site: same-origin' \
  -H 'Pragma: no-cache'
