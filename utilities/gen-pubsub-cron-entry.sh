#!/bin/bash
config_file=$1
override_file=$2
if [[ -z "$override_file" ]]; then
  echo "*/5 * * * * root /usr/local/bin/node /app/utilities/node/reconcile-query-state.mjs /app/$config_file >> /var/log/cron.log 2>&1" > /app/utilities/cron/pubsub-handler.cron
else
  echo "*/5 * * * * root /usr/local/bin/node /app/utilities/node/reconcile-query-state.mjs /app/$config_file /app/$override_file >> /var/log/cron.log 2>&1" > /app/utilities/cron/pubsub-handler.cron
fi
