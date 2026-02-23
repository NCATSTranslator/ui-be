#!/bin/bash

set -euxo pipefail

# DANGER: defaulting to prod is ok for F&F but shouldn't
# stay like this for too long.
APP_ENVIRONMENT="${APP_ENVIRONMENT:-production}"
export APP_ENVIRONMENT

# At present, owing to the ITRB setup, we do not control app launch
# definitions. To mitigate the impact of this, we expect only a single
# env var defining the environment, and set our required variables on
# the basis of that. This is not an ideal pattern but will do for now.
mem_limit=8192
case "$APP_ENVIRONMENT" in
    production)
        config_file="configurations/production.json"
        ;;
    test)
        config_file="configurations/test.json"
        ;;
    ci)
        config_file="configurations/ci.json"
        mem_limit=4096
        ;;
    dev)
        config_file="configurations/dev.json"
        ;;
    *)
        echo "Unexpected value $APP_ENVIRONMENT for APP_ENVIRONMENT"
        ;;
esac

if [[ $# -eq 0 ]]; then
  echo "Usage: $0 <app> [<config_file>] [<override_file>]"
  exit 1
fi

app=$1

# If a cmdline arg is provided, override the env var-sourced
# configuration file.
if [ $# -ge 2 ]; then
    echo "Overriding env var-driven config file with explicitly supplied file"
    config_file="$2"
fi

override_file=""
if [ $# -eq 3 ]; then
    echo "Using additional override file $3"
    override_file="$3"
fi

if [[ "$app" == "app" ]]; then
    node --max-old-space-size="$mem_limit" StartServer.mjs "$config_file" "$override_file"
elif [[ "$app" == "cron" ]]; then
    utilities/gen-pubsub-cron-entry.sh
    cron -f
else
  echo "Unknown application parameter: $1"
  exit 1
fi
