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
#
case "$APP_ENVIRONMENT" in
    production)
        config_file="configurations/production.json"
        ;;
    test)
        config_file="configurations/test.json"
        ;;
    ci)
        config_file="configurations/ci.json"
        ;;
    dev)
        config_file="configurations/dev.json"
        ;;
    *)
        echo "Unexpected value $APP_ENVIRONMENT for APP_ENVIRONMENT"
        ;;
esac

# If a cmdline arg is provided, override the env var-sourced
# configuration file.
if [ $# -ge 1 ]; then
    echo "Overriding env var-driven config file with explicitly supplied file"
    config_file="$1"
fi

if [ $# -eq 2 ]; then
    echo "Using additional override file $2"
    node StartServer.mjs "$config_file" "$2"
else
    node StartServer.mjs "$config_file" 
fi


