#!/bin/sh

set -e

if [ $# -ne 3 ]; then
    echo "Usage: $0 <fe-branch-name> <local|dev|ci|test|prod> <yes|no>"
    exit 1
fi

fe_repo="https://github.com/NCATSTranslator/ui-fe.git"
fe_src="ui-fe"
fe_branch="${1}"
app_env="${2}"
do_build="${3}"
echo "Starting UI build"
if [ ! -d "${fe_src}" ]; then
   echo "Cloning front end"
   git clone ${fe_repo}
fi
cd ${fe_src}
git checkout ${fe_branch}
git pull
if [ "${do_build}" = "yes" ]; then
    echo "Installing front end dependencies"
    npm install
    echo "Building front end source"
    npm run build:$app_env
    echo "Done building front end"
else
    echo "Skipping npm build process"
fi
cd ..
echo "Done"
