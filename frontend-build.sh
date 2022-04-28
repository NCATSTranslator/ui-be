#!/bin/sh

ui_repo="git@github.com:dnsmith124/ui-prototype-one.git"
ui_src="ui-prototype-one"
fe_branch=${1}
if [ -z "${fe_branch}" ]; then
  fe_branch="develop"
fi

echo "Starting UI build"

echo "Downloading front end"
git clone ${ui_repo}
echo "Building front end"
pushd ${ui_src} > /dev/null
git checkout ${fe_branch}
git pull
echo "Installing front end dependencies"
npm install
echo "Building front end source"
npm run build
rsync -av build ..
popd > /dev/null
echo "Done building front end"

echo "Done building UI"