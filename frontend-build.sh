#!/bin/sh

ui_repo="git@github.com:dnsmith124/ui-prototype-one.git"
ui_src="ui-prototype-one"

if [ $# -eq 0 ]; then
    fe_branch="develop"
    do_build="yes"
elif [ $# -eq 1 ]; then
    if [ "${1}" = "nobuild" ]; then
        fe_branch="develop"
        do_build="nobuild"
    else
        fe_branch="${1}"
        do_build="yes"
    fi
else # $# == 2
    fe_branch="${1}"
    do_build="${2}"
fi

echo "Starting UI build"

echo "Downloading front end"
git clone ${ui_repo}
echo "Cloning front end"
pushd ${ui_src} > /dev/null
git checkout ${fe_branch}
git pull
if [ "${do_build}" != "nobuild" ]; then 
    echo "Installing front end dependencies"
    npm install
    echo "Building front end source"
    npm run build
    rsync -av build ..
    echo "Done building front end"
else
    echo "Skipping npm build process"
fi
popd > /dev/null


echo "Done building UI"
