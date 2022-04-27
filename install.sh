#!/usr/bin/bash
build_dir="$( pwd )/ui-build"
backend_target=/opt/transltr
frontend_target=/srv/build
test_target=/srv/test

function backup
{
  local dir_to_backup=${1}
  local prev=${dir_to_backup}.prev
  mkdir -p ${dir_to_backup}
  mkdir -p ${prev}
  rm -rf ${prev}/*
  cp -r ${dir_to_backup}/* ${prev}
  rm -rf ${dir_to_backup}/*
}

echo "Beginning install"

echo "Backing up old UI installation"
backup ${backend_target}
backup ${frontend_target}
backup ${test_target}
echo "Done backing up old UI installation"

echo "Installing UI"
pushd ${build_dir} > /dev/null
cp -r *.rkt mock ${backend_target}
cp -r build/* ${frontend_target}
cp -r test/* ${test_target}
popd > /dev/null
echo "Done installing UI"

echo "Restarting UI service"
systemctl restart transltr
echo "Done restarting UI service"

echo "Done installing"