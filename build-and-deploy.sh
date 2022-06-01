#!/bin/sh

user=${TRANSLTR_USER}
host=${TRANSLTR_HOST}
while getopts b:f: arg; do
  case $arg in
  b) be_branch="${OPTARG}";;
  f) fe_branch="${OPTARG}";;
  ?) printf "Usage: %s: [-b <backend-branch>] [-f <frontend-branch>]\n" $0
     exit 2;;
  esac
done

if [ -z "${be_branch}" ]; then
  be_branch="main"
fi

git checkout ${be_branch}
git pull
./frontend-build.sh ${fe_branch}

echo "Packaging front end and back end build together"
mkdir -p ui-build
rm -r ui-build/*
cp -r *.sh *.rkt mock test build ui-build
echo "Transfering to \"\"\"production\"\"\" server"
rsync -avzhe ssh ui-build "${user}@${host}:/home/${user}"
echo "Installing on \"\"\"production\"\"\" server"
ssh ${user}@${host} "sudo /home/ubuntu/ui-build/install.sh"
