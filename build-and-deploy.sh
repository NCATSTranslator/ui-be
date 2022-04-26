#!/bin/sh

user=${TRANSLTR_USER}
host=${TRANSLTR_HOST}
echo "Switching to main branch"
git checkout main
./frontend-build.sh

echo "Packaging front end and back end build together"
mkdir -p ui-build
rm -r ui-build/*
cp -r *.sh *.rkt mock test build ui-build
echo "Transfering to \"\"\"production\"\"\" server"
rsync -avzhe ssh ui-build "${user}@${host}:/home/${user}"
echo "Installing on \"\"\"production\"\"\" server"
ssh ${user}@${host} "sudo /home/ubuntu/ui-build/install.sh"