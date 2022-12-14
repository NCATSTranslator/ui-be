#!/bin/sh

set -e

usage()
{
    echo "$0 -b <be-branch-name> -f <fe-branch-name> -e <local|dev|test|ci|production> [-i <image_name>]"
    exit 1
}

if [ $# -lt 6 ]; then
    usage
    exit 1
fi

image_name="translator-app"
while getopts 'b:f:e:i:' opt
do
    case $opt in
        b) be_branch="$OPTARG" ;;
        f) fe_branch="$OPTARG" ;;
        e) app_env="$OPTARG" ;;
        i) image_name="$OPTARG" ;;
        ?) usage ;;
    esac
done

echo "fe-branch: $fe_branch; be_branch: $be_branch; app_env: $app_env; image_name: $image_name"

# This script ensures the BE repo is on the right branch,
# that the FE repo is cloned and on the right branch (via a script),
# and then builds and tags the image

save_branch=$(git branch --show-current)
echo "Checking out be-branch $be_branch"
git checkout $be_branch
git pull
be_tag=$(git rev-parse --short HEAD)
# Clone but do not build the FE dependencies; do that inside the container
./build-fe.sh "$fe_branch" "$app_env" no
cd ui-fe
fe_tag=$(git rev-parse --short HEAD)
cd ..
timestamp=$(date -u "+%Y.%m.%dt%H.%M.%Sz")
version_tag="FE.${fe_tag}_BE.${be_tag}_$timestamp.$app_env"
# Removing latest tag
docker build --no-cache -t "$image_name:$version_tag" -t "$image_name:$app_env" --build-arg APP_ENVIRONMENT="$app_env" .
echo "restoring branch $save_branch"
git checkout $save_branch
