#!/bin/sh

base="$( cd -- "$(dirname "$0")" >/dev/null 2>&1 ; pwd -P )"
pgdb_image=$1
path_to_data=$2
db_password=$3
if [ -z "$docker_name" ]; then
  pgdb_image='postgres:latest'
fi

if [ -z "$path_to_data" ]; then
  path_to_data="${base}/../assets/db/pref_data.csv"
fi

if [ -z "$db_password" ]; then
  db_password=$(cat "${base}/../configurations/secrets/db_password")
fi

target_volume=/var/lib/postgresql/data
docker run --rm --name pgdb -e POSTGRES_PASSWORD=${db_password} -d -p 5432:5432 \
    -v ~/translator/pgdata:${target_volume} \
    -v ${path_to_data}:${target_volume}/pref_data.csv \
    ${pgdb_image}

if [ $? -ne 0 ]; then
  echo "Failed to start database container"
  exit 1
fi

# Allow the container and database ample time to startup
echo "Waiting for database to start..."
sleep 30
docker exec -i pgdb psql -U postgres -d app_data -a -f - < ${base}/sql/populatePreferencesTable.sql
docker stop pgdb > /dev/null
echo "Done"