#!/bin/bash


if [ "$#" -ne 1 ]; then
    echo "Usage: $0 \"exactly one arg that can be a string\""
    exit 1
fi
descr="$1"

# Replace spaces in the first argument with underscores
descr_filename=$(echo "$1" | sed 's/ /_/g')

# Generate the timestamp with milliseconds
epoch=$(date +%s%3N)
if [[ "$epoch" == *N ]]; then
    echo "Please rerun in a modern shell that supports the additional millisecond-level in the date command. Exiting."
    exit 1
fi

# Define the file name
filename="${epoch}.${descr_filename}.mjs"

if ls "${epoch}."* 1> /dev/null 2>&1; then
    echo "Error: A file with the prefix '${epoch}.' already exists."
    exit 1
fi

# Write the content to the file using a HERE document
cat <<EOF > "$filename"
'use strict';

import { pg, pgExec } from '../../lib/postgres_preamble.mjs';

import { BaseMigration } from './BaseMigration.mjs';

export { Migration_${epoch} };

class Migration_${epoch} extends BaseMigration {

  static identifier = '${epoch}';

  constructor(dbPool) {
      super(dbPool);
      this.sql = [
        //'put',
        //'statements',
        //'here'
      ];
  }

  // override execute() only if you must

  async verify(obj=null) {
      return true;
  }

  success_message(obj=null) {
      return \`${descr}\`;
  }

}
EOF

# Notify the user
echo "File created: $filename"
