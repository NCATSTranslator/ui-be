#!/bin/bash

# Check if at least one argument is provided
if [ -z "$1" ]; then
    echo "Error: Argument 1 (string) is required."
    exit 1
fi

# Replace spaces in the first argument with underscores
arg1=$(echo "$1" | sed 's/ /_/g')

# Generate the timestamp with milliseconds 
epoch=$(date +%s%3N)


# Define the file name
filename="${epoch}.${arg1}.mjs"

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

  successMessage(obj=null) {
      return \`Ran successfully\`;
  }

}
EOF

# Notify the user
echo "File created: $filename"
