#!/bin/bash

# Generate directory index files for extensionless URL support.
# For each foo.html, creates foo/index.html with a meta refresh redirect.
# This way /doc/apps/erts/erlang serves erlang/index.html which redirects
# to erlang.html, without needing Netlify redirect rules or pretty URLs.

set -e

DOCS_DIR="$1"

find "${DOCS_DIR}" -name "*.html" -type f | while read -r file; do
    dir="${file%.html}"
    # Skip files that would conflict with existing directories
    [ -d "$dir" ] && continue
    basename=$(basename "$file")
    mkdir -p "$dir"
    echo "<meta http-equiv=\"refresh\" content=\"0; url=../${basename}\">" > "$dir/index.html"
done
