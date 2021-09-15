#!/bin/bash

set -x
## We use rjson as described here: https://github.com/stedolan/jq/wiki/FAQ#processing-not-quite-valid-json
## to remove comments from the docsearch.json file
CONFIG=$(npx rjson docsearch.json | jq -r tostring)
exec docker run -it -e "APPLICATION_ID=${APPLICATION_ID}" -e "API_KEY=${API_KEY}" -e "CONFIG=${CONFIG}" algolia/docsearch-scraper
