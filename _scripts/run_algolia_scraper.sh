#!/bin/bash

set -x
CONFIG=$(cat docsearch.json | jq -r tostring)
docker run -it -e "APPLICATION_ID=${APPLICATION_ID}" -e "API_KEY=${API_KEY}" -e "CONFIG=${CONFIG}" algolia/docsearch-scraper
