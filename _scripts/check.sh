#!/bin/bash

set -x

## Start jekyll server
bundle exec jekyll serve --trace &
SERVER=$!

## Wait for server to come up
perl -e 'alarm 300; exec @ARGV' -- bash -c "while ! echo exit | nc localhost 4000; do sleep 1; done"

## Wait a bit more for site to be available
sleep 5

## Read baseurl from _config.yml so the link checker hits the
## right entry point. Forks get baseurl rewritten to e.g.
## /erlang-org by the GitHub Actions workflow; upstream stays
## empty. Without this, blc 404s on / and the test step fails on
## every fork.
BASEURL=$(awk -F'"' '/^baseurl:/ {print $2; exit}' _config.yml)

## Run broken-link-checker
npx blc --exclude "erl_cmd.md" --exclude "%60/c:c/1%60" --exclude "http://localhost:4000${BASEURL}/doc" --exclude "http://localhost:4000${BASEURL}/docs" -re "http://localhost:4000${BASEURL}/"
RES=$?

## Kill the jekyll server
kill $SERVER
if [ "${RES}" != 0 ];
then
    echo "Found broken links!"
    exit 1
fi
