#!/bin/bash

set -x

## Start jekyll server
bundle exec jekyll serve --trace &
SERVER=$!

## Wait for server to come up
timeout 5m bash -c "while ! echo exit | nc localhost 4000; do sleep 1; done"

## Wait a bit more for site to be available
sleep 5

## Run broken-link-checker
npx blc --exclude http://localhost:4000/doc --exclude http://localhost:4000/docs -re http://localhost:4000
RES=$?

## Kill the jekyll server
kill $SERVER
if [ "${RES}" != 0 ];
then
    echo "Found broken links!"
    exit 1
fi
