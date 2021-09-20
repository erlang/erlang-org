#!/bin/bash

set -x

bundle exec jekyll serve --trace &
SERVER=$!
timeout 5m bash -c "while ! echo exit | nc localhost 4000; do sleep 1; done"
npx blc --exclude http://localhost:4000/doc --exclude http://localhost:4000/docs -re http://localhost:4000
RES=$?
kill $SERVER
if [ "${RES}" != 0 ];
then
    echo "Found broken links!"
    exit 1
fi
