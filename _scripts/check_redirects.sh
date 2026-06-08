#!/bin/bash
#
# Smoke-test the documentation redirect behaviour against a live site.
#
# The extensionless redirects rely on the Netlify edge function
# (netlify/edge-functions/extensionless-redirect.js), its excludedPath
# config in netlify.toml, and the static rules in _redirects. None of
# that runs under `jekyll serve`, which is why check.sh has to exclude
# /doc and /docs from the link checker. So, like check_algolia_search.sh,
# this test runs against a real deployed site instead of the local build.
#
# It mainly guards two things:
#   * extensionless URLs still get redirected by the edge function -- if a
#     future excludedPath change is too greedy and swallows them, the
#     `redirect_to` assertions below start failing;
#   * the man-page rules in _redirects keep resolving to the real pages.
#
# Note: this cannot verify the *invocation count* dropped (the point of the
# excludedPath change) -- an excluded .html request and a passed-through one
# both return 200. That is only visible in the Netlify dashboard.
#
# Usage:  _scripts/check_redirects.sh [BASE_URL]
# BASE_URL defaults to https://www.erlang.org. Point it at a Netlify
# deploy-preview URL to test a branch before it goes live.

set -u
set -o pipefail

BASE="${1:-https://www.erlang.org}"
BASE="${BASE%/}"

FAILS=0

CURL=(curl -sS --max-time 30 --retry 2 --retry-delay 1)

# redirect_to PATH EXPECTED_LOCATION_SUFFIX
# First response must be a 301 whose Location ends with the suffix.
redirect_to() {
    local path="$1" want="$2"
    local headers code loc
    headers=$("${CURL[@]}" -o /dev/null -D - "${BASE}${path}") || {
        echo "FAIL  ${path}  (curl error)"; FAILS=$((FAILS + 1)); return
    }
    code=$(printf '%s' "$headers" | awk 'NR==1{print $2}')
    loc=$(printf '%s' "$headers" | awk -F': ' 'tolower($1)=="location"{print $2}' | tr -d '\r')
    if [[ "$code" == "301" && "$loc" == *"$want" ]]; then
        echo "ok    ${path}  301 -> ${loc}"
    else
        echo "FAIL  ${path}  expected 301 ending in '${want}', got ${code} '${loc}'"
        FAILS=$((FAILS + 1))
    fi
}

# serves_ok PATH
# Path is served directly with 200 and is not redirected (it is excluded
# from the edge function). Confirms the URL still resolves; it cannot tell
# whether the function was skipped or ran-and-passed-through.
serves_ok() {
    local path="$1"
    local code
    code=$("${CURL[@]}" -o /dev/null -w '%{http_code}' "${BASE}${path}") || {
        echo "FAIL  ${path}  (curl error)"; FAILS=$((FAILS + 1)); return
    }
    if [[ "$code" == "200" ]]; then
        echo "ok    ${path}  200 (served directly)"
    else
        echo "FAIL  ${path}  expected 200, got ${code}"
        FAILS=$((FAILS + 1))
    fi
}

# chain_ends PATH EXPECTED_FINAL_SUFFIX
# Following all redirects must land on a 200 whose URL ends with the suffix.
chain_ends() {
    local path="$1" want="$2"
    local out code final
    out=$("${CURL[@]}" -L -o /dev/null -w '%{http_code} %{url_effective}' "${BASE}${path}") || {
        echo "FAIL  ${path}  (curl error)"; FAILS=$((FAILS + 1)); return
    }
    code=${out%% *}
    final=${out#* }
    if [[ "$code" == "200" && "$final" == *"$want" ]]; then
        echo "ok    ${path}  -> ${final}"
    else
        echo "FAIL  ${path}  expected final 200 ending in '${want}', got ${code} '${final}'"
        FAILS=$((FAILS + 1))
    fi
}

echo "Checking documentation redirects against ${BASE}"

## Extensionless URLs the edge function must still redirect.
## Directory page -> trailing slash; leaf page -> .html sibling.
redirect_to /doc/apps/erts             /doc/apps/erts/
redirect_to /doc/apps/erts/erlang      /doc/apps/erts/erlang.html
redirect_to /doc                       /doc/

## Same, but under the versioned /docs/* tree (separate edge_functions
## declaration). 28 is an ExDoc-era version that stays published.
redirect_to /docs/28/apps/erts/erlang  /docs/28/apps/erts/erlang.html

## Extension-bearing and trailing-slash URLs are excluded from the edge
## function and served straight from the CDN.
serves_ok /doc/apps/erts/erlang.html
serves_ok /doc/apps/erts/

## Static _redirects man-page rule resolves through to the real module page.
chain_ends /doc/man/erlang  /doc/apps/erts/erlang.html

if [[ "$FAILS" -ne 0 ]]; then
    echo "Found ${FAILS} redirect failure(s)!"
    exit 1
fi
echo "All redirect checks passed."
