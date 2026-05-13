#!/bin/bash
#
# Run a small set of well-known doc searches against Algolia and check
# that each one returns the expected URL in its top 5 hits. Catches
# the scraper-vs-ExDoc-HTML-shape drift that has bitten us before:
# when ExDoc moves a CSS class or restructures a page, the scraper's
# selectors silently produce different (or empty) records, and the
# only visible symptom is that searches like "lists" stop finding
# lists.html.
#
# Usage:  _scripts/check_algolia_search.sh [VERSION]
# VERSION defaults to whatever is in ./LATEST_MAJOR_VSN.

set -e
set -o pipefail

APP_ID="LUYTU1J2MB"
API_KEY="86152ba1d4a9d7e179d537b8060a4c31"
INDEX="erlang"

VERSION="${1:-$(cat LATEST_MAJOR_VSN 2>/dev/null || echo 28)}"

# Each test: filter | query | expected_url_substring | expected_lvl1
# The lvl1 column is optional; leave it empty to skip the title check.
# Filter syntax is Algolia's; "app" is the indexed application name.
TESTS=(
    'app:"stdlib"|lists|/apps/stdlib/lists.html|'
    'app:"stdlib"|gen_server|/apps/stdlib/gen_server.html|'
    'app:"stdlib"|binary|/apps/stdlib/binary.html|'
    'app:"kernel"|file|/apps/kernel/file.html|'
    'app:"erts"|erlang|/apps/erts/erlang.html|'
    # Cross-app core-group case: "file" lives in kernel but we expect
    # to find it when searching stdlib + kernel + erts together.
    'app:"stdlib" OR app:"kernel" OR app:"erts"|file|/apps/kernel/file.html|'
    'app:"ssl"|ssl|/apps/ssl/ssl.html|'
    'app:"crypto"|crypto|/apps/crypto/crypto.html|'
    'app:"public_key"|public_key|/apps/public_key/public_key.html|'
    'app:"compiler"|compile|/apps/compiler/compile.html|'
    'app:"common_test"|ct|/apps/common_test/ct.html|'
    'app:"eunit"|eunit|/apps/eunit/eunit.html|'
    'app:"Erlang System Documentation"|reference manual|/system/reference_manual.html|'
    # Section-anchor and page-summary hits where the scraper should
    # carry the page-level <h1> in lvl1. These currently fail because
    # the scraper's selectors are tuned for ExDoc's module-page
    # layout and miss the <h1> on user guides, system docs, and
    # command-line tool pages. Same underlying bug across thousands
    # of records — these three exemplify the common shapes.
    'app:"Erlang System Documentation"|macro|/system/records_macros.html#macros|Records and Macros'
    'app:"common_test"|write test suites|/apps/common_test/write_test_chapter.html|Writing Test Suites'
    'app:"erts"|werl|/apps/erts/werl_cmd.html|werl'
)

PASS=0
FAIL=0

for test in "${TESTS[@]}"; do
    IFS='|' read -r filter query expected_url expected_lvl1 <<< "$test"
    full_filter="version:${VERSION} AND ${filter}"
    label="[$filter] $query"

    response=$(curl -fsS -X POST \
        "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
        -H "X-Algolia-Application-Id: ${APP_ID}" \
        -H "X-Algolia-API-Key: ${API_KEY}" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg q "$query" --arg f "$full_filter" \
                '{query: $q, filters: $f, hitsPerPage: 5, analytics: false,
                  attributesToRetrieve: ["url", "hierarchy.lvl1"]}')")

    # Pick the first hit whose URL contains the expected substring.
    match=$(echo "$response" | jq -r --arg u "$expected_url" \
        '.hits[] | select(.url | contains($u)) | {url, lvl1: .hierarchy.lvl1} | @json' \
        | head -1)

    if [ -z "$match" ]; then
        FAIL=$((FAIL + 1))
        urls=$(echo "$response" | jq -r '.hits[].url' || echo "")
        printf '  \033[31mFAIL\033[0m %-60s -> %s\n' "$label" "$expected_url"
        printf '       got: %s\n' "${urls:-<no hits>}"
        continue
    fi

    if [ -n "$expected_lvl1" ]; then
        got_lvl1=$(echo "$match" | jq -r '.lvl1')
        if [ "$got_lvl1" != "$expected_lvl1" ]; then
            FAIL=$((FAIL + 1))
            printf '  \033[31mFAIL\033[0m %-60s -> lvl1 "%s"\n' "$label" "$expected_lvl1"
            printf '       got lvl1: "%s"\n' "$got_lvl1"
            continue
        fi
    fi

    PASS=$((PASS + 1))
    printf '  \033[32mOK\033[0m   %-60s -> %s\n' "$label" "$expected_url"
done

echo
echo "Search regression check: ${PASS} passed, ${FAIL} failed (version ${VERSION})"
[ "$FAIL" -eq 0 ]
