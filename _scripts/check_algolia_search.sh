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

# Crawler-shape regression checks. Each one targets a specific
# breakage the crawler is supposed to prevent. See _crawler.js.
#
# These currently FAIL — they encode bugs we plan to fix in the
# crawler. They flip green automatically on the next crawl after
# _crawler.js is redeployed.

_algolia_query() {
    local filter="$1" query="$2" count="${3:-10}"
    curl -fsS -X POST \
        "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
        -H "X-Algolia-Application-Id: ${APP_ID}" \
        -H "X-Algolia-API-Key: ${API_KEY}" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg q "$query" --arg f "$filter" --argjson n "$count" \
                '{query: $q, filters: $f, hitsPerPage: $n, analytics: false}')"
}

_report() {
    local ok="$1" label="$2" detail="$3"
    if [ "$ok" = "1" ]; then
        PASS=$((PASS + 1))
        printf '  \033[32mOK\033[0m   %-60s -> %s\n' "$label" "$detail"
    else
        FAIL=$((FAIL + 1))
        printf '  \033[31mFAIL\033[0m %-60s -> %s\n' "$label" "$detail"
    fi
}

# Helpers used by several test groups below.

_check_present() {
    local path="$1" query="$2" appname="$3"
    resp=$(_algolia_query "version:${VERSION} AND app:\"$appname\"" "$query")
    found=$(echo "$resp" | jq -r --arg u "$path" \
        '[.hits[] | select(.url | contains($u))] | length')
    if [ "$found" -gt 0 ]; then
        _report 1 "[crawler] indexed ${path}" "present"
    else
        _report 0 "[crawler] indexed ${path}" "missing"
    fi
}

_check_lvl2() {
    # Use endswith rather than contains so that paths like
    # `#enif_alloc` don't accidentally match `#enif_alloc_binary`.
    local path="$1" expected_prefix="$2" appname="$3" query="$4"
    resp=$(_algolia_query "version:${VERSION} AND app:\"$appname\"" "$query")
    got=$(echo "$resp" | jq -r --arg u "$path" \
        '.hits[] | select(.url | endswith($u)) | .hierarchy.lvl2' | head -1)
    if [[ "$got" == "$expected_prefix"* ]]; then
        _report 1 "[crawler] lvl2 ${path}" "${got}"
    else
        _report 0 "[crawler] lvl2 ${path}" "got '${got}', want prefix '${expected_prefix}'"
    fi
}

_check_lvl3() {
    local path="$1" expected_prefix="$2" appname="$3" query="$4"
    resp=$(_algolia_query "version:${VERSION} AND app:\"$appname\"" "$query")
    got=$(echo "$resp" | jq -r --arg u "$path" \
        '.hits[] | select(.url | contains($u)) | .hierarchy.lvl3' | head -1)
    if [[ "$got" == "$expected_prefix"* ]]; then
        _report 1 "[crawler] lvl3 ${path}" "${got}"
    else
        _report 0 "[crawler] lvl3 ${path}" "got '${got}', want prefix '${expected_prefix}'"
    fi
}

_check_content_prefix() {
    local path="$1" expected_prefix="$2" appname="$3" query="$4"
    resp=$(_algolia_query "version:${VERSION} AND app:\"$appname\"" "$query")
    got=$(echo "$resp" | jq -r --arg u "$path" \
        '.hits[] | select(.url | contains($u)) | .content' | head -1)
    if [[ "$got" == "$expected_prefix"* ]]; then
        _report 1 "[crawler] content-prefix ${path}" "starts with '${expected_prefix}'"
    else
        _report 0 "[crawler] content-prefix ${path}" \
            "got '$(printf "%s" "$got" | head -c 60)', want prefix '${expected_prefix}'"
    fi
}

# 1. Module-overview URLs must not have a trailing '#'.
#    `lists.html#` is ugly in the address bar after click-through.
#    Algolia rejects filter-only (empty-query) requests, so we
#    query for the module name and pick the overview hit by URL
#    shape.
for mod in lists gen_server ssl crypto; do
    case "$mod" in
        ssl|crypto) app="$mod" ;;
        *)          app="stdlib" ;;
    esac
    resp=$(_algolia_query "version:${VERSION} AND app:\"${app}\"" "$mod")
    overview_url=$(echo "$resp" | jq -r --arg m "${mod}.html" \
        '.hits[] | select(.url | endswith($m) or endswith($m + "#")) | .url' \
        | head -1)
    if [ -z "$overview_url" ]; then
        _report 0 "[crawler] no-trailing-# ${mod}" "no module-overview record found"
    elif [[ "$overview_url" == *# ]]; then
        _report 0 "[crawler] no-trailing-# ${mod}" "got ${overview_url}"
    else
        _report 1 "[crawler] no-trailing-# ${mod}" "${overview_url}"
    fi
done

# 2. Code tokens in indexed content must not be space-separated by
#    the syntax-highlight <span> teardown. We look for the mangled
#    pattern `mod : func` adjacent to the unmangled `mod:func`
#    appearing nowhere — testing absence of the mangled form is the
#    simpler invariant.
for pair in "lists|append" "lists|subtract" "ssl|connect" "ets|new"; do
    mod="${pair%|*}"; func="${pair#*|}"
    case "$mod" in
        ssl) app="ssl" ;;
        *)   app="stdlib" ;;
    esac
    resp=$(_algolia_query "version:${VERSION} AND app:\"${app}\"" "${mod}:${func}")
    mangled=$(echo "$resp" | jq -r --arg p "${mod} : ${func}" \
        '[.hits[] | select(.content != null and (.content | contains($p)))] | length')
    if [ "$mangled" = "0" ]; then
        _report 1 "[crawler] unmangled-code ${mod}:${func}" "no '${mod} : ${func}' in any hit"
    else
        _report 0 "[crawler] unmangled-code ${mod}:${func}" \
            "${mangled} hit(s) still contain '${mod} : ${func}'"
    fi
done

# 3. `kind` must reflect function / type / callback. Without this,
#    callback `c:init/1` and function `init/1` look identical in
#    results. The default attributesToRetrieve set on the index
#    omits `kind`, so the test must request it explicitly.
_check_kind() {
    local app="$1" path="$2" expected="$3" query="$4"
    resp=$(curl -fsS -X POST \
        "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
        -H "X-Algolia-Application-Id: ${APP_ID}" \
        -H "X-Algolia-API-Key: ${API_KEY}" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg q "$query" \
                    --arg f "version:${VERSION} AND app:\"${app}\"" \
                '{query: $q, filters: $f, hitsPerPage: 10, analytics: false,
                  attributesToRetrieve: ["url", "kind"]}')")
    got=$(echo "$resp" | jq -r --arg u "$path" \
        '.hits[] | select(.url | contains($u)) | .kind' | head -1)
    if [ "$got" = "$expected" ]; then
        _report 1 "[crawler] kind ${path}" "${got}"
    else
        _report 0 "[crawler] kind ${path}" "got '${got}', want '${expected}'"
    fi
}
_check_kind "stdlib" "/apps/stdlib/supervisor.html#c:init/1"      "callback" "supervisor:init"
_check_kind "stdlib" "/apps/stdlib/gen_server.html#c:handle_cast/2" "callback" "gen_server:handle_cast"
_check_kind "stdlib" "/apps/stdlib/gen_server.html#t:server_ref/0" "type"     "gen_server server_ref"
_check_kind "stdlib" "/apps/stdlib/lists.html#append/2"           "function" "lists:append"

# 4. Section records that have no narrative under the heading must
#    be dropped from the index — they produce empty-excerpt hits.
#    Module-overview records (lvl2 is null) are exempt: they're the
#    landing page itself, kept regardless of content.
#
#    A specific URL doesn't make a good fixture here: anchors like
#    `options-accepted-by-escript` get reused by parseDefinitionList
#    as the fallback anchor for child <dt>/<dd> records, so the URL
#    can still appear via the children even after the parent shell
#    is dropped. Instead, assert globally that no non-overview hit
#    in a broad sample has empty content.
resp=$(curl -fsS -X POST \
    "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
    -H "X-Algolia-Application-Id: ${APP_ID}" \
    -H "X-Algolia-API-Key: ${API_KEY}" \
    -H "Content-Type: application/json" \
    -d "$(jq -n --arg f "version:${VERSION}" \
            '{query: "", filters: $f, hitsPerPage: 1000, analytics: false,
              attributesToRetrieve: ["url", "content", "hierarchy.lvl2"]}')")
empty_count=$(echo "$resp" | jq \
    '[.hits[] | select((.content == "" or .content == null)
                       and .hierarchy.lvl2 != null)] | length')
if [ "$empty_count" = "0" ]; then
    _report 1 "[crawler] no-empty-content (sample of 1000)" "0 empty-content non-overview hits"
else
    _report 0 "[crawler] no-empty-content (sample of 1000)" \
        "${empty_count} hit(s) with empty content but non-null lvl2"
fi

# 4b. Chapter-page overview records (reference-manual chapters,
#     other user-guide pages where h1 is immediately followed by
#     h2) must have a useful excerpt — without the crawler's
#     fallback they'd be title-only hits because the h1-to-h2
#     gap is empty of narrative text.
_check_overview_content() {
    local path="$1" expected_prefix="$2" appname="$3" query="$4"
    resp=$(curl -fsS -X POST \
        "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
        -H "X-Algolia-Application-Id: ${APP_ID}" \
        -H "X-Algolia-API-Key: ${API_KEY}" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg q "$query" \
                    --arg f "version:${VERSION} AND app:\"$appname\"" \
                '{query: $q, filters: $f, hitsPerPage: 20, analytics: false,
                  attributesToRetrieve: ["url", "content"]}')")
    got=$(echo "$resp" | jq -r --arg u "$path" \
        '.hits[] | select(.url | endswith($u) or endswith($u + "#")) | .content' | head -1)
    if [[ "$got" == "$expected_prefix"* ]]; then
        _report 1 "[crawler] overview-content ${path}" "starts with '${expected_prefix}'"
    else
        _report 0 "[crawler] overview-content ${path}" \
            "got '$(printf "%s" "$got" | head -c 60)', want prefix '${expected_prefix}'"
    fi
}
_check_overview_content "/system/typespec.html" \
    "Erlang is a dynamically typed language" \
    "Erlang System Documentation" "types function specifications"
_check_overview_content "/system/modules.html" \
    "Erlang code is divided into" \
    "Erlang System Documentation" "modules"
_check_overview_content "/system/ref_man_functions.html" \
    "A function declaration is a sequence of function clauses" \
    "Erlang System Documentation" "function declaration"

# 5. Searching "reference manual" must surface multiple chapter
#    overview records. The chapters (typespec, modules,
#    ref_man_functions, ...) are user-guide pages whose
#    module-overview record has empty content; without the
#    anchor==='' carve-out in the filter, they'd vanish from the
#    index. Today they're the top-15 hits for that query — assert
#    we still see ≥10 of them.
resp=$(_algolia_query "version:${VERSION}" "reference manual" 20)
chapter_hits=$(echo "$resp" | jq -r \
    '[.hits[] | select(.url | test("/system/[a-z_]+\\.html#?$"))] | length')
if [ "$chapter_hits" -ge 10 ]; then
    _report 1 "[crawler] reference-manual-chapters" "${chapter_hits} chapter pages"
else
    _report 0 "[crawler] reference-manual-chapters" \
        "${chapter_hits} chapter pages (want ≥10)"
fi
# Verify a representative subset by URL.
for chapter in reference_manual typespec modules ref_man_functions ref_man_records expressions; do
    found=$(echo "$resp" | jq -r --arg u "/system/${chapter}.html" \
        '[.hits[] | select(.url | endswith($u) or endswith($u + "#"))] | length')
    if [ "$found" -gt 0 ]; then
        _report 1 "[crawler] chapter-indexed ${chapter}" "present"
    else
        _report 0 "[crawler] chapter-indexed ${chapter}" "missing from 'reference manual' results"
    fi
done

# 6. Sanity: section records with real content must stay indexed
#    (they have non-empty content, so the filter shouldn't touch
#    them — this guards against the filter being too aggressive).
_check_present "/system/modules.html#module-syntax"                       "module syntax"        "Erlang System Documentation"
_check_present "/system/ref_man_functions.html#function-declaration-syntax" "function declaration" "Erlang System Documentation"
_check_present "/system/typespec.html#types-and-their-syntax"             "types syntax"         "Erlang System Documentation"

# 7. Definition-list parsing. ExDoc renders markdown definition
#    lists as `<ul><li><p><strong>term</strong> ...`, and
#    parseDefinitionList walks that shape to emit one record per
#    dt/dd pair with hierarchy.lvl3 = term text. If ExDoc ever
#    changes the wrapper (e.g. drops the <strong>), this whole
#    code path silently stops emitting records — there's no error,
#    the items just vanish from the index.
_check_present "/apps/erts/erl_cmd.html#+pad"           "+pad emulator flag"   "erts"
_check_present "/apps/erts/erl_cmd.html#emulator-flags" "emulator flags"       "erts"

# lvl3 must hold the dt term (with whatever modifier text follows
# inside the <strong>). Catches the case where parseDefinitionList
# emits records but loses the term text. _check_lvl3 picks the
# top-ranked hit for the query whose URL contains the path — fine
# when each dl-item has its own anchor (erl_cmd flags) and also
# fine when dl-items share the parent's anchor (errors#exit-reasons,
# erts_alloc#allocators) as long as the query disambiguates.
_check_lvl3 "/apps/erts/erl_cmd.html#+pad"               "+pad"        "erts"   "+pad emulator flag"
_check_lvl3 "/apps/erts/erl_cmd.html#file_name_encoding" "+fnl"        "erts"   "+fnl filename encoding"
_check_lvl3 "/system/errors.html#exit-reasons"           "system_limit" "Erlang System Documentation" "system_limit exit reason"

# A dl-item's content should lead with the dd body, not the dt
# term — the dt is already in hierarchy.lvl3, so leaving it on
# the front of content duplicates the term in the snippet. The
# crawler strips the dt + separator before indexing.
_check_content_prefix "/apps/erts/erl_cmd.html#+pad"     "The boolean value used with the +pad" "erts" "+pad emulator flag"
_check_content_prefix "/system/errors.html#exit-reasons" "A system limit has been reached" "Erlang System Documentation" "system_limit exit reason"

# A dl with many items must produce many child records — guards
# against a regression where the parser bails out after the first
# item or only picks up the last one.
#
# The index has `distinct: true, attributeForDistinct: "url"`, so
# dl-items that share an anchor (because their <li> has no inner
# [id]) collapse to one hit per query — `distinct: false` in the
# request bypasses that so we can count them all.
_count_dl_children() {
    local label="$1" query="$2" appname="$3" lvl2="$4" url_contains="$5"
    local want="$6"
    resp=$(curl -fsS -X POST \
        "https://${APP_ID}-dsn.algolia.net/1/indexes/${INDEX}/query" \
        -H "X-Algolia-Application-Id: ${APP_ID}" \
        -H "X-Algolia-API-Key: ${API_KEY}" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg q "$query" \
                    --arg f "version:${VERSION} AND app:\"${appname}\"" \
                '{query: $q, filters: $f, hitsPerPage: 200, distinct: false,
                  analytics: false,
                  attributesToRetrieve: ["url", "hierarchy.lvl2", "hierarchy.lvl3"]}')")
    count=$(echo "$resp" | jq --arg l "$lvl2" --arg u "$url_contains" \
        '[.hits[] | select(.hierarchy.lvl2 == $l
                           and .hierarchy.lvl3 != null
                           and (.url | contains($u)))] | length')
    if [ "$count" -ge "$want" ]; then
        _report 1 "[crawler] ${label}" "${count} children"
    else
        _report 0 "[crawler] ${label}" "${count} children (want ≥${want})"
    fi
}

_count_dl_children "dl-children erl_cmd Emulator Flags" \
    "emulator flags" "erts" "Emulator Flags" "erl_cmd" 15

# erts_alloc has two distinct dl sections — "Allocators" (each
# allocator name as a dt; <li> elements have no inner id so all
# child records share #allocators as their URL anchor) and
# "System Flags Effecting erts_alloc" (each flag-group heading as
# a dt). Both shapes should produce many records.
_check_lvl3          "/apps/erts/erts_alloc.html#allocators" "temp_alloc"  "erts" "temp_alloc allocator"
_check_content_prefix "/apps/erts/erts_alloc.html#allocators" "Allocator used for temporary" "erts" "temp_alloc allocator"
_check_lvl3 \
    "/apps/erts/erts_alloc.html#flags-for-configuration-of-alloc_util" \
    "Flags for Configuration of alloc_util" \
    "erts" "flags for configuration of alloc_util"
_count_dl_children "dl-children erts_alloc Allocators" \
    "erts_alloc allocators" "erts" "Allocators" "erts_alloc" 5

# erl_nif (the NIF C API) renders its "Data Types" section as a
# definition list — each typedef name is a dt. These records have
# lvl2 = "Data Types" and lvl3 = the type name. Different shape
# from the function records (which are extracted via
# section.detail), so worth its own coverage.
_check_lvl3 \
    "/apps/erts/erl_nif.html#ErlNifResourceType" "ErlNifResourceType" \
    "erts" "ErlNifResourceType NIF resource"
_check_content_prefix \
    "/apps/erts/erl_nif.html#ErlNifResourceType" "Each instance of ErlNifResourceType represents" \
    "erts" "ErlNifResourceType NIF resource"
_count_dl_children "dl-children erl_nif Data Types" \
    "erl_nif data types" "erts" "Data Types" "erl_nif" 5

# erl_nif also has many C-function records (the bulk of the page,
# extracted via the functions path, not parseDefinitionList). lvl2
# should hold the full C signature. Catches a regression where the
# signature collector picks up just the function name or empties.
_check_lvl2 \
    "/apps/erts/erl_nif.html#enif_alloc" "enif_alloc()" \
    "erts" "enif_alloc NIF"
_check_lvl2 \
    "/apps/erts/erl_nif.html#enif_make_atom" "enif_make_atom()" \
    "erts" "enif_make_atom"

# 8. Sanity: function records always have hierarchy.lvl2 set to
#    the full signature (mod-level lvl1, signature-level lvl2).
#    Catches a regression where the function-extractor stops
#    picking up signatures.
_check_lvl2 "/apps/stdlib/lists.html#append/2"      "append(List1, List2)" "stdlib" "lists:append"
_check_lvl2 "/apps/stdlib/gen_server.html#cast/2"   "cast(ServerRef"       "stdlib" "gen_server:cast"
_check_lvl2 "/apps/ssl/ssl.html#connect/4"          "connect(Host, Port"   "ssl"    "ssl:connect"

echo
echo "Search regression check: ${PASS} passed, ${FAIL} failed (version ${VERSION})"
[ "$FAIL" -eq 0 ]
