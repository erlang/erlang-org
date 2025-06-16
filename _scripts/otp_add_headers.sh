#!/bin/bash

# This script takes all the html files and adds
# necessary html head tags before the deployment
#
# List of html tags added:
#
#    - <meta name="exdoc:full-text-search-url" \\
#        content="/doc/search.html?v=${MAJOR_VSN}&q=">
#
#    - <meta name="major-vsn" content="${MAJOR_VSN}">
#
#    - <meta name="exdoc:autocomplete" content="off">
#
#    - <link rel="canonical" href="https://www.erlang.org/doc/(...)"/>
#

set -e
set -o pipefail

DOCS_DIR="$1"
shift
LATEST_MAJOR_VSN="$(cat "LATEST_MAJOR_VSN")"
MAJOR_VSNS="$(ls -I "*\.*" "${DOCS_DIR}")"
CANONICAL_URL="https://www.erlang.org/doc/"

_app_name() {
    echo "$1" | awk -F- '{print $1}'
}

_fixup_search_link() {
    META_CONTENT_PREFIX="<meta name=\"exdoc:full-text-search-url\" content=\"\/doc\/search\.html\?v="
    EXDOC_SEARCH=$(grep -P "${META_CONTENT_PREFIX}(?!(?:1\&))\d+" "$1" || echo "")
    if [ $(echo "$EXDOC_SEARCH" | wc -w) -gt "0" ]; then
        sed '/content="\/doc\/search.html?v=1/! s@'"${META_CONTENT_PREFIX}"'[0-9]\+@'"${META_CONTENT_PREFIX}"''"${MAJOR_VSN}"'@' -i -- "$1"
    else
        sed 's@\s*<title>@'"${META_CONTENT_PREFIX}"''"${MAJOR_VSN}"'\&q=">\n&@g' -i -- "$@"
    fi
}

_fixup_major_version() {
    MAJOR_VSN_SEARCH=$(grep -P "<meta name=\"major-vsn\" content=\"\d+\"" "$1" || echo "")
    if [ ! $(echo "$MAJOR_VSN_SEARCH" | wc -w) -gt "0" ]; then
        sed 's@\s*<title>@<meta name="major-vsn" content="'"${MAJOR_VSN}"'">\n&@g' -i -- "$@"
    fi
}

_disable_autocomplete() {
    AUTOCOMPLETE_SEARCH=$(grep -P "<meta name=\"exdoc:autocomplete\" content=\"off\">" "$1" || echo "")
    if [ ! $(echo "$AUTOCOMPLETE_SEARCH" | wc -w) -gt "0" ]; then
        sed 's@\s*<title>@<meta name="exdoc:autocomplete" content="off">\n&@g' -i -- "$@"
    fi
}

_add_canonical() {
    i=0
    for file in "$@"; do
        canonical=${file/#"${TARGET_DIR}/"}
        ADD_CANONICAL=true
        if [ ! "$MAJOR_VSN" -gt "26" ]; then
            REDIRECT=$(grep -P "^/doc/.*?/?${canonical#apps\/}" "_redirects" || echo "")
            if [ $(echo "${REDIRECT}" | wc -l) -gt "0" ] && [ $(echo "${REDIRECT}" | wc -w) -gt "0" ]; then
                canonical=$(echo "${REDIRECT}" | tr " " "\n" | head -1 | sed 's/^\/doc\///')
                canonical=${canonical#/}
            else
                ADD_CANONICAL=false
            fi
        fi
        if [ "$ADD_CANONICAL" = true ]; then
            sed 's@\s*<title>@<link rel="canonical" href="'"${CANONICAL_URL}""${canonical}"'" />\n&@g' -i -- "${file}"
        fi
    i=$((i+1))
    echo -ne "done ${i} of $# files\r"
    done
}

_exdoc_headers() {
    _fixup_search_link "$@"
    _disable_autocomplete "${TARGET_DIR}/"*.html
    _disable_autocomplete "${TARGET_DIR}/system/"*.html
}

_add_head_tags() {
    _fixup_major_version "$@"
    if [ "$MAJOR_VSN" -gt "26" ]; then
        _exdoc_headers "$@"
    fi
    _add_canonical "$@"
}

for MAJOR_VSN in ${MAJOR_VSNS}; do

    TARGET_DIR="${DOCS_DIR}/${MAJOR_VSN}"
    if [ "$MAJOR_VSN" = "doc" ]; then
        MAJOR_VSN="${LATEST_MAJOR_VSN}"
    elif [ "$MAJOR_VSN" -eq "1" ]; then
        MAJOR_VSN="${LATEST_MAJOR_VSN}"
    fi
    echo "Adding head tags for OTP ${MAJOR_VSN} in ${TARGET_DIR}"
    # FOUND=$($(find "${TARGET_DIR}" -type f -name "*.html" -print0 | xargs -0 echo "$1") || echo "")
    _add_head_tags $(find "${TARGET_DIR}" -type f -name "*.html" -print0 | xargs -0 echo "$1" || echo "" | awk -F"\n" '{print $1}')

done

exit 0
