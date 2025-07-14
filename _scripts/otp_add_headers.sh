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


_fixup_search_link() {
    META_FULL_TEXT_SEARCH="<meta name=\"exdoc:full-text-search-url\""
    EXDOC_SEARCH=$(grep "${META_FULL_TEXT_SEARCH}" "$1" || echo "")
    if [ ! "$(echo "$EXDOC_SEARCH" | wc -w)" -gt "0" ]; then
        sed 's@\s*<title>@'"${META_FULL_TEXT_SEARCH}"' content="/doc/search.html?v='"${MAJOR_VSN}"'\&q=">\n&@g' -i -- "$1"
    fi
}

_fixup_major_version() {
    MAJOR_VSN_SEARCH=$(grep -P "<meta name=\"major-vsn\" content=\"\d+\"" "$1" || echo "")
    if [ ! "$(echo "$MAJOR_VSN_SEARCH" | wc -w)" -gt "0" ]; then
        sed 's@\s*<title>@<meta name="major-vsn" content="'"${MAJOR_VSN}"'">\n&@g' -i -- "$1"
    fi
}

_disable_autocomplete() {
    AUTOCOMPLETE_SEARCH=$(grep -P "<meta name=\"exdoc:autocomplete\" content=\"off\">" "$1" || echo "")
    if [ ! "$(echo "$AUTOCOMPLETE_SEARCH" | wc -w)" -gt "0" ]; then
        sed 's@\s*<title>@<meta name="exdoc:autocomplete" content="off">\n&@g' -i -- "$1"
    fi
}

_add_canonical() {
    canonical=${1/#"${TARGET_DIR}/"}
    ADD_CANONICAL=true
    if [ ! "$MAJOR_VSN" -gt "26" ]; then
        REDIRECT=$(grep -P "^/doc/.*?/?${canonical#apps\/}" "_redirects" || echo "")
        if [ "$(echo "${REDIRECT}" | wc -l)" -gt "0" ] && [ "$(echo "${REDIRECT}" | wc -w)" -gt "0" ]; then
            canonical=$(echo "${REDIRECT}" | tr " " "\n" | head -1 | sed 's/^\/doc\///')
            canonical=${canonical#/}
        else
            ADD_CANONICAL=false
        fi
    fi
    CANONICAL_GREP=$(grep -P "<link rel=\"canonical\"" "$1" || echo "")
    if [ "$(echo "${CANONICAL_GREP}" | wc -w)" -gt "0" ]; then
        sed 's@<link rel="canonical" href="[^"]*" />@<link rel="canonical" href="'"${CANONICAL_URL}${canonical}"'" />@g' -i -- "$1"
        ADD_CANONICAL=false
    fi
    if [ "$ADD_CANONICAL" = true ]; then
        sed 's@\s*<title>@<link rel="canonical" href="'"${CANONICAL_URL}""${canonical}"'" />\n&@g' -i -- "$1"
    fi
}

_add_head_tags() {
    find "${TARGET_DIR}" -type f -name "*.html" -print0 \
    | while IFS= read -r -d $'\0' file; do
        _fixup_major_version "${file}"
        if [ "$MAJOR_VSN" -gt "26" ]; then
            _fixup_search_link "${file}"
        fi
        _add_canonical "${file}"
    done
}

for MAJOR_VSN in ${MAJOR_VSNS}; do

    TARGET_DIR="${DOCS_DIR}/${MAJOR_VSN}"
    if [ "$MAJOR_VSN" = "doc" ]; then
        MAJOR_VSN="${LATEST_MAJOR_VSN}"
    elif [ "$MAJOR_VSN" -eq "1" ]; then # MAJOR_VSN = 1 means master-branch vsn
        MAJOR_VSN="${LATEST_MAJOR_VSN}"
    fi
    echo "Adding head tags for OTP ${MAJOR_VSN} in ${TARGET_DIR}"
    # shellcheck disable=SC2046
    _add_head_tags "${TARGET_DIR}"
    if [ "${MAJOR_VSN}" -gt "26" ]; then
        _disable_autocomplete "${TARGET_DIR}/"*.html
        _disable_autocomplete "${TARGET_DIR}/system/"*.html
    fi
done

exit 0
