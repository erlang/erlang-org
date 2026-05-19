#!/bin/bash

# This script takes all the html files and adds
# necessary html head tags before the deployment
#
# List of html tags added:
#
#    - <meta name="exdoc:full-text-search-url" \\
#        content="/doc/search.html?v=${MAJOR_VSN}&q="> (ex_doc < v0.39.0)
#
#    - <meta name="major-vsn" content="${MAJOR_VSN}">
#
#    - <meta name="exdoc:autocomplete" content="off">
#
#    - <link rel="canonical" href="https://www.erlang.org/doc/(...)"/>
#
# List of modified tags:
#    - data-engine-url="search.html?q=" to \\
#        data-engine-url="/doc/search.html?v=${MAJOR_VSN}&q=" (ex_doc >= v0.39.0)
#

set -e
set -o pipefail

# Portable in-place sed (BSD sed -i '' and GNU sed -i are incompatible)
_sed_i() {
    local expr="$1"
    local file="$2"
    sed "$expr" "$file" > "$file.tmp" && mv "$file.tmp" "$file"
}

# Insert a line before the first <title> tag.
# Portable across GNU and BSD sed (no \n in replacement needed).
_insert_before_title() {
    local tag="$1"
    local file="$2"
    sed "/<title>/i\\
$tag" "$file" > "$file.tmp" && mv "$file.tmp" "$file"
}

# Insert `tag` before <title> only if `marker` isn't already in the file.
_insert_before_title_once() {
    local marker="$1"
    local tag="$2"
    local file="$3"
    grep -q "$marker" "$file" || _insert_before_title "$tag" "$file"
}

DOCS_DIR="$1"
shift
LATEST_MAJOR_VSN="$(cat "LATEST_MAJOR_VSN")"
# shellcheck disable=SC2010
MAJOR_VSNS="$(ls "${DOCS_DIR}" | grep -v '\..*$')"
CANONICAL_URL="https://www.erlang.org/doc/"


_fixup_search_link() {
    local file
    local ex_doc_version_regex
    local ex_doc_major
    local ex_doc_minor

    file=$(cat "$1" || echo "")
    ex_doc_version_regex='<meta name="generator" content="ExDoc v([0-9]+)\.([0-9]+)\.([0-9]+)">'
    
    if [[ "$file" =~ $ex_doc_version_regex ]]; then
        ex_doc_major=${BASH_REMATCH[1]}
        ex_doc_minor=${BASH_REMATCH[2]}
    fi

    if [[ -n "$ex_doc_major" ]] && [[ "$ex_doc_major" -gt 0 || "$ex_doc_minor" -ge 39 ]]; then
        if grep -q 'data-engine-url="search.html?q="' "$1"; then
            _sed_i 's@data-engine-url="search.html?q="@data-engine-url="/doc/search.html?v='"${MAJOR_VSN}"'\&q="@g' "$1"
        fi
    else
        META_FULL_TEXT_SEARCH="<meta name=\"exdoc:full-text-search-url\""
        _insert_before_title_once "$META_FULL_TEXT_SEARCH" \
            "${META_FULL_TEXT_SEARCH} content=\"/doc/search.html?v=${MAJOR_VSN}\&q=\">" "$1"
    fi
}

_fixup_major_version() {
    _insert_before_title_once '<meta name="major-vsn" content="[0-9][0-9]*"' \
        "<meta name=\"major-vsn\" content=\"${MAJOR_VSN}\">" "$1"
}

_disable_autocomplete() {
    _insert_before_title_once '<meta name="exdoc:autocomplete" content="off">' \
        '<meta name="exdoc:autocomplete" content="off">' "$1"
}

_inject_algolia_typeahead() {
    # Skip if our marker is already there (the meta tag below is also
    # an idempotency guard on its own). Disable ExDoc's Lunr
    # autocomplete on the same pages so it doesn't render alongside
    # the Algolia dropdown.
    if ! grep -q "algolia-typeahead" "$1"; then
        _disable_autocomplete "$1"
        _insert_before_title '<link rel="stylesheet" href="/assets/css/algolia-typeahead.css">' "$1"
        _insert_before_title '<script src="/assets/js/algolia-typeahead.bundle.js" defer></script>' "$1"
    fi
}

_add_canonical() {
    canonical=${1/#"${TARGET_DIR}/"}
    ADD_CANONICAL=true
    if [ ! "$MAJOR_VSN" -gt "26" ]; then
        REDIRECT=$(grep "^/doc/.*?/?${canonical#apps\/}" "_redirects" || echo "")
        if [ "$(echo "${REDIRECT}" | wc -l)" -gt "0" ] && [ "$(echo "${REDIRECT}" | wc -w)" -gt "0" ]; then
            canonical=$(echo "${REDIRECT}" | tr " " "\n" | head -1 | sed 's/^\/doc\///')
            canonical=${canonical#/}
        else
            ADD_CANONICAL=false
        fi
    fi
    CANONICAL_GREP=$(grep "<link rel=\"canonical\"" "$1" || echo "")
    if [ "$(echo "${CANONICAL_GREP}" | wc -w)" -gt "0" ]; then
        _sed_i 's@<link rel="canonical" href="[^"]*" />@<link rel="canonical" href="'"${CANONICAL_URL}${canonical}"'" />@g' "$1"
        ADD_CANONICAL=false
    fi
    if [ "$ADD_CANONICAL" = true ]; then
        _insert_before_title "<link rel=\"canonical\" href=\"${CANONICAL_URL}${canonical}\" />" "$1"
    fi
}

_add_head_tags() {
    find "${TARGET_DIR}" -type f -name "*.html" -print0 \
    | while IFS= read -r -d $'\0' file; do
        _fixup_major_version "${file}"
        if [ "$MAJOR_VSN" -gt "26" ]; then
            _fixup_search_link "${file}"
            _inject_algolia_typeahead "${file}"
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
done

exit 0
