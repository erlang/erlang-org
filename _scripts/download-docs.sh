#!/bin/bash

## Download the docs from github if they are there, or else from erlang.org
## Then we flatten all the docs and put them in the docs/ folder
##
## We use rsync to sync from erlang.org in order to not be rate-limited

set -e

OTP_VERSIONS_TABLE=$1
TIME_LIMIT=${3:-120m}
TOKEN=${2:-"token ${GITHUB_TOKEN}"}
HDR=(--silent --location --fail --show-error -H "Authorization: ${TOKEN}" -H "X-GitHub-Api-Version: 2022-11-28")

# The files that are involved when generating docs
SCRIPT_FILES="${OTP_VERSIONS_TABLE} _scripts/download-docs.sh _scripts/otp_flatten_docs _scripts/otp_flatten_ex_docs _scripts/otp_doc_sitemap.sh LATEST_MAJOR_VSN _scripts/otp_add_headers.sh"

_get_vsns() {
    grep "${1}" "${OTP_VERSIONS_TABLE}" | awk '{print $1}' | sed 's/OTP-\(.*\)/\1/g'
}

_get_latest_vsn() {
    _get_vsns "${1}" | head -1
}

_get_doc_hash() {
    # shellcheck disable=SC2086
    (echo "${1}" && echo "${LATEST_MAJOR_VERSION}" && cat ${SCRIPT_FILES}) | sha256sum | awk '{print $1}'
}

_flatten_docs() {
    if [ -f "docs/doc-$1/doc/readme.html" ]; then
        (cd docs && ../_scripts/otp_flatten_ex_docs "doc-$1" "$2" "$3" "../${OTP_VERSIONS_TABLE}" "${MAJOR_VSNs}" "${MASTER_MAJOR_VSN}")
    else
        (cd docs && ../_scripts/otp_flatten_docs "doc-$1" "$2")
    fi
}

MAJOR_VSNs=$(_get_vsns "OTP-[0-9]\+\.0 " | sed 's/^\([0-9]\+\).*/\1/g')
LATEST_MAJOR_VSN=$(cat "LATEST_MAJOR_VSN")
RINCLUDE=()

for VSN in ${MAJOR_VSNs}; do
    LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}")
    ARCHIVE="docs/otp_doc_html_${LATEST_VSN}.tar.gz"

    if [ ! -f "${ARCHIVE}" ] && [ ! -f "docs/${VSN}/$(_get_doc_hash "${LATEST_VSN}")" ]; then
        echo "Checking for ${LATEST_VSN} on github"
        if ! curl "${HDR[@]}" "https://github.com/erlang/otp/releases/download/OTP-${LATEST_VSN}/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
            rm -f "${ARCHIVE}"
            LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}\.[0-9] ")
            if [ ! -f "docs/${VSN}/$(_get_doc_hash "${LATEST_VSN}")" ]; then
                echo "Checking for ${LATEST_VSN} on erlang.org::erlang-download"
                RINCLUDE=("--include=otp_doc_html_${LATEST_VSN}.tar.gz" "${RINCLUDE[@]}")
            else
                echo "${LATEST_VSN} already exists"
            fi
        fi
    else
        echo "${LATEST_VSN} already exists"
    fi
done

MASTER_MAJOR_VSN=$(( LATEST_MAJOR_VSN + 1 ))
MASTER_VSN="${MASTER_MAJOR_VSN}.0"
MASTER_SHA=$(curl "${HDR[@]}" https://api.github.com/repos/erlang/otp/commits/master | jq ".sha")
ARCHIVE="docs/otp_doc_html_${MASTER_VSN}.tar.gz"
MAJOR_VSNs="${MASTER_MAJOR_VSN} ${MAJOR_VSNs}"
if [ ! -f "${ARCHIVE}" ] && [ ! -f "docs/${MASTER_MAJOR_VSN}/$(_get_doc_hash "${MASTER_SHA}")" ]; then
    echo "Checking for ${MASTER_VSN} on github"
    if curl "${HDR[@]}" "https://api.github.com/repos/erlang/otp/actions/artifacts?name=otp_doc_html" | jq '[.artifacts[] | select(.workflow_run.head_branch == "master")][0] | .archive_download_url' | xargs curl "${HDR[@]}" > "${ARCHIVE}.zip"; then
        unzip "${ARCHIVE}.zip"
        mv otp_doc_html.tar.gz "${ARCHIVE}"
        rm -f "${ARCHIVE}.zip"
    fi
fi

if [ ! "${RINCLUDE[0]}" = "" ]; then
    set -x
    timeout "${TIME_LIMIT}" rsync --archive --verbose --compress "${RINCLUDE[@]}" --exclude='*' \
      erlang.org::erlang-download docs/ || true
    set +x
fi

for ARCHIVE in docs/*.tar.gz; do
    [ -f "${ARCHIVE}" ] || continue
    mkdir "docs/tmp"
    tar xzf "${ARCHIVE}" -C "docs/tmp"
    ERTS_VSN=$(echo docs/tmp/erts-* | sed 's/.*erts-\(.*\)/\1/')
    # shellcheck disable=SC2001
    VSN=$(echo "${ARCHIVE}" | sed 's/.*otp_doc_html_\(.*\)\.tar.gz/\1/')
    MAJOR_VSN=$(echo "${VSN}" | awk -F. '{ print $1 }')
    echo "Flattening ${MAJOR_VSN}"
    mv "docs/tmp" "docs/doc-${ERTS_VSN}"
    if [ "${MAJOR_VSN}" = "${LATEST_MAJOR_VSN}" ]; then
        _flatten_docs "${ERTS_VSN}" "${MAJOR_VSN}" "${LATEST_MAJOR_VSN}"
        rm -rf "docs/doc" || true
        mv docs/doc-1 docs/doc
        printf -- '---\nlayout: search\nversion: %s\n---\n' "${MAJOR_VSN}" > "docs/doc/search.html"
    fi
    _flatten_docs "${ERTS_VSN}" "${MAJOR_VSN}" "${LATEST_MAJOR_VSN}"
    touch "docs/doc-1/$(_get_doc_hash "${VSN}")"
    rm -rf "docs/${MAJOR_VSN}" || true
    mv docs/doc-1 "docs/${MAJOR_VSN}"
    rm -rf "docs/doc-${ERTS_VSN}"
    rm -f "${ARCHIVE}"
done

URL=$(grep "^url: " _config.yml | sed 's@url: "\([^"]*\)".*@\1@')
BASEURL=$(grep "^baseurl: " _config.yml | sed 's@baseurl: "\([^"]*\)".*@\1@')
MAJOR_VSNs=$(echo "${MAJOR_VSNs}" | tr '\n' ' ')
_scripts/otp_doc_sitemap.sh "${MAJOR_VSNs}" "${LATEST_MAJOR_VSN}" "${URL}${BASEURL}" > docs/sitemap_algolia.xml
