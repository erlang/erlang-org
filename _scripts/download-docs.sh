#!/bin/bash

## Download the docs from github if they are there, or else from erlang.org
## Then we flatten all the docs and put them in the docs/ folder
##
## We use rsync to sync from erlang.org in order to not be rate-limited

set -e

OTP_VERSIONS_TABLE=$1
TIME_LIMIT=${3:-120m}
TOKEN=${2:-"token ${GITHUB_TOKEN}"}
HDR=(-H "Authorization: ${TOKEN}")

_get_vsns() {
    grep "${1}" ${OTP_VERSIONS_TABLE} | awk '{print $1}' | sed 's/OTP-\(.*\)/\1/g'
}
_get_latest_vsn() {
    _get_vsns "${1}" | head -1
}

MAJOR_VSNs=$(_get_vsns "OTP-[0-9]\+\.0 " | sed 's/^\([0-9]\+\).*/\1/g')

RINCLUDE=()

for VSN in ${MAJOR_VSNs}; do
    LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}")
    ARCHIVE="docs/otp_doc_html_${LATEST_VSN}.tar.gz"
    if [ ! -f "${ARCHIVE}" ]; then
        echo "Checking for ${LATEST_VSN} on github"
        if ! curl --silent --location --fail --show-error "${HDR[@]}" "https://github.com/erlang/otp/releases/download/OTP-${LATEST_VSN}/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
            rm -f "${ARCHIVE}"
            LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}\.[0-9] ")
            echo "Checking for ${LATEST_VSN} on erlang.org::erlang-download"
            RINCLUDE=("--include=otp_doc_html_${LATEST_VSN}.tar.gz" "${RINCLUDE[@]}")
        fi
    else
        echo "${LATEST_VSN} already exists"
    fi
done

set -x

! timeout ${TIME_LIMIT} rsync --archive --verbose --compress "${RINCLUDE[@]}" --exclude='*' \
  erlang.org::erlang-download docs/

set +x

for ARCHIVE in docs/*.tar.gz; do
    mkdir "docs/tmp"
    tar xzf "${ARCHIVE}" -C "docs/tmp"
    ERTS_VSN=$(echo docs/tmp/erts-* | sed 's/.*erts-\(.*\)/\1/')
    MAJOR_VSN=$(echo "${ARCHIVE}" | sed 's/.*otp_doc_html_\([^.]\+\).*/\1/')
    mv "docs/tmp" "docs/doc-${ERTS_VSN}"
    (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}")
    mv docs/doc-1 "docs/${MAJOR_VSN}"
    rm -rf "docs/doc-${ERTS_VSN}"
done

CURRENT_VSN=$(echo "${MAJOR_VSNs}" | head -1)

if [ ! -f doc ]; then
    ln -s "docs/${CURRENT_VSN}" doc
fi

rm -f docs/otp_doc_html_*.tar.gz
