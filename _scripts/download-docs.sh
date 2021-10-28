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

# The files that are involved when generating docs
SCRIPT_FILES="_scripts/otp_flatten_docs _scripts/otp_doc_sitemap.sh assets/doc-search.tsx"

_get_vsns() {
    grep "${1}" "${OTP_VERSIONS_TABLE}" | awk '{print $1}' | sed 's/OTP-\(.*\)/\1/g'
}
_get_latest_vsn() {
    _get_vsns "${1}" | head -1
}
_get_doc_hash() {
    # shellcheck disable=SC2086
    (echo "${1}" && cat ${SCRIPT_FILES}) | sha256sum | awk '{print $1}'
}

MAJOR_VSNs=$(_get_vsns "OTP-[0-9]\+\.0 " | sed 's/^\([0-9]\+\).*/\1/g')
LATEST_MAJOR_VSN=$(echo "$MAJOR_VSNs" | tr ' ' '\n' | sort -n | tail -1)
RINCLUDE=()

for VSN in ${MAJOR_VSNs}; do
    LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}")
    ARCHIVE="docs/otp_doc_html_${LATEST_VSN}.tar.gz"

    if [ ! -f "${ARCHIVE}" ] && [ ! -f "docs/${VSN}/$(_get_doc_hash "${LATEST_VSN}")" ]; then
        if [ "${VSN}" = "${LATEST_MAJOR_VSN}" ]; then
            echo "Checking for ${LATEST_VSN} on garazdawi github"
            _scripts/download-latest-doc.sh;
        else
            echo "Checking for ${LATEST_VSN} on github"
            if ! curl --silent --location --fail --show-error "${HDR[@]}" "https://github.com/erlang/otp/releases/download/OTP-${LATEST_VSN}/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
                rm -f "${ARCHIVE}"
                LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}\.[0-9] ")
                if [ ! -f "docs/${VSN}/$(_get_doc_hash "${LATEST_VSN}")" ]; then
                    echo "Checking for ${LATEST_VSN} on erlang.org::erlang-download"
                    RINCLUDE=("--include=otp_doc_html_${LATEST_VSN}.tar.gz" "${RINCLUDE[@]}")
                else
                    echo "${LATEST_VSN} already exists"
                fi
            fi
        fi
    else
        echo "${LATEST_VSN} already exists"
    fi
done


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
    mv "docs/tmp" "docs/doc-${ERTS_VSN}"
    if [ "${MAJOR_VSN}" = "${LATEST_MAJOR_VSN}" ]; then
        (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}" true)
        rm -rf "docs/doc" || true
        mv docs/doc-1 docs/doc
        URL=$(grep "^url: " _config.yml | sed 's@url: "\([^"]*\)".*@\1@')
        BASEURL=$(grep "^baseurl: " _config.yml | sed 's@baseurl: "\([^"]*\)".*@\1@')
        _scripts/otp_doc_sitemap.sh docs/doc "${URL}${BASEURL}/doc/" > docs/doc/sitemap_algolia.xml
    fi
    (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}" false)
    touch "docs/doc-1/$(_get_doc_hash "${VSN}")"
    rm -rf "docs/${MAJOR_VSN}" || true
    mv docs/doc-1 "docs/${MAJOR_VSN}"
    rm -rf "docs/doc-${ERTS_VSN}"
    rm -f "${ARCHIVE}"
done