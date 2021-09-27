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
    grep "${1}" "${OTP_VERSIONS_TABLE}" | awk '{print $1}' | sed 's/OTP-\(.*\)/\1/g'
}
_get_latest_vsn() {
    _get_vsns "${1}" | head -1
}

MAJOR_VSNs=$(_get_vsns "OTP-[0-9]\+\.0 " | sed 's/^\([0-9]\+\).*/\1/g')
LATEST_MAJOR_VSN=$(echo "$MAJOR_VSNs" | tr ' ' '\n' | sort -n | tail -1)

RINCLUDE=()

for VSN in ${MAJOR_VSNs}; do
    LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}")
    ARCHIVE="docs/otp_doc_html_${LATEST_VSN}.tar.gz"

    if { [ ! -f "${ARCHIVE}" ] && [ ! -f "docs/${VSN}/${LATEST_VSN}" ]; } ||
       { [ "$DEPLOY" = "true" ] && [ "${VSN}" = "${LATEST_MAJOR_VSN}" ]; }; then
        if [ "${VSN}" = "${LATEST_MAJOR_VSN}" ]; then
            echo "Checking for ${LATEST_VSN} on garazdawi github"
            curl --silent --location --fail --show-error "${HDR[@]}" "https://github.com/garazdawi/otp/releases/download/OTP-24.0.6-doc/otp_doc_html_24.0.6.tar.gz" > "${ARCHIVE}";
        elif [ "${DEPLOY}" = "true" ]; then
            echo "Checking for ${LATEST_VSN} on github"
            if ! curl --silent --location --fail --show-error "${HDR[@]}" "https://github.com/erlang/otp/releases/download/OTP-${LATEST_VSN}/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
                rm -f "${ARCHIVE}"
                LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}\.[0-9] ")
                if [ ! -f "docs/${VSN}/${LATEST_VSN}" ]; then
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
    VSN=$(echo "${ARCHIVE}" | sed 's/.*otp_doc_html_\(.\+\)\.tar.gz/\1/')
    MAJOR_VSN=$(echo "${VSN}" | awk -F. '{ print $1}')
    mv "docs/tmp" "docs/doc-${ERTS_VSN}"
    if [ "${MAJOR_VSN}" = "${LATEST_MAJOR_VSN}" ]; then
        (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}" true)
        rm -rf "doc" || true
        mv docs/doc-1 doc
        URL=$(grep "^url: " _config.yml | sed 's@url: "\([^"]*\)".*@\1@')
        BASEURL=$(grep "^baseurl: " _config.yml | sed 's@baseurl: "\([^"]*\)".*@\1@')
        _scripts/otp_doc_sitemap.sh doc "${URL}${BASEURL}/doc/" > doc/sitemap_algolia.xml
    fi
    (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}" false)
    touch "docs/doc-1/${VSN}"
    rm -rf "docs/${MAJOR_VSN}" || true
    mv docs/doc-1 "docs/${MAJOR_VSN}"
    rm -rf "docs/doc-${ERTS_VSN}"
    rm -f "${ARCHIVE}"
done

# If we are not deploying, we copy the latest docs into the other docs
# in order to speed up the build.
if [ "${DEPLOY}" != "true" ]; then
    for VSN in ${MAJOR_VSNs}; do
        if [ ! -d "docs/${VSN}" ]; then
            cp -r "docs/${LATEST_MAJOR_VSN}" "docs/${VSN}"
        fi
    done
fi
