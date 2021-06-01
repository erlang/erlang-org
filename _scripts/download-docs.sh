#!/bin/bash

_get_vsns() {
    grep "${1}" otp_versions.table | awk '{print $1}' | sed 's/OTP-\(.*\)/\1/g'
}
_get_latest_vsn() {
    _get_vsns "${1}" | head -1
}

MAJOR_VSNs=$(_get_vsns "OTP-[0-9]\+\.0 " | sed 's/^\([0-9]\+\).*/\1/g')

for VSN in ${MAJOR_VSNs}; do
    LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}")
    ARCHIVE="docs/otp_doc_html_${VSN}.tar.gz"
    echo ${LATEST_VSN}
    if ! curl --silent --location --fail --show-error "https://github.com/erlang/otp/releases/download/OTP-${LATEST_VSN}/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
        LATEST_VSN=$(_get_latest_vsn "^OTP-${VSN}\.[0-9] ")
        if ! curl --silent --location --fail --show-error "https://erlang.org/download/otp_doc_html_${LATEST_VSN}.tar.gz" > "${ARCHIVE}"; then
            echo "Failed to fetch both ${LATEST_VSN} and ${LATEST_PACKAGE}";
        fi
    fi
    mkdir "docs/doc-${LATEST_VSN}"
    tar xzf "${ARCHIVE}" -C "docs/doc-${LATEST_VSN}"
    ERTS_VSN=$(echo docs/"doc-${LATEST_VSN}"/erts-* | sed 's/.*erts-\(.*\)/\1/')
    mv "docs/doc-${LATEST_VSN}" "docs/doc-${ERTS_VSN}"
    (cd docs && ../_scripts/otp_flatten_docs "doc-${ERTS_VSN}")
    mv docs/doc-1 "docs/${VSN}"
    rm -rf "docs/doc-${ERTS_VSN}"
done

CURRENT_VSN=$(echo "${MAJOR_VSNs}" | head -1)

ln -s "docs/${CURRENT_VSN}" doc
