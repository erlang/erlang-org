#!/bin/sh

LATEST=$(head -1 otp_versions.table | awk '{print $1}' | sed 's/^OTP-//')
REPO="erlang"
BRANCH="maint"
REPO="https://api.github.com/repos/${REPO}/otp"
RUN_ID=$(curl "${REPO}/actions/runs?branch=${BRANCH}&event=push&per_page=1&status=success" | jq ".workflow_runs[0].id")
ARTIFACT=$(curl "${REPO}/actions/runs/${RUN_ID}/artifacts" | jq -r '.artifacts[] | select(.name == "otp_doc_html") | .archive_download_url')
if curl --silent --location --fail --show-error -H "Authorization: token ${GITHUB_TOKEN}" "${ARTIFACT}" > /tmp/doc.zip; then
    unzip /tmp/doc.zip && rm -f /tmp/doc.zip
else
    echo "WARNING: DOWNLOADING STALE DOCS AS GITHUB_TOKEN NOT SET"
    curl --silent --location --fail --show-error "https://github.com/garazdawi/otp/releases/download/OTP-24.0.6-doc/otp_doc_html_24.0.6.tar.gz" > otp_doc_html.tar.gz
fi
mv otp_doc_html.tar.gz "docs/otp_doc_html_${LATEST}.tar.gz"