#!/bin/sh

LATEST=$(head -1 otp_versions.table | awk '{print $1}' | sed 's/^OTP-//')
REPO="garazdawi"
BRANCH="lukas/erl_docgen/make-docs-more-responsive"
REPO="https://api.github.com/repos/${REPO}/otp"
RUN_ID=$(curl "${REPO}/actions/runs?branch=${BRANCH}&event=push&per_page=1&status=success" | jq ".workflow_runs[0].id")
ARTIFACT=$(curl ${REPO}/actions/runs/${RUN_ID}/artifacts | jq -r '.artifacts[] | select(.name == "otp_doc_html") | .archive_download_url')
curl -v --location -H "Authorization: token ${GITHUB_TOKEN}" "${ARTIFACT}" > /tmp/doc.zip
unzip /tmp/doc.zip && rm -f /tmp/doc.zip
mv otp_doc_html.tar.gz docs/otp_doc_html_${LATEST}.tar.gz