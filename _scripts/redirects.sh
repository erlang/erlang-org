#!/bin/bash

## This script creates netlify redirects for the pre 27 system docs
## so that any link on the internet point to them, they still end up
## in the correct place.
## It also creates redirects for all modules from /doc/man/MODULE to the correct app.

set -e

VERIFY_SOURCE=false
SYSTEM_GUIDES=$(find docs/26/ -name users_guide.html | grep -v "apps" | awk -F/ '{print $3}' | grep -v general_info | grep -v system_architecture_intro)

_check() {
    LOWERCASE_TARGET=$(echo "$2" | tr '[:upper:]' '[:lower:]')
    if [ -f "doc/$2" ] || [ -f "doc/${LOWERCASE_TARGET}" ]; then
        if ! $VERIFY_SOURCE || curl -Is -L "https://www.erlang.org/doc/$1" | grep "^HTTP/2 200" > /dev/null; then
            return 0
        else
            echo "Failed to fetch https://www.erlang.org/doc/$1" >&2
            return 1
        fi
    else
        echo "doc/$1 -> doc/$2 does not exist" >&2
        return 1
    fi
    }

_redirect() {
    local FROM="$1"
    local TO="$2"
    if _check "$FROM" "$TO"; then
        TO="${TO/%.html/}"
        echo "/doc/$FROM  /doc/$TO" | tr '[:upper:]' '[:lower:]'
        FROM="${FROM/%.html/}"
        echo "/doc/$FROM  /doc/$TO" | tr '[:upper:]' '[:lower:]'
    else
        exit 1;
    fi

}

## Check if primary docs are ExDoc docs
if [ ! -f doc/readme.html ]; then
    exit 0
fi

echo "## Pre 27 system docs redirects"
for guide in ${SYSTEM_GUIDES}; do
    _redirect "$guide/users_guide.html" "system/$guide.html"
    CHAPTERS=$(find "docs/26/${guide}/" -maxdepth 1 -mindepth 1 -name "*.html" | awk -F/ '{print $4}' | grep -v users_guide.html)
    for chapter in ${CHAPTERS}; do
        if grep 'id="chapters"' "docs/26/$guide/$chapter" > /dev/null; then
            case "$guide-$chapter" in
                *intro*.html) new_chapter="$guide.html";;
                *-"des_princ.html") new_chapter="design_principles.html";;
                eff*-"functions.html") new_chapter="eff_guide_functions.html";;
                eff*-"advanced.html") new_chapter="memory.html";;
                ref*-"functions.html") new_chapter="ref_man_functions.html";;
                eff*-"processes.html") new_chapter="eff_guide_processes.html";;
                ref*-"processes.html") new_chapter="ref_man_processes.html";;
                *-"record_macros.html") new_chapter="records_macros.html";;
                prog*-"records.html") new_chapter="prog_ex_records.html";;
                ref*-"records.html") new_chapter="ref_man_records.html";;
                *"install-binary.html") continue;;
                *"retired-myths.html") continue;;
                *"myths.html") continue;;
                *"solaris.html") continue;;
                *"embedded_nt.html") continue;;
                *) new_chapter=$chapter;;
            esac
            if [ "$chapter" = "$new_chapter" ]; then
                _check "$guide/$chapter" "system/$new_chapter"
            else
                _redirect "$guide/$chapter" "system/$new_chapter"
            fi
        fi
    done
    echo "/doc/$guide/*" "/doc/system/:splat 301!"
done

echo
echo "## jinterface java docs"
echo "/doc/apps/jinterface/java/* //doc/apps/jinterface/assets/java/:splat"

echo
echo "## man page redirects"
APPS=$(find doc/apps/ -maxdepth 1 -mindepth 1 -type d | awk -F/ '{print $3}')

## Never verify source for man page redirects
VERIFY_SOURCE=false

for app in ${APPS}; do
    echo
    echo "### Redirects for ${app}"
    FILES=$(ls -1 "doc/apps/$app/"*.html)
    for file in ${FILES}; do
        if grep 'id="moduledoc"' "$file" > /dev/null; then
            MODULE=$(echo "$file" | awk -F/ '{print $4}')
            _redirect "man/${MODULE}" "apps/${app}/${MODULE}"
        fi
    done
done

_redirect "man/assert.hrl.html" "apps/stdlib/assert_hrl.html"
_redirect "man/erl.html" "apps/erts/erl_cmd.html"
_redirect "man/werl.html" "apps/erts/werl_cmd.html"
_redirect "man/erlc.html" "apps/erts/erlc_cmd.html"
_redirect "man/app.html" "apps/kernel/app.html"
_redirect "man/appup.html" "apps/sasl/appup.html"
_redirect "man/rel.html" "apps/sasl/rel.html"
_redirect "man/relup.html" "apps/sasl/relup.html"
_redirect "man/driver_entry.html" "apps/erts/driver_entry.html"
_redirect "man/erl_driver.html" "apps/erts/erl_driver.html"
_redirect "man/erl_nif.html" "apps/erts/erl_nif.html"
_redirect "man/erts_alloc.html" "apps/erts/erts_alloc.html"
