%% Tests for the version-tree generator.
%%
%% The page can only be as right as what it is handed, and the shapes below are
%% the ones that have caught it out: a CVE record that bounds the versions it
%% knows nothing about, a CWE description that repeats its own id, an openvex
%% purl with a doubled slash. Fixtures are trimmed from real records.
%%
%%   cd _scripts && rebar3 eunit
%%
%% The ordering rules the page draws the tree with are tested on the other side,
%% in assets/otp-version-scheme.test.ts, where they run.

-module('create-versions_tests').

-include_lib("eunit/include/eunit.hrl").

-import('create-versions',
        [parse_line/1, vsn_le/2, release_ranges/1, applications/1, cwe/1, cvss/1,
         is_version/1, statement/2, component/1, otp_purl/1]).


parse_line_test() ->
    ?assertEqual(#{ vsn => <<"27.3.4.3">>,
                    changed => [<<"erts-15.2.7.1">>, <<"ssl-11.2.11">>],
                    same => [<<"asn1-5.3.2">>, <<"stdlib-6.2.2">>] },
                 parse_line(<<"OTP-27.3.4.3 : erts-15.2.7.1 ssl-11.2.11 "
                              "# asn1-5.3.2 stdlib-6.2.2 :">>)).

%% Sorting the table as text puts 27.3.4.11 before 27.3.4.9, which puts the
%% wrong release at the tip of the branch.
version_order_is_numeric_test() ->
    ?assert(vsn_le(<<"27.3.4.9">>, <<"27.3.4.11">>)),
    ?assertNot(vsn_le(<<"27.3.4.11">>, <<"27.3.4.9">>)).

%%--------------------------------------------------------------------
%% CVE records
%%--------------------------------------------------------------------

otp_affected(Versions) ->
    [#{ <<"packageName">> => <<"otp">>, <<"versions">> => Versions }].

affected_range(From, LessThan) ->
    #{ <<"version">> => From, <<"lessThan">> => LessThan,
       <<"versionType">> => <<"otp">>, <<"status">> => <<"affected">> }.

%% One bounded range per maintenance line.
release_ranges_bounded_test() ->
    ?assertEqual([#{ from => <<"26.0">>, until => <<"26.2.5.11">> },
                  #{ from => <<"27.0">>, until => <<"27.3.3">> }],
                 release_ranges(otp_affected([affected_range(<<"26.0">>, <<"26.2.5.11">>),
                                              affected_range(<<"27.0">>, <<"27.3.3">>)]))).

%% Or one open range from where the flaw was introduced, with a `changes' entry
%% per line saying where it was fixed.
release_ranges_open_test() ->
    ?assertEqual(
       [#{ from => <<"17.0">>, fixedAt => [<<"26.2.5.11">>, <<"27.3.3">>] }],
       release_ranges(
         otp_affected(
           [(affected_range(<<"17.0">>, <<"*">>))#{
              <<"changes">> =>
                  [#{ <<"at">> => <<"26.2.5.11">>, <<"status">> => <<"unaffected">> },
                   #{ <<"at">> => <<"27.3.3">>, <<"status">> => <<"unaffected">> }] }]))).

%% A record may bound the versions below the first release it knows about as
%% "unknown". Reading that as affected marked every release vulnerable.
release_ranges_ignores_unknown_test() ->
    ?assertEqual([], release_ranges(otp_affected(
                                      [(affected_range(<<"0">>, <<"17.0">>))#{
                                         <<"status">> => <<"unknown">> }]))).

%% The default status is unaffected, so an entry that does not say is not a
%% claim that those versions are affected either.
release_ranges_ignores_statusless_test() ->
    ?assertEqual([], release_ranges(otp_affected(
                                      [maps:remove(<<"status">>,
                                                   affected_range(<<"26.0">>, <<"26.2">>))]))).

%% Releases are numbered by `otp'; the same block also carries semver ranges for
%% the applications, which are not release numbers.
release_ranges_ignores_other_version_types_test() ->
    ?assertEqual([], release_ranges(otp_affected(
                                      [(affected_range(<<"11.0">>, <<"11.2.12">>))#{
                                         <<"versionType">> => <<"semver">> }]))).

%% Applications are the entries the release ones are not: named individually,
%% and never a vendored component, which carries its upstream repository name.
applications_test() ->
    ?assertEqual(
       #{ <<"ssl">> => [#{ from => <<"11.0">>, until => <<"11.2.12">> }] },
       applications([#{ <<"packageName">> => <<"ssl">>,
                        <<"versions">> => [affected_range(<<"11.0">>, <<"11.2.12">>),
                                           (affected_range(<<"10.0">>, <<"10.9">>))#{
                                             <<"status">> => <<"unaffected">> }] },
                     #{ <<"packageName">> => <<"otp">>,
                        <<"versions">> => [affected_range(<<"27.0">>, <<"27.3.3">>)] },
                     #{ <<"packageName">> => <<"madler/zlib">>,
                        <<"versions">> => [affected_range(<<"1.2">>, <<"1.3.1">>)] }])).

%% The description repeats the id, and the page prints them next to each other.
cwe_test() ->
    ?assertEqual(#{ id => <<"CWE-757">>,
                    description => <<"Selection of Less-Secure Algorithm During "
                                     "Negotiation ('Algorithm Downgrade')">> },
                 cwe([#{ <<"descriptions">> =>
                             [#{ <<"cweId">> => <<"CWE-757">>,
                                 <<"description">> =>
                                     <<"CWE-757 Selection of Less-Secure Algorithm "
                                       "During Negotiation ('Algorithm Downgrade')">> }] }])),
    ?assertEqual(null, cwe([#{ <<"descriptions">> => [#{ <<"description">> => <<"n/a">> }] }])).

%% Containers appear in any order and only some carry a score.
cvss_test() ->
    ?assertEqual(#{ score => 9.8, severity => <<"critical">>,
                    vector => <<"CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H">> },
                 cvss([#{ <<"metrics">> => [] },
                       #{ <<"metrics">> =>
                              [#{ <<"cvssV3_1">> =>
                                      #{ <<"baseScore">> => 9.8,
                                         <<"baseSeverity">> => <<"CRITICAL">>,
                                         <<"vectorString">> =>
                                             <<"CVSS:3.1/AV:N/AC:L/PR:N/UI:N/S:U/C:H/I:H/A:H">> } }] }])),
    ?assertEqual(null, cvss([])).

is_version_test() ->
    ?assert(is_version(<<"27.3.4.11">>)),
    ?assertNot(is_version(<<"*">>)),
    ?assertNot(is_version(<<"27.0-rc1">>)),
    ?assertNot(is_version(undefined)).

%%--------------------------------------------------------------------
%% openvex statements
%%--------------------------------------------------------------------

%% An advisory against an application: the fix is a version of the same
%% application, which the page compares against what the release carries.
statement_advisory_test() ->
    ?assertEqual({advisory, #{ major => <<"27">>, cve => <<"CVE-2025-32433">>,
                               app => <<"ssh">>, introduced => <<"5.2.9">>,
                               fixed => <<"5.2.10">> }},
                 statement(<<"27">>,
                           #{ <<"pkg:otp/ssh@5.2.9">> => <<"CVE-2025-32433">>,
                              <<"status">> =>
                                  #{ <<"fixed">> => [<<"pkg:otp/ssh@5.2.10">>] } })).

%% A vendored component Erlang/OTP is not affected by, with the reason and the
%% applications that bundle it.
statement_dismissal_test() ->
    ?assertEqual({not_affected, #{ major => <<"27">>, id => <<"CVE-2023-45853">>,
                                   component => <<"madler/zlib">>, ref => <<"04f42ce">>,
                                   apps => [<<"erts">>],
                                   justification => <<"vulnerable_code_not_present">> }},
                 statement(<<"27">>,
                           #{ <<"pkg:github//madler/zlib@04f42ce">> => <<"CVE-2023-45853">>,
                              <<"status">> =>
                                  #{ <<"not_affected">> => <<"vulnerable_code_not_present">>,
                                     <<"apps">> => [<<"pkg:otp/erts@15.2">>] } })).

statement_under_investigation_test() ->
    ?assertEqual({not_affected, #{ major => <<"27">>, id => <<"CVE-2024-0001">>,
                                   component => <<"openssl/openssl">>, ref => <<"3.0.0">>,
                                   apps => [], justification => <<"under_investigation">> }},
                 statement(<<"27">>,
                           #{ <<"pkg:github/openssl/openssl@3.0.0">> => <<"CVE-2024-0001">>,
                              <<"status">> => <<"under_investigation">> })).

%% A vendored component that Erlang/OTP *is* affected by. Every statement about
%% one has been a dismissal so far, so this test is the only thing exercising
%% the path. `fixed' is a version of the component, which is what the page
%% prints; giving it the component's name instead reads as "fixed in
%% madler/zlib of that component".
statement_bundled_affected_test() ->
    ?assertEqual({bundled_affected, #{ major => <<"28">>, cve => <<"CVE-2026-0001">>,
                                       component => <<"madler/zlib">>, ref => <<"1.3.0">>,
                                       apps => [<<"erts">>], fixed => <<"1.3.1">> }},
                 statement(<<"28">>,
                           #{ <<"pkg:github/madler/zlib@1.3.0">> => <<"CVE-2026-0001">>,
                              <<"status">> =>
                                  #{ <<"fixed">> => [<<"pkg:github/madler/zlib@1.3.1">>],
                                     <<"apps">> => [<<"pkg:otp/erts@16.0">>] } })).

%% A fix naming a different application says nothing about this one.
statement_mismatched_fix_test() ->
    ?assertEqual(skip,
                 statement(<<"27">>,
                           #{ <<"pkg:otp/ssh@5.2.9">> => <<"CVE-2025-32433">>,
                              <<"status">> =>
                                  #{ <<"fixed">> => [<<"pkg:otp/ssl@11.2.12">>] } })).

%% Neither a fix nor a justification is nothing to say.
statement_silent_test() ->
    ?assertEqual(skip,
                 statement(<<"27">>, #{ <<"pkg:otp/ssh@5.2.9">> => <<"CVE-2025-32433">>,
                                        <<"status">> => #{} })).

component_test() ->
    ?assertEqual({<<"madler/zlib">>, <<"04f42ce">>},
                 component(<<"pkg:github/madler/zlib@04f42ce">>)),
    %% A couple of the entries carry a doubled slash after the host.
    ?assertEqual({<<"madler/zlib">>, <<"04f42ce">>},
                 component(<<"pkg:github//madler/zlib@04f42ce">>)),
    ?assertEqual({<<"wxWidgets/wxWidgets">>, null},
                 component(<<"pkg:github/wxWidgets/wxWidgets">>)).

otp_purl_test() ->
    ?assertEqual({ok, <<"ssh">>, <<"5.2.9">>}, otp_purl(<<"pkg:otp/ssh@5.2.9">>)),
    ?assertEqual(error, otp_purl(<<"pkg:github/madler/zlib@04f42ce">>)),
    ?assertEqual(error, otp_purl(<<"pkg:otp/ssh">>)).
