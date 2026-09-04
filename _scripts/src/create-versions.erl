%% Builds the data behind the Erlang/OTP version tree page.
%%
%% Everything the page shows is derived from three sources:
%%
%%   * otp_versions.table   - which application versions each Erlang/OTP
%%                            version contains, and which of them changed.
%%   * the OTP-* git tags   - the release date. Tags go back to 2014, while
%%                            GitHub releases only start at Erlang/OTP 21 and
%%                            the older ones were backfilled with the date of
%%                            the backfill rather than of the release.
%%   * the CVE records      - which releases each advisory affects, said in
%%                            release terms rather than application ones, plus
%%                            the CVSS score, the CWE and any workaround. Only
%%                            the ones the Erlang Ecosystem Foundation assigned
%%                            carry release data, so openvex remains the source
%%                            for the rest.
%%   * the repository       - the list of advisories, with their GHSA ids,
%%     security advisories    severities and summaries. This is the index: it is
%%                            what the bot maintaining openvex watches, so
%%                            openvex trails it by however long that pull
%%                            request sits unmerged. Indexing on openvex instead
%%                            leaves the newest advisories invisible.
%%   * the openvex branch   - two kinds of statement. For an OTP application,
%%                            the affected and fixed versions per CVE; it names
%%                            CVEs only, so GHSA ids, severities and summaries
%%                            are joined in from the repository security
%%                            advisories. For a vendored component such as
%%                            zlib or OpenSSL, an assessment that Erlang/OTP is
%%                            not affected, with the reason why. Those carry no
%%                            fix version because there is nothing to fix, and
%%                            exist to answer the scanners that flag the
%%                            bundled copy.
%%
%% The tree structure and the ordering between versions are *not* emitted:
%% a version number alone determines both, so the page derives them.
-module('create-versions').
-include_lib("kernel/include/logger.hrl").
-export([main/1]).

main([OTPVersionTable, OutFile]) ->
    [?LOG_WARNING("Running without github authentication, consider setting "
                  "GITHUB_TOKEN in order for the API to not throttle you.")
     || os:getenv("GITHUB_TOKEN") =:= false],
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),

    Versions = parse_otp_versions_table(OTPVersionTable),
    Dates = tag_dates(),
    Ghsa = ghsa_by_cve(),
    {VexMajors, Advisories, NotAffected, BundledAffected} = advisories(),
    Cves = cve_records(Ghsa),

    {Rows, StringIx} = lists:mapfoldl(
                         fun(V, Acc) -> row(V, Dates, Acc) end, #{}, Versions),

    Json = json:encode(
             #{ strs => [S || {S, _} <- lists:keysort(2, maps:to_list(StringIx))],
                versions => Rows,
                advisories => Advisories,
                notAffected => NotAffected,
                bundledAffected => BundledAffected,
                cves => Cves,
                vexMajors => VexMajors }),
    ok = filelib:ensure_dir(OutFile),
    ok = file:write_file(OutFile, Json),
    ?LOG_INFO("Wrote ~ts: ~p versions, ~p advisories, ~p not-affected "
              "assessments, ~p application versions, ~p advisories "
              "(~p with release data)",
              [OutFile, length(Rows), length(Advisories), length(NotAffected),
               maps:size(StringIx), maps:size(Cves),
               length([C || C <- maps:values(Cves), maps:get(releases, C, []) =/= []])]),
    ok.

%%====================================================================
%% otp_versions.table
%%====================================================================

parse_otp_versions_table(File) ->
    {ok, Bin} = file:read_file(File),
    Lines = [L || L <- string:split(Bin, "\n", all), L =/= <<>>],
    Versions = [parse_line(L) || L <- Lines],
    lists:sort(fun(#{ vsn := A }, #{ vsn := B }) -> vsn_le(A, B) end, Versions).

parse_line(Line) ->
    {match, [Vsn, Changed, Same]} =
        re:run(Line, "^OTP-(\\S+)\\s*:([^#]*)#([^:]*):\\s*$",
               [{capture, all_but_first, binary}]),
    #{ vsn => Vsn, changed => tokens(Changed), same => tokens(Same) }.

tokens(Bin) ->
    [T || T <- string:split(string:trim(Bin), " ", all), T =/= <<>>].

%% Sorts on the numeric components so 27.3.4.9 comes before 27.3.4.11.
vsn_le(A, B) -> components(A) =< components(B).

components(Vsn) ->
    [binary_to_integer(P) || P <- string:split(Vsn, ".", all)].

row(#{ vsn := Vsn, changed := Changed, same := Same }, Dates, Acc0) ->
    {ChangedIx, Acc1} = lists:mapfoldl(fun intern/2, Acc0, Changed),
    {SameIx, Acc2} = lists:mapfoldl(fun intern/2, Acc1, Same),
    Row = #{ v => Vsn,
             d => maps:get(Vsn, Dates, null),
             c => ChangedIx,
             s => SameIx },
    {Row, Acc2}.

%% Application versions repeat across every release that carries them
%% unchanged, so they are interned into one table and referenced by index.
intern(Str, Ix) ->
    case Ix of
        #{ Str := N } -> {N, Ix};
        _ ->
            N = maps:size(Ix),
            {N, Ix#{ Str => N }}
    end.

%%====================================================================
%% CVE records, from the CVE Services API
%%====================================================================

%% openvex says which application version fixes an advisory, and only for the
%% majors still receiving updates. The CVE record says which *releases* are
%% affected, from the version that introduced the flaw, which usually reaches
%% much further back. Fetched for every advisory; the ones that carry no
%% release data still contribute a score, a CWE and a workaround.
cve_records(Ghsa) ->
    maps:map(
      fun(Cve, Meta) ->
              case cve_record(Cve) of
                  {ok, Record} ->
                      maps:merge(Meta, Record);
                  error ->
                      ?LOG_WARNING("No CVE record for ~ts", [Cve]),
                      maps:merge(Meta, #{ releases => [], applications => #{},
                                          cvss => null, cwe => null,
                                          workaround => null })
              end
      end, Ghsa).

cve_record(Cve) ->
    Url = "https://cveawg.mitre.org/api/cve/" ++ binary_to_list(Cve),
    case httpc:request(get, {Url, [{"User-Agent", "erlang-httpc"}]},
                       [{ssl, httpc:ssl_verify_host_options(true)}],
                       [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
            #{ <<"containers">> := Containers } = json:decode(Body),
            Cna = maps:get(<<"cna">>, Containers, #{}),
            Adp = maps:get(<<"adp">>, Containers, []),
            {ok, #{ releases => release_ranges(maps:get(<<"affected">>, Cna, [])),
                    applications => applications(maps:get(<<"affected">>, Cna, [])),
                    cvss => cvss([Cna | Adp]),
                    cwe => cwe(maps:get(<<"problemTypes">>, Cna, [])),
                    workaround => first_value(maps:get(<<"workarounds">>, Cna, [])) }};
        _ ->
            error
    end.

%% The applications an advisory concerns and the versions of them it affects.
%% openvex says this too, but only for the majors it covers, and only once the
%% bot that maintains it has caught up.
applications(Affected) ->
    maps:from_list(
      [{Name, [range(V) || V <- Versions]}
       || A <- Affected,
          Name <- [maps:get(<<"packageName">>, A, undefined)],
          is_binary(Name),
          not lists:member(Name, [<<"otp">>, <<"erlang/otp">>]),
          %% Vendored components are named by their upstream repository.
          nomatch =:= binary:match(Name, <<"/">>),
          Versions <- [[V || V <- maps:get(<<"versions">>, A, []),
                             maps:get(<<"versionType">>, V, undefined) =:= <<"otp">>,
                             maps:get(<<"status">>, V, undefined) =:= <<"affected">>,
                             is_version(maps:get(<<"version">>, V, undefined))]],
          Versions =/= []]).

%% Two shapes appear, and both say the same thing. Either a bounded range per
%% maintenance line, or one open range from where the flaw was introduced with a
%% `changes' entry marking where each line was fixed.
release_ranges(Affected) ->
    [range(V)
     || A <- Affected,
        lists:member(maps:get(<<"packageName">>, A, undefined), [<<"otp">>, <<"erlang/otp">>]),
        V <- maps:get(<<"versions">>, A, []),
        maps:get(<<"versionType">>, V, undefined) =:= <<"otp">>,
        %% Entries carry a status of their own and the default is unaffected, so
        %% only the explicitly affected ranges count. A record may also bound
        %% the versions below the first release as "unknown", which is not a
        %% claim that they are affected.
        maps:get(<<"status">>, V, undefined) =:= <<"affected">>,
        is_version(maps:get(<<"version">>, V, undefined))].

range(V) ->
    From = maps:get(<<"version">>, V),
    case maps:get(<<"lessThan">>, V, undefined) of
        Until when is_binary(Until) ->
            case is_version(Until) of
                true -> #{ from => From, until => Until };
                false -> #{ from => From, fixedAt => fixed_at(V) }
            end;
        _ ->
            #{ from => From, fixedAt => fixed_at(V) }
    end.

fixed_at(V) ->
    [At || C <- maps:get(<<"changes">>, V, []),
           maps:get(<<"status">>, C, undefined) =:= <<"unaffected">>,
           At <- [maps:get(<<"at">>, C, undefined)],
           is_version(At)].

is_version(V) when is_binary(V) ->
    match =:= re:run(V, "^[0-9]+(\\.[0-9]+)*$", [{capture, none}]);
is_version(_) ->
    false.

cvss([]) ->
    null;
cvss([Container | Rest]) ->
    Scores = [M || Metric <- maps:get(<<"metrics">>, Container, []),
                   M <- maps:values(Metric),
                   is_map(M), is_map_key(<<"baseScore">>, M)],
    case Scores of
        [] ->
            cvss(Rest);
        [Score | _] ->
            #{ score => maps:get(<<"baseScore">>, Score),
               severity => string:lowercase(maps:get(<<"baseSeverity">>, Score, <<>>)),
               vector => maps:get(<<"vectorString">>, Score, null) }
    end.

cwe([#{ <<"descriptions">> := [#{ <<"cweId">> := Id, <<"description">> := Text } | _] } | _]) ->
    %% The description repeats the id, so it is dropped here rather than left
    %% for every consumer to notice and strip.
    #{ id => Id,
       description => iolist_to_binary(
                        re:replace(Text, "^CWE-[0-9]+\\s*", "", [{return, binary}])) };
cwe([_ | Rest]) ->
    cwe(Rest);
cwe([]) ->
    null.

first_value([#{ <<"value">> := Value } | _]) -> Value;
first_value(_) -> null.

%%====================================================================
%% Release dates, from the git tags
%%====================================================================

tag_dates() ->
    case tag_dates(null, #{}) of
        {ok, Dates} ->
            Dates;
        {error, Reason} ->
            ?LOG_WARNING("Could not read tag dates (~p), falling back to "
                         "GitHub release dates. Releases made before "
                         "Erlang/OTP 21 will have no date.", [Reason]),
            release_dates()
    end.

tag_dates(After, Acc) ->
    Query =
        "query($after: String) {"
        "  repository(owner: \"erlang\", name: \"otp\") {"
        "    refs(refPrefix: \"refs/tags/\", first: 100, after: $after,"
        "         orderBy: {field: ALPHABETICAL, direction: ASC}) {"
        "      pageInfo { hasNextPage endCursor }"
        "      nodes { name target { __typename"
        "        ... on Tag { tagger { date } }"
        "        ... on Commit { committedDate } } }"
        "    } } }",
    case gh:graphql(Query, #{ 'after' => After }) of
        {ok, #{ <<"repository">> := #{ <<"refs">> := Refs } }} ->
            #{ <<"nodes">> := Nodes,
               <<"pageInfo">> := #{ <<"hasNextPage">> := More,
                                    <<"endCursor">> := Cursor } } = Refs,
            Acc1 = lists:foldl(fun add_tag_date/2, Acc, Nodes),
            case More of
                true -> tag_dates(Cursor, Acc1);
                false -> {ok, Acc1}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

add_tag_date(#{ <<"name">> := Name, <<"target">> := Target }, Acc) ->
    case {re:run(Name, "^OTP-(.*)$", [{capture, all_but_first, binary}]),
          tag_date(Target)} of
        {{match, [Vsn]}, {ok, Date}} -> Acc#{ Vsn => Date };
        _ -> Acc
    end.

tag_date(#{ <<"tagger">> := #{ <<"date">> := D } }) -> {ok, day(D)};
tag_date(#{ <<"committedDate">> := D }) -> {ok, day(D)};
tag_date(_) -> error.

day(Timestamp) -> binary:part(Timestamp, 0, 10).

%% Only reached when the GraphQL API is unavailable.
release_dates() ->
    case gh:get("/repos/erlang/otp/releases") of
        {ok, Releases} ->
            maps:from_list(
              [{Vsn, day(Published)}
               || #{ <<"tag_name">> := Tag,
                     <<"published_at">> := Published } <- Releases,
                  Published =/= null,
                  {match, [Vsn]} <-
                      [re:run(Tag, "^OTP-(.*)$", [{capture, all_but_first, binary}])]]);
        {error, Reason} ->
            ?LOG_WARNING("Could not read release dates either (~p)", [Reason]),
            #{}
    end.

%%====================================================================
%% Security advisories
%%====================================================================

advisories() ->
    case gh:get("/repos/erlang/otp/contents/openvex.table?ref=openvex",
                [{"Accept", "application/vnd.github.raw"}]) of
        {ok, Raw} ->
            Vex = json:decode(Raw),
            Majors = [major_of(K) || K <- maps:keys(Vex)],
            Statements =
                lists:append(
                  [[S || Entry <- Entries,
                         S <- [statement(major_of(Major), Entry)],
                         S =/= skip]
                   || {Major, Entries} <- maps:to_list(Vex)]),
            {lists:sort(Majors),
             [A || {advisory, A} <- Statements],
             [N || {not_affected, N} <- Statements],
             [B || {bundled_affected, B} <- Statements]};
        {error, Reason} ->
            ?LOG_WARNING("Could not read openvex.table (~p), the page will "
                         "show no advisories.", [Reason]),
            {[], [], [], []}
    end.

major_of(<<"otp-", Major/binary>>) -> Major;
major_of(Other) -> Other.

%% An entry maps one purl to a vulnerability id and carries the status
%% alongside it. A `pkg:otp/` purl with a fix is an advisory against an
%% application; anything else is an assessment of a vendored component.
statement(Major, Entry) ->
    case [{K, V} || {K, V} <- maps:to_list(Entry), K =/= <<"status">>] of
        [{Purl, Id}] -> statement(Major, Purl, Id, maps:get(<<"status">>, Entry, #{}));
        _ -> skip
    end.

statement(Major, Purl, Id, Status) when is_map(Status) ->
    case {otp_purl(Purl), maps:get(<<"fixed">>, Status, [])} of
        {{ok, App, Introduced}, [FixedPurl | _]} ->
            case otp_purl(FixedPurl) of
                {ok, App, Fixed} ->
                    {advisory,
                     #{ major => Major,
                        cve => Id,
                        app => App,
                        introduced => Introduced,
                        fixed => Fixed }};
                _ ->
                    skip
            end;
        {_, []} ->
            not_affected(Major, Purl, Id, maps:get(<<"not_affected">>, Status, null), Status);
        {error, [FixedPurl | _]} ->
            %% A bundled component that Erlang/OTP *is* affected by. Every
            %% statement about one has so far been a dismissal, so this path is
            %% untravelled, but dropping it would hide a real advisory. The fix
            %% is a version of the component, not of an application, so it
            %% cannot be compared against anything a release carries: the
            %% release it applies to comes from the CVE record, and this records
            %% what the advisory is about.
            {Component, Ref} = component(Purl),
            {bundled_affected,
             #{ major => Major,
                cve => Id,
                component => Component,
                ref => Ref,
                apps => [App || A <- maps:get(<<"apps">>, Status, []),
                                {ok, App, _} <- [otp_purl(A)]],
                fixed => element(1, component(FixedPurl)) }};
        _ ->
            skip
    end;
statement(Major, Purl, Id, <<"under_investigation">>) ->
    not_affected(Major, Purl, Id, <<"under_investigation">>, #{});
statement(_, _, _, _) ->
    skip.

not_affected(_Major, _Purl, _Id, null, _Status) ->
    skip;
not_affected(Major, Purl, Id, Justification, Status) ->
    {Component, Ref} = component(Purl),
    {not_affected,
     #{ major => Major,
        id => Id,
        component => Component,
        ref => Ref,
        %% Which applications bundle the component, where openvex says so.
        apps => [App || A <- maps:get(<<"apps">>, Status, []),
                        {ok, App, _} <- [otp_purl(A)]],
        justification => Justification }}.

otp_purl(<<"pkg:otp/", Rest/binary>>) ->
    case string:split(Rest, "@") of
        [App, Vsn] -> {ok, App, Vsn};
        _ -> error
    end;
otp_purl(_) -> error.

%% pkg:github/madler/zlib@<sha> -> {<<"madler/zlib">>, <<"<sha>">>}. A couple of
%% the entries carry a doubled slash after the host, so leading ones are dropped.
component(Purl) ->
    Without = case Purl of
                  <<"pkg:github/", R/binary>> -> R;
                  <<"pkg:", R/binary>> -> R;
                  R -> R
              end,
    case string:split(Without, "@", trailing) of
        [Name, Ref] -> {string:trim(Name, leading, "/"), Ref};
        _ -> {string:trim(Without, leading, "/"), null}
    end.

ghsa_by_cve() ->
    case gh:get("/repos/erlang/otp/security-advisories") of
        {ok, Advisories} ->
            maps:from_list(
              [{Cve, #{ ghsa => maps:get(<<"ghsa_id">>, A, null),
                        severity => maps:get(<<"severity">>, A, null),
                        summary => maps:get(<<"summary">>, A, null),
                        url => maps:get(<<"html_url">>, A, null) }}
               || A <- Advisories,
                  Cve <- [maps:get(<<"cve_id">>, A, null)],
                  Cve =/= null]);
        {error, Reason} ->
            ?LOG_WARNING("Could not read security advisories (~p), advisories "
                         "will show without GHSA ids or severities.", [Reason]),
            #{}
    end.
