-module('create-releases').
-include_lib("kernel/include/logger.hrl").
-export([main/1]).

main([OTPVersionTable, ReleasesJson, PatchesDir]) ->
    [?LOG_WARNING("Running without github authentication, consider setting GITHUB_TOKEN in order for the API to not throttle you.") || os:getenv("GITHUB_TOKEN") =:= false],
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    Versions = parse_otp_versions_table(OTPVersionTable),
    Downloads = parse_erlang_org_downloads(),
    Tags = parse_github_tags(),
    {ok, GHReleases} = gh:get("/repos/erlang/otp/releases"),
    Releases = maps:map(fun(Key,Val) -> process_patches(Key,Val,Downloads,Tags,GHReleases) end, Versions),
    ok = file:write_file(ReleasesJson, create_release_json(Releases)),
    create_patches(PatchesDir, Releases),
    ok.

parse_otp_versions_table(OTPVersionTable) ->
    {ok, Versions} = file:read_file(OTPVersionTable),
    parse_otp_versions_table(string:split(Versions, "\n", all), #{}).
parse_otp_versions_table([<<>>|T], MajorVsns) ->
    parse_otp_versions_table(T, MajorVsns);
parse_otp_versions_table([Line|T], MajorVsns) ->
    {match,[Vsn,Major]} = re:run(Line,"OTP-(([0-9]+)[^\\s]+)\\s:",
                                 [{capture,all_but_first,binary}]),
    Vsns = maps:get(Major, MajorVsns, []),
    parse_otp_versions_table(T, MajorVsns#{ Major => Vsns ++ [Vsn] });
parse_otp_versions_table([], MajorVsns) ->
    MajorVsns.

parse_erlang_org_downloads() ->
    {match,Downloads} = re:run(download_erlang_org_downloads(),
                               <<"<a href=\"([^\"/]+)\"">>,
                               [global,{capture,all_but_first,binary}]),
    Matches = #{ readme => "^(?:otp_src_|OTP-)(.*)\\.(?:readme|README)$",
                 erlang_download_readme => "^(?:otp_src_|OTP-)(.*)\\.(?:readme|README)$",
                 html => "^otp_(?:doc_)?html_(.*)\\.tar\\.gz$",
                 man => "^otp_(?:doc_)?man_(.*)\\.tar\\.gz$",
                 win32 => "^otp_win32_(.*)\\.exe$",
                 win64 => "^otp_win64_(.*)\\.exe$",
                 src => "^otp_src_(.*)\\.tar\\.gz$" },
    lists:foldl(
      fun(Download, Vsns) ->
              case maps:fold(
                     fun(Key, Match, Acc) ->
                             case re:run(Download, Match, [{capture,all_but_first,binary}]) of
                                 nomatch ->
                                     Acc;
                                 {match,[Vsn]} ->
                                     [{Vsn, Key}|Acc]
                             end
                     end, [], Matches) of
                  Ms ->
                    lists:foldl(
                        fun({Vsn, Key}, Map) ->
                            Info = maps:get(Vsn, Map, #{}),
                            Map#{ Vsn => Info#{ Key => iolist_to_binary(["https://erlang.org/download/",Download]) } }
                        end, Vsns, Ms)
              end
      end, #{}, Downloads).

download_erlang_org_downloads() ->
    {ok,{{_,200,_},Hdrs,Body}} = httpc:request(
        get,{"https://erlang.org/download",[]},
        ssl_opts("https://erlang.org/download"),[]),
    case lists:member({"content-encoding","gzip"}, Hdrs) of
        true ->
            zlib:gunzip(Body);
        false ->
            Body
    end.

parse_github_tags() ->
    {ok, Json} = gh:get("/repos/erlang/otp/tags"),
    maps:from_list(
      [begin
           TagName = maps:get(<<"name">>,Tag),
           case re:run(TagName,"OTP[-_](.*)",[{capture,all_but_first,binary}]) of
               {match,[Vsn]} ->
                   {Vsn, {TagName, maps:get(<<"tarball_url">>,Tag)}};
               nomatch ->
                   {TagName, {TagName, maps:get(<<"tarball_url">>,Tag)}}
           end
       end || Tag <- Json]).

process_patches(Major, Patches, Downloads, Tags, Releases) ->
    NewPatches = pmap(fun(Patch) ->  process_patch(Patch, Releases, Downloads, Tags) end, Patches),
    %% Filter out if we did not get a readme. We are in the middle of doing a patch
    %% and thus we do not want to show the patch for now.
    CompletePatches = lists:filter(fun(Patch) -> maps:is_key(readme, Patch) end, NewPatches),
    #{ patches => CompletePatches,
       latest => hd(CompletePatches),
       release => Major }.

process_patch(PatchVsn, Releases, Downloads, Tags) ->
    ErlangOrgDownload = maps:get(PatchVsn, Downloads, #{}),
    case lists:search(
           fun(Release) ->
                   string:equal(maps:get(<<"tag_name">>, Release),"OTP-"++ PatchVsn)
           end, Releases) of
        {value, Json} ->
            Assets = fetch_assets(
                       maps:get(<<"assets">>, Json)),
            Patch = #{ name => PatchVsn,
                       tag_name => maps:get(<<"tag_name">>, Json),
                       published_at => maps:get(<<"published_at">>, Json),
                       html_url => maps:get(<<"html_url">>, Json)},
            maps:merge(ErlangOrgDownload,maps:merge(Patch, Assets));
        false ->
            {TagName, Src} = maps:get(PatchVsn, Tags, {undefined, undefined}),
            maps:merge(#{ tag_name => TagName, src => Src, name => PatchVsn },
                       ErlangOrgDownload)
    end.

fetch_assets(Assets) ->
    Matches = #{ readme => "^OTP-.*\\.README$",
                 html => "^otp_doc_html.*",
                 man => "^otp_doc_man.*",
                 win32 => "^otp_win32.*",
                 win64 => "^otp_win64.*",
                 src => "^otp_src.*" },
    maps:from_list(
      lists:flatmap(
        fun({Key, Match}) ->
                case lists:search(
                     fun(Asset) ->
                             re:run(maps:get(<<"name">>,Asset),Match) =/= nomatch
                     end, Assets) of
                    {value,V} ->
                        [{Key,#{ url => maps:get(<<"browser_download_url">>,V),
                                 id => maps:get(<<"id">>,V) }}];
                    false ->
                        []
                end
        end, maps:to_list(Matches))).

create_release_json(Releases) ->
    jsone:encode(
      lists:map(
        fun({_Key, Release}) ->
                Release#{ latest => strip_ids(maps:get(latest,Release)),
                          patches => [strip_ids(Patch) || Patch <- maps:get(patches,Release)] }
        end, maps:to_list(Releases)),
     [native_forward_slash,{indent,2}]).

strip_ids(Patch) ->
    maps:map(fun(_Key, #{ id := _, url := Url }) ->
                     Url;
                (_,Value) ->
                     Value
             end, Patch).

create_patches(Dir, Releases) ->
    TmpDir = string:trim(os:cmd("mktemp -d")),
    os:cmd("rsync --archive --verbose --compress --include='*.readme' --include='*.README' --exclude='*' erlang.org::erlang-download "++TmpDir ++ "/"),
    maps:map(
      fun(Release, #{ patches := Patches }) ->
              pmap(
                fun(#{ erlang_download_readme := Url } = Patch) ->
                        Name = lists:last(string:split(Url, "/", all)),
                        {ok, Readme } = file:read_file(filename:join(TmpDir, Name)),
                        create_patch(Dir, strip_ids(Patch#{ release => Release }), Readme)
                end, Patches)
      end, Releases),
    file:del_dir_r(TmpDir).

create_patch(Dir, Patch, ReadmeStr) ->
    FrontMatter = lists:map(
                    fun({Key,Value}) ->
                            io_lib:format("~p: ~s\n",[Key,Value])
                    end, maps:to_list(Patch)),
    Readme = otp_readme:parse(ReadmeStr),
    ok = file:write_file(
           filename:join(Dir,iolist_to_binary([maps:get(tag_name,Patch),".html"])),
           ["---\n"
            "layout: patch\n",
            FrontMatter,
            otp_readme:render_yaml(Readme),
            "---\n",
            otp_readme:render(Readme)]).



pmap(Fun, List) ->
    Refs =
        [begin
             {_, Ref} = spawn_monitor(fun() -> exit(Fun(A)) end),
             Ref
         end || A <- List],
    [receive
         {'DOWN',Ref, _ , _, Value} ->
             Value
     end || Ref <- Refs].

    ssl_opts(Url) ->
        #{ host := Hostname } = uri_string:parse(Url),
        VerifyFun = {fun ssl_verify_hostname:verify_fun/3,
                      [{check_hostname, Hostname}]},
        %% CACerts = certifi:cacerts(),
        [{ssl,[{verify, verify_peer},
               {cacertfile, "/etc/ssl/certs/ca-certificates.crt"},
               {verify_fun, VerifyFun},
               {customize_hostname_check,
                [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
              ]}].