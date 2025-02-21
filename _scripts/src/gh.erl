-module(gh).
-include_lib("public_key/include/OTP-PUB-KEY.hrl").
-export([get/1]).
-compile({no_auto_import,[get/1]}).

get(Url) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(ssl),
    get(Url,[]).
get(Url, GetHdrs) ->
    Auth = case os:getenv("GITHUB_TOKEN") of
               false -> [];
               Token ->
                   [{"Authorization","token " ++ Token}]
           end,
    ParsedURL = uri_string:parse(Url),
    OrigQS = uri_string:dissect_query(maps:get(query,ParsedURL,[])),
    QS = case lists:keymember("per_page",1,OrigQS) of
             true -> OrigQS;
             false -> lists:usort([{"per_page","100"} | OrigQS])
         end,
    FullUrl = uri_string:recompose(
                ParsedURL#{ host => "api.github.com",
                            scheme => "https",
                            query => uri_string:compose_query(QS)
                          }),

    Accept = proplists:get_value("Accept", GetHdrs, "application/vnd.github.v3+json"),
    case httpc:request(
           get,
           {FullUrl,
            [{"Accept", Accept},
             {"User-Agent","erlang-httpc"} | Auth]},ssl_opts(FullUrl),[{body_format,binary}]) of
        {ok,{{_,200,_},Hdrs,Body}} when Accept =:= "application/vnd.github.v3+json" ->
            case lists:keyfind("link",1,Hdrs) of
                false ->
                    {ok, json:decode(Body)};
                {"link",Link} ->
                    %% If there is a link header, the results are paginated, so
                    %% we follow the pages until the end.
                    case re:run(Link,"<([^>]+)>; rel=\"next\"",[{capture,all_but_first,binary}]) of
                        nomatch ->
                            {ok, json:decode(Body)};
                        {match,[NextLink]} ->
                            {ok, NextJson} = get(NextLink),
                            {ok, json:decode(Body) ++ NextJson}
                    end
            end;
        {ok,{{_,200,_},_Hdrs,Body}} ->
            {ok, Body};
        Else ->
            {error, Else}
    end.

ssl_opts(_Url) ->
    [{ssl, httpc:ssl_verify_host_options(true)}].
