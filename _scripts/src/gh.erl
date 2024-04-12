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
                    {ok, jsone:decode(Body)};
                {"link",Link} ->
                    %% If there is a link header, the results are paginated, so
                    %% we follow the pages until the end.
                    case re:run(Link,"<([^>]+)>; rel=\"next\"",[{capture,all_but_first,binary}]) of
                        nomatch ->
                            {ok, jsone:decode(Body)};
                        {match,[NextLink]} ->
                            {ok, NextJson} = get(NextLink),
                            {ok, jsone:decode(Body) ++ NextJson}
                    end
            end;
        {ok,{{_,200,_},_Hdrs,Body}} ->
            {ok, Body};
        Else ->
            {error, Else}
    end.


ssl_opts(Url) ->
    #{ host := Hostname } = uri_string:parse(Url),
    VerifyFun = {fun ssl_verify_hostname:verify_fun/3,
                 [{check_hostname, Hostname}]},
    CACerts = public_key:cacerts_get(),
    [{ssl,[{verify, verify_peer},
           {cacerts, CACerts},
           {verify_fun, VerifyFun},
           {customize_hostname_check,
            [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}
          ]}].

%% This code is gratefully copied from rebar3_util.erl
%% It is currently not used as we try to use the OSs bundled certificates instead
%% but I leave it down here in case it is needed in the future.
-ifdef(USE_CERTIFI).
ssl_opts_certify(Url) ->
    #{ host := Hostname } = uri_string:parse(Url),
    VerifyFun = {fun ssl_verify_hostname:verify_fun/3,
                 [{check_hostname, Hostname}]},
    CACerts = certifi:cacerts(),
    [{ssl,[{verify, verify_peer},
           {cacerts, CACerts},
           {partial_chain, fun partial_chain/1},
           {verify_fun, VerifyFun},
           {customize_hostname_check,
            [{match_fun, public_key:pkix_verify_hostname_match_fun(https)}]}]
     }].
partial_chain(Certs) ->
    Certs1 = [{Cert, public_key:pkix_decode_cert(Cert, otp)} || Cert <- Certs],
    CACerts = certifi:cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],
    case lists:search(fun({_, Cert}) ->
                              check_cert(CACerts1, Cert)
                      end, Certs1) of
        {value, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        false ->
            unknown_ca
    end.

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

check_cert(CACerts, Cert) ->
    lists:any(fun(CACert) ->
                      extract_public_key_info(CACert) == extract_public_key_info(Cert)
              end, CACerts).
-endif.
