-module('create-bugs').
-export([main/1]).

main([BugsDir]) ->
    {ok,Issues} = gh:get("/repos/erlang/otp/issues?state=all"),
    lists:foreach(
      fun(#{ <<"title">> := Title, <<"html_url">> := Url }) ->
              case re:run(Title, "^ERL-[0-9]+",[{capture,all,list}]) of
                  {match,[BugsId]} ->
                      file:write_file(
                        filename:join(BugsDir, BugsId++".html"),
                        ["---\n"
                         "layout: redirect\n"
                         "html_url: \"",Url,"\"\n"
                         "---"]);
                  nomatch ->
                      ok
              end
      end, Issues).
