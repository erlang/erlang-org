#!/usr/bin/env escript

-mode(compile).
-include_lib("xmerl/include/xmerl.hrl").

main([File]) ->
    {ok, B} = file:read_file(File),
    Tags = [{"&nbsp;","\\&#160;"},
            {"&quot;","\\&#34;"},
            {"&ouml;","\\&#246;"},
            {"&auml;","\\&#228;"},
            {"&ldquo;","\\&#8220;"},
            {"&rdquo;","\\&#8221;"},
            {"&lsquo;","\\&#8216;"},
            {"&rsquo;","\\&#8217;"},
            {"&ndash;","\\&#8211;"},
            {"&copy;","\\&#169;"},
            {"&pound;","\\&#163;"},
            {" & "," \\&amp; "}
           ],
    {XML,_} = xmerl_scan:string(replace(Tags, B)),
    Articles = articles(XML),
    [case file:write_file(Filename,unicode:characters_to_binary(Content, utf8)) of
         ok -> ok;
         {error,_} ->
             io:format("~s: ~p~n",[Filename,unicode:characters_to_binary(Content, utf8)]),
             exit(0)
     end || {Filename,Content} <- Articles].

replace(Tags, Bin) ->
    lists:foldl(fun({What,With},B) ->
                        re:replace(B,What,With,[{return,list},global])
                end,Bin,Tags).

articles(#xmlElement{ name = articles, content = Content }) ->
    lists:flatmap(fun article/1, Content).

article(#xmlElement{ name = article, content = Content }) ->
    Props =
        lists:flatmap(
          fun(#xmlElement{ name = N, content = C }) ->
                  [{N,C}];
             (_) ->
                  []
          end,Content),
    Markdown = proplists:get_value(content, Props),
    Frontmatter = proplists:delete(content, Props),
    io:format("Parsing: ~p~n",[render(proplists:get_value(id,Frontmatter))]),
    [{render(proplists:get_value(id,Frontmatter)) ++ ".md",
      ["---\n",
       "layout: post\n",
       [case render(C) of
             "" -> [];
            Val when Name =:= id ->
                [yamlify(Name),": ",Val,"\n"];
            Val when Name =:= 'article-date' ->
                ["date: \"",hd(string:split(Val,"T")),"\"\n"];
            Val ->
                [yamlify(Name),": \"",string:replace(Val,"\"","\\\"",all),"\"\n"]
        end || {Name,C} <- Frontmatter],
       "---\n",
       html_to_markdown(Markdown)
      ]}];
article(_) ->
    [].

render([E|T]) ->
    [render(E)|render(T)];
render([]) ->
    [];
render(#xmlText{ value = V }) ->
    V.

yamlify(Name) ->
    replace([{"-","_"}],atom_to_list(Name)).

html_to_markdown([E|T]) ->
    [html_to_markdown(E) | html_to_markdown(T)];
html_to_markdown([]) ->
    [];
html_to_markdown(#xmlElement{ name = Ignore, content = Cs })
  when Ignore =:= font;
       Ignore =:= pre;
       Ignore =:= wbr ->
    html_to_markdown(Cs);
html_to_markdown(#xmlElement{ name = h1, content = Cs }) ->
    ["\n# ", string:trim(html_to_markdown(Cs)), "\n"];
html_to_markdown(#xmlElement{ name = h2, content = Cs }) ->
    ["\n## ", string:trim(html_to_markdown(Cs)), "\n"];
html_to_markdown(#xmlElement{ name = h3, content = Cs }) ->
    ["\n### ", string:trim(html_to_markdown(Cs)), "\n"];
html_to_markdown(#xmlElement{ name = h4, content = Cs }) ->
    ["\n#### ", string:trim(html_to_markdown(Cs)), "\n"];
html_to_markdown(#xmlElement{ name = hr, content = Cs }) ->
    ["****** ", string:trim(html_to_markdown(Cs)), "\n"];
html_to_markdown(#xmlElement{ name = p, content = Cs }) ->
    case lists:flatten(html_to_markdown(Cs)) of
        "" ->
            "";
        [160] -> %% Remove single nbsp
            "";
        MD ->
            ["\n", MD, "\n"]
    end;
html_to_markdown(#xmlElement{ name = ul, content = Cs }) ->
    [["* ", string:trim(html_to_markdown(C)), "\n"] || #xmlElement{ content = C } <- Cs];
html_to_markdown(#xmlElement{ name = strong, content = Cs }) ->
    ["**", html_to_markdown(Cs), "**"];
html_to_markdown(#xmlElement{ name = b, content = Cs }) ->
    ["**", html_to_markdown(Cs), "**"];
html_to_markdown(#xmlElement{ name = em, content = Cs }) ->
    ["*", html_to_markdown(Cs), "*"];
html_to_markdown(#xmlElement{ name = i, content = Cs }) ->
    ["*", html_to_markdown(Cs), "*"];
html_to_markdown(#xmlElement{ name = code, content = Cs }) ->
    ["`", html_to_markdown(Cs), "`"];
html_to_markdown(#xmlElement{ name = span, attributes = Attrs, content = Cs }) ->
    case lists:keyfind(style,#xmlAttribute.name,Attrs) of
        false ->
            html_to_markdown(Cs);
        Style ->
            case string:find(Style#xmlAttribute.value, "monospace") of
                nomatch -> html_to_markdown(Cs);
                _ -> html_to_markdown(#xmlElement{ name = code, content = Cs})
            end
    end;
html_to_markdown(#xmlElement{ name = 'div', attributes = Attrs, content = Cs }) ->
    case lists:keyfind(class,#xmlAttribute.name,Attrs) of
        false ->
            html_to_markdown(Cs);
        Class ->
            case string:find(Class#xmlAttribute.value, "-x-evo-paragraph") of
                nomatch -> html_to_markdown(Cs);
                _ ->
                    R = html_to_markdown(#xmlElement{ name = p, content = Cs}),
                    io:format("evo-par: ~p -> ~p~n",[Cs, R]),
                    R
            end
    end;
html_to_markdown(#xmlElement{ name = br, content = _Cs }) ->
    ["\n"];
html_to_markdown(#xmlElement{ name = a, attributes = Attrs, content = Cs }) ->
    Href = lists:keyfind(href,#xmlAttribute.name,Attrs),
    ["[", html_to_markdown(Cs), "](",Href#xmlAttribute.value,")"];
html_to_markdown(#xmlElement{ name = img, attributes = Attrs, content = Cs }) ->
    Src = lists:keyfind(src,#xmlAttribute.name,Attrs),
    ["![", html_to_markdown(Cs), "](",Src#xmlAttribute.value,")"];
%% html_to_markdown(#xmlText{ parents = [{content,_}|_], value = " " }) ->
%%     "";
html_to_markdown(#xmlText{ value = Val }) ->
    case string:trim(Val) of
        "" -> "";
        _ -> unicode:characters_to_list(Val, utf8)
    end.
